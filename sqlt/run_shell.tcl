#!/usr/bin/env tclsh
# kate: syntax Tcl/Tk;
#
# run_shell.tcl — shell-level tests of the `sqlsh` and `sqrsh` REPLs.
#
# run_sqlt.tcl covers SQL through sqlsh; the Fortran unit tests cover the
# library. This covers what sits between them and is reachable from neither:
# the meta-commands (".tables", ...) and how the two shells treat the encoding
# of their input stream (UTF-8 byte-order mark ignored; UTF-16, UTF-32 and
# the other recognisable encodings refused by name, with exit status 1),
# and the command line: an argument that starts with '-' is an option, never
# a database directory, so a mistyped one must not create a directory.
# sqlsh and sqrsh each read their own input, so the same cases are run
# through both to show that they give the same answer.
#
# Each case writes a script as raw bytes, runs `<shell> < script` and checks
# the exit status, stdout and stderr.
#
# Usage:   tclsh run_shell.tcl <sqlsh-binary> <sqrsh-binary>
# Exit:    0 if every check passed, 1 otherwise.

namespace eval shelltest {
    variable sqlsh  ""
    variable sqrsh  ""
    variable work   ""
    variable npass  0
    variable nfail  0

    # Byte-order marks, as the bytes that open a file.
    variable BOM_UTF8    [binary format H* EFBBBF]
    variable BOM_UTF16LE [binary format H* FFFE]
    variable BOM_UTF16BE [binary format H* FEFF]
    variable BOM_UTF32LE [binary format H* FFFE0000]
    variable BOM_UTF32BE [binary format H* 0000FEFF]
    variable SIG_GB18030 [binary format H* 84319533]

    # Record one check. `cond` is an expression evaluated in the caller's scope.
    proc check {cond label} {
        variable npass
        variable nfail
        if {[uplevel 1 [list expr $cond]]} {
            incr npass
            puts "  OK   $label"
        } else {
            incr nfail
            puts "  FAIL $label"
        }
    }

    # Run a shell (default sqlsh) with `bytes` (a byte string) as its standard
    # input. Returns a dict: status (exit code), out (stdout), err (stderr).
    proc run {bytes {shell ""}} {
        variable sqlsh
        variable work
        if {$shell eq ""} { set shell $sqlsh }
        set script [file join $work script.sql]
        set outf   [file join $work out.txt]
        set errf   [file join $work err.txt]
        set fh [open $script wb]
        puts -nonewline $fh $bytes
        close $fh
        set status 0
        if {[catch {exec $shell < $script > $outf 2> $errf} msg opts]} {
            set code [dict get $opts -errorcode]
            if {[lindex $code 0] eq "CHILDSTATUS"} {
                set status [lindex $code 2]
            } else {
                set status -1                ;# killed by a signal, or not started
            }
        }
        set result [dict create status $status]
        foreach {key path} [list out $outf err $errf] {
            set fh [open $path r]
            fconfigure $fh -encoding utf-8
            dict set result $key [read $fh]
            close $fh
        }
        return $result
    }

    # Run `shell` with command-line arguments `args` and an empty standard
    # input, from inside the work area (so a directory created by mistake
    # lands there and can be looked for). Returns the same dict as `run`.
    proc run_args {shell args} {
        variable work
        set outf [file join $work out.txt]
        set errf [file join $work err.txt]
        set here [pwd]
        set status 0
        cd $work
        try {
            exec $shell {*}$args < /dev/null > $outf 2> $errf
        } trap CHILDSTATUS {msg opts} {
            set status [lindex [dict get $opts -errorcode] 2]
        } on error {} {
            set status -1                    ;# killed by a signal, or not started
        } finally {
            cd $here
        }
        set result [dict create status $status]
        foreach {key path} [list out $outf err $errf] {
            set fh [open $path r]
            fconfigure $fh -encoding utf-8
            dict set result $key [read $fh]
            close $fh
        }
        return $result
    }

    # The release in fpm.toml, which `sqlsh --version` must agree with.
    proc fpm_version {} {
        set fh [open [file join [file dirname [file dirname [file normalize [info script]]]] fpm.toml] r]
        set text [read $fh]
        close $fh
        if {![regexp -line {^version *= *"([^"]+)"} $text -> ver]} { return "" }
        return $ver
    }

    # SQL text as the UTF-8 bytes sqlsh expects.
    proc utf8 {text} { return [encoding convertto utf-8 $text] }

    # ASCII text as UTF-16 code units, little- or big-endian. Built by hand
    # (not `encoding convertto`) because the encoding's name differs between
    # Tcl 8.6 and Tcl 9.
    proc utf16 {text bigEndian} {
        set codes {}
        foreach ch [split $text ""] { lappend codes [scan $ch %c] }
        return [binary format [expr {$bigEndian ? "S*" : "s*"}] $codes]
    }

    # The same as 32-bit code units.
    proc utf32 {text bigEndian} {
        set codes {}
        foreach ch [split $text ""] { lappend codes [scan $ch %c] }
        return [binary format [expr {$bigEndian ? "I*" : "i*"}] $codes]
    }

    # A fresh, empty database directory name inside the work area.
    proc fresh_db {name} {
        variable work
        set dir [file join $work $name]
        file delete -force $dir
        return $dir
    }

    # ---- cases ----

    proc t_tables {} {
        set db [fresh_db tables]
        set r [run [utf8 ".open $db\nCREATE TABLE t(a INTEGER, b CHAR(4))\nINSERT INTO t VALUES(1,'x')\nINSERT INTO t VALUES(2,'y')\n.tables\n.quit\n"]]
        check {[dict get $r status] == 0}                              "tables: exit status 0"
        check {[regexp -line {^t  \(2 rows, 2 cols\)$} [dict get $r out]]} "tables: lists the table with its row and column counts"
    }

    # A statement that fails part-way inside BEGIN dooms the transaction: the
    # shell says so, refuses the next write, and COMMIT rolls back.
    proc t_txn_fail {} {
        set db [fresh_db txnfail]
        set sql ".open $db\nCREATE TABLE t(a INTEGER)\nCREATE UNIQUE INDEX ON t (a)\nINSERT INTO t VALUES(10)\n"
        append sql "BEGIN\nINSERT INTO t VALUES(1), (2), (1)\nINSERT INTO t VALUES(3)\nCOMMIT\n"
        append sql "SELECT COUNT(*) FROM t\n"
        append sql "BEGIN\nINSERT INTO t VALUES(10), (11)\nINSERT INTO t VALUES(12)\nCOMMIT\n"
        append sql "SELECT a FROM t ORDER BY a\n.quit\n"
        set r [run [utf8 $sql]]
        set err [dict get $r err]
        set out [dict get $r out]
        check {[dict get $r status] == 1}                     "txn fail: exit status 1 (a statement failed)"
        check {[regexp {ROLLBACK is required} $err]}          "txn fail: the next write is refused, with the remedy"
        check {[regexp {not committed.*rolled back} $err]}    "txn fail: COMMIT reports the rollback"
        check {[regexp -line {^1 *$} $out]}                   "txn fail: no row of the failed statement was kept"
        check {[regexp {committed} $out]}                     "txn fail: a failure before the first write leaves the transaction usable"
        check {[regexp {10\s+12\s} $out] && ![regexp -line {^(2|3|11) *$} $out]} "txn fail: final rows are 10 and 12"
    }

    # LIKE on a value near the widest CHAR column: the answer comes back and
    # the shell is still running afterwards (it once ran out of stack).
    proc t_like_long {} {
        set db [fresh_db likelong]
        set sql ".open $db\nCREATE TABLE w(id INTEGER, c CHAR(65000))\n"
        append sql "INSERT INTO w VALUES(11,'[string repeat a 60000]')\n"
        append sql "INSERT INTO w VALUES(22,'[string repeat a 59999]b')\n"
        append sql "SELECT id FROM w WHERE c LIKE '%a'\n"
        append sql "SELECT id FROM w WHERE c LIKE '%a%a%a%a%a%b'\n"
        append sql "SELECT COUNT(*) FROM w\n.quit\n"
        set r [run [utf8 $sql]]
        set out [dict get $r out]
        check {[dict get $r status] == 0}                     "like long: exit status 0"
        check {[regexp -all -line {^11 *$} $out] == 1}        "like long: % then a literal finds one row, once"
        check {[regexp -all -line {^22 *$} $out] == 1}        "like long: several % find the other row, once"
        check {[regexp {(?w)^11 *$.*^22 *$.*^2 *$} $out]}     "like long: in that order, and the shell answers the next statement"
    }

    # ALTER TABLE ... ADD COLUMN giving a table its first TEXT column: the
    # column is usable at once, and by a second sqlsh run on the same database.
    proc t_add_text_column {} {
        set db [fresh_db addtext]
        set sql ".open $db\nCREATE TABLE n(id INTEGER, tag CHAR(8))\n"
        append sql "INSERT INTO n VALUES(1,'one')\nINSERT INTO n VALUES(2,'two')\n"
        append sql "ALTER TABLE n ADD COLUMN body TEXT\n"
        append sql "UPDATE n SET body = 'the long story' WHERE id = 2\n"
        append sql "SELECT body FROM n WHERE id = 2\n.quit\n"
        set r [run [utf8 $sql]]
        check {[dict get $r status] == 0}                                  "add text column: exit status 0"
        check {[dict get $r err] eq ""}                                    "add text column: nothing on stderr"
        check {[regexp -line {^the long story *$} [dict get $r out]]}      "add text column: the new column holds a value"
        check {[file exists [file join $db n.blob]] && ![file exists [file join $db n.blob.alter]]} \
                                                                           "add text column: its blob file is in place"
        set r [run [utf8 ".open $db\nSELECT tag FROM n WHERE body IS NOT NULL\nSELECT body FROM n WHERE id = 2\n.quit\n"]]
        check {![regexp {error|cannot open} [dict get $r err]]}            "add text column: the database opens on the next start"
        check {[regexp -line {^the long story *$} [dict get $r out]]}      "add text column: and the value is still there"
        check {[regexp -all -line {^(one|two) *$} [dict get $r out]] == 1 && [regexp -line {^two *$} [dict get $r out]]} \
                                                                           "add text column: on the one row that was given it"
    }

    # A table name beginning with '_' is the engine's: CREATE TABLE _journal
    # is refused, and the database is still there on the next start.
    proc t_reserved_name {} {
        set db [fresh_db reserved]
        set r [run [utf8 ".open $db\nCREATE TABLE t(a INTEGER)\nINSERT INTO t VALUES(1)\nCREATE TABLE _journal(a INTEGER)\nCREATE TABLE _catalog(a INTEGER)\n.quit\n"]]
        check {[dict get $r status] == 1}                                   "reserved name: exit status 1 (a statement failed)"
        check {[regexp {reserved table name: "_journal"} [dict get $r err]]} "reserved name: _journal refused, by name"
        check {[regexp {reserved table name: "_catalog"} [dict get $r err]]} "reserved name: _catalog refused, by name"
        set r [run [utf8 ".open $db\nSELECT a FROM t\n.tables\n.quit\n"]]
        check {![regexp {error|cannot open} [dict get $r err]]}             "reserved name: the database opens on the next start"
        check {[regexp -line {^t  \(1 rows, 1 cols\)$} [dict get $r out]]}   "reserved name: with its one table intact"
    }

    # A new table's name is held to a plain character set, and may not differ
    # only in case from an existing table's.
    proc t_table_names {} {
        set db [fresh_db names]
        set sql ".open $db\nCREATE TABLE Notes(a INTEGER)\nCREATE TABLE \"my notes, v1.2\"(a INTEGER)\n"
        append sql "CREATE TABLE \"a:b\"(a INTEGER)\nCREATE TABLE NOTES(a INTEGER)\n"
        append sql "CREATE TABLE \"café\"(a INTEGER)\nINSERT INTO \"café\" VALUES(1)\nCREATE TABLE café2(a INTEGER)\n"
        append sql ".tables\n.quit\n"
        set r [run [utf8 $sql]]
        set err [dict get $r err]
        set out [dict get $r out]
        check {[dict get $r status] == 1}                                 "table names: exit status 1 (a statement failed)"
        check {[regexp {invalid table name: "a:b"} $err]}                 "table names: a colon is refused, by name"
        check {[regexp {differs only in case from "Notes"} $err]}         "table names: a case variant is refused, naming the other"
        check {[regexp -line {^my notes, v1\.2  \(0 rows} $out]}           "table names: space, comma and dot are accepted in a quoted name"
        check {[regexp -line {^café  \(1 rows} $out]}                      "table names: a non-ASCII name is accepted when quoted, and usable"
        check {![regexp {café2} $out]}                                    "table names: a bare non-ASCII word is not a name in SQL"
        check {[llength [regexp -all -inline -line {rows, 1 cols\)$} $out]] == 3} "table names: three tables exist"
    }

    # Review finding E2: `readonly` in sqrsh, then `close`, must not lose the
    # row counters.  sqlsh builds the table; sqrsh deletes, demotes and closes.
    proc t_readonly_counters {} {
        variable sqrsh
        set db [fresh_db rocount]
        run [utf8 ".open $db\nCREATE TABLE t(a INTEGER)\nINSERT INTO t VALUES(1)\nINSERT INTO t VALUES(2)\nINSERT INTO t VALUES(3)\n.quit\n"]
        set cmds "open $db\nuse t\ndelete 2\nback\nreadonly\nclose\nopen $db\ntables\nuse t\nverify\nquit\n"
        set r [run [utf8 $cmds] $sqrsh]
        set out [dict get $r out]
        check {[dict get $r status] == 0}              "readonly counters: exit status 0"
        check {[regexp {now read-only} $out]}          "readonly counters: the handle was demoted"
        check {[regexp {\(2 rows, 1 cols\)} $out]}     "readonly counters: the delete is counted after reopen"
        check {[regexp {verify: OK} $out]}             "readonly counters: verify is clean"
    }

    # Review finding A1: a database too large to pack is refused with a
    # message, not packed short.  The 5 GiB data file is sparse.
    proc t_pack_too_large {} {
        variable work
        set db [fresh_db packbig]
        set out [file join $work packbig.sqr]
        run [utf8 ".open $db\nCREATE TABLE t(a INTEGER)\n.quit\n"]
        set fh [open [file join $db t.dat] r+b]
        seek $fh [expr {5 * 1024 * 1024 * 1024 - 1}]
        puts -nonewline $fh \0
        close $fh
        set r [run [utf8 ".pack $db $out\n.quit\n"]]
        check {[regexp {failed: cannot pack: "t\.dat" is larger than 2 GiB} [dict get $r err]]} \
            "pack too large: refused, naming the file"
        check {![file exists $out]} "pack too large: no container written"
    }

    # .pack then .unpack: the restored directory is a database a later sqlsh
    # run can open and query, and no temporary directory is left beside it.
    proc t_pack_unpack {} {
        variable work
        set db  [fresh_db packsrc]
        set dst [fresh_db packdst]
        set pk  [file join $work packsrc.sqr]
        file delete -force $pk $dst.unpack-tmp
        set sql ".open $db\nCREATE TABLE t(a INTEGER, b CHAR(8), c TEXT)\n"
        append sql "INSERT INTO t VALUES(1,'one','first body')\nINSERT INTO t VALUES(2,'two','second body')\n"
        append sql "CREATE INDEX ON t(a)\n.quit\n"
        run [utf8 $sql]
        set r [run [utf8 ".pack $db $pk\n.unpack $pk $dst\n.quit\n"]]
        check {[dict get $r status] == 0 && ![regexp {failed|error} [dict get $r err]]} \
                                                                       "pack/unpack: both commands succeed"
        check {[file isdirectory $dst] && ![file exists $dst.unpack-tmp]} \
                                                                       "pack/unpack: the directory is in place, no temporary left"
        set r [run [utf8 ".open $dst\nSELECT b FROM t WHERE a = 2\nSELECT c FROM t WHERE a = 1\n.quit\n"]]
        check {![regexp {error|cannot open} [dict get $r err]]}        "pack/unpack: the restored database opens on the next start"
        check {[regexp -line {^two *$} [dict get $r out]]}             "pack/unpack: an indexed lookup answers"
        check {[regexp -line {^first body *$} [dict get $r out]]}      "pack/unpack: TEXT values came across"
        set r [run [utf8 ".unpack $pk $dst\n.quit\n"]]
        check {[regexp {failed} [dict get $r err]] && ![file exists $dst.unpack-tmp]} \
                                                                       "pack/unpack: a second unpack onto it is refused, leaving no temporary"
        set dst2 [fresh_db packdst2]
        file delete -force $dst2.unpack-tmp
        set r [run [utf8 ".unpack $pk $dst2/\n.quit\n"]]
        check {![regexp {failed|error} [dict get $r err]] && [file isdirectory $dst2]
               && ![file exists $dst2.unpack-tmp] && ![file exists $dst2/.unpack-tmp]} \
                                                                       "pack/unpack: a target with a trailing separator unpacks, leaving no temporary"
    }

    # Transaction statements need an open database (sqlsh started with no
    # directory), and a failure carries the engine's reason.
    proc t_sql_reasons {} {
        set r [run [utf8 "BEGIN\nCOMMIT\nROLLBACK\nSELECT 1\n.quit\n"]]
        check {[regexp -all -line {^error: no database open$} [dict get $r err]] == 3} \
            "no database: BEGIN, COMMIT and ROLLBACK are refused"
        check {[regexp -line {^1 *$} [dict get $r out]]} "no database: a SELECT of a literal still runs"
        set db [fresh_db reasons]
        set sql ".open $db\nCREATE TABLE t(a INTEGER)\nCREATE UNIQUE INDEX ON t (a)\nINSERT INTO t VALUES(1)\n"
        append sql "INSERT INTO t VALUES(1)\nUPDATE t SET a = 2, a = 3\n.quit\n"
        set r [run [utf8 $sql]]
        check {[regexp -line {^error: insert failed: unique-key violation on index over "a"} [dict get $r err]]} \
            "engine reason: a unique violation is named"
        check {[regexp -line {^error: duplicate column: a$} [dict get $r err]]} \
            "engine reason: a column twice in SET is refused"
    }

    # R18: columns are padded by characters, so the column after one holding
    # a non-ASCII value starts at the same character on every line, and the
    # last column is not padded, so no line ends in a blank.
    proc t_display {} {
        set db [fresh_db display]
        set sql ".open $db\nCREATE TABLE t(id INTEGER, name CHAR(8), v REAL)\n"
        append sql "INSERT INTO t VALUES(1,'é',1.5),(2,'àé',-0.0),(3,'abc',1e-300)\n"
        append sql "SELECT name, v FROM t ORDER BY id\n.quit\n"
        set r [run [utf8 $sql]]
        set lines [lrange [split [dict get $r out] "\n"] 2 6]   ;# header, rule, 3 rows
        set starts {}
        set trailing 0
        foreach ln $lines {
            if {[regexp {[ \t]$} $ln]} { set trailing 1 }
            regexp -indices {^\S+ +} $ln m
            lappend starts [expr {[lindex $m 1] + 1}]
        }
        check {[llength $lines] == 5 && [llength [lsort -unique $starts]] == 1} \
            "display: the column after non-ASCII values starts at one character position"
        check {!$trailing && ![regexp -line {[ \t]$} [dict get $r out]]} \
            "display: no output line ends in a blank"
        check {[regexp -line {^àé +0\.00000000E\+00$} [dict get $r out]]} \
            "display: -0.0 is shown unsigned"
        check {[regexp -line {^abc +1\.00000000E-300$} [dict get $r out]]} \
            "display: a three-digit exponent keeps its E"
    }

    # R17: .schema prints statements that sqlsh reads back to the same schema.
    proc t_schema {} {
        set db1 [fresh_db schema1]
        set db2 [fresh_db schema2]
        set sql ".open $db1\nCREATE TABLE t(id INTEGER, \"na\"\"me\" CHAR(8), v REAL, b TEXT)\n"
        append sql "CREATE UNIQUE INDEX ON t (id)\nCREATE INDEX ON t (\"na\"\"me\", id)\n"
        append sql "CREATE INDEX ON t (v)\nDROP INDEX ON t (v)\n"
        append sql "CREATE TABLE \"x y\"(a INT)\n.schema\n.quit\n"
        set r1 [run [utf8 $sql]]
        set schema [dict get $r1 out]
        set i [string first "CREATE TABLE" $schema]
        set schema [string range $schema $i end]
        check {[regexp -line {^CREATE TABLE "t" \("id" INTEGER, "na""me" CHAR\(8\), "v" REAL, "b" TEXT\);$} $schema]} \
            "schema: CREATE TABLE with quoted names and types"
        check {[regexp -line {^CREATE UNIQUE INDEX ON "t" \("id"\);$} $schema]
               && [regexp -line {^CREATE INDEX ON "t" \("na""me", "id"\);$} $schema]
               && ![regexp {"v"\);} $schema]} \
            "schema: live indexes in slot order, dropped one left out"
        set r2 [run [utf8 ".open $db2\n$schema.schema\n.quit\n"]]
        set again [dict get $r2 out]
        set again [string range $again [string last "CREATE TABLE \"t\"" $again] end]
        set first [string range $schema [string first "CREATE TABLE \"t\"" $schema] end]
        check {[dict get $r2 status] == 0 && $again eq $first} \
            "schema: output read back gives the same schema"
    }

    # R13: sqlsh ends with status 1 when a statement or command failed.
    proc t_exit_status {} {
        set db [fresh_db status]
        set r [run [utf8 ".open $db\nCREATE TABLE t(a INTEGER)\nSELECT a FROM t\n.quit\n"]]
        check {[dict get $r status] == 0} "status: a clean script ends with 0"
        set r [run [utf8 ".open $db\nSELECT nope FROM t\nSELECT a FROM t\n"]]
        check {[dict get $r status] == 1 && [regexp -line {^\(0 row\(s\)\)$} [dict get $r out]]} \
            "status: a failing statement ends with 1, and the script runs on"
        set r [run [utf8 ".open $db\n.nonsense\n.quit\n"]]
        check {[dict get $r status] == 1} "status: a failing meta-command ends with 1"
        set r [run [utf8 ".tables\n"]]
        check {[dict get $r status] == 1} "status: no database open ends with 1"
    }

    # A close writes the row counters to the table's schema, through a
    # temporary file beside it.  A directory of that name makes the write
    # fail: sqlsh says so and ends with 1, at .close and at the end of input.
    proc t_close_failure {} {
        set db [fresh_db closefail]
        run [utf8 ".open $db\nCREATE TABLE t(a INTEGER)\n.quit\n"]
        file mkdir [file join $db t.schema.tmp]
        set r [run [utf8 ".open $db\nINSERT INTO t VALUES(1)\n"]]
        check {[dict get $r status] == 1 &&
               [regexp -line {^error: cannot close the database: } [dict get $r err]]} \
            "close: a failed close at the end of input is reported, status 1"
        set r [run [utf8 ".open $db\nINSERT INTO t VALUES(2)\n.close\n.quit\n"]]
        check {[dict get $r status] == 1 &&
               [regexp -line {^error: cannot close the database: } [dict get $r err]]} \
            "close: a failed .close is reported, status 1"
        file delete [file join $db t.schema.tmp]
        set r [run [utf8 ".open $db\nSELECT a FROM t ORDER BY a\n.quit\n"]]
        check {[dict get $r status] == 0 && [regexp {1\s+2\s} [dict get $r out]]} \
            "close: with the obstacle gone the rows are there and the close is clean"
    }

    # P1: standard input that cannot be read (a directory) ends the shell
    # at once with status 1 and a message, rather than reading again for
    # ever.  `timeout` turns a spin into status 124, a failed check.  A
    # gfortran runtime reports a directory as end of file, not as an error:
    # that build ends at once with status 0 and nothing on stderr, also a
    # pass (LOWS-DESIGN 6.3.1).  Any other status fails.
    proc t_unreadable_stdin {} {
        variable sqlsh
        variable sqrsh
        variable work
        set errf [file join $work err.txt]
        # Not a case a Windows program can be put in (a directory cannot be
        # opened as a file there), and not one the wine wrapper can pass on.
        if {[info exists ::env(SQR_TEST_WINDOWS)]} {
            puts "  SKIP unreadable standard input (a directory as stdin is POSIX only)"
            return
        }
        foreach {shell name msg} [list $sqlsh sqlsh {^error: cannot read standard input$} \
                                       $sqrsh sqrsh {^cannot read standard input$}] {
            set db [fresh_db unreadable_$name]
            set status 0
            try {
                exec timeout 20 $shell $db < $work > /dev/null 2> $errf
            } trap CHILDSTATUS {m opts} {
                set status [lindex [dict get $opts -errorcode] 2]
            } on error {} {
                set status -1
            }
            set fh [open $errf r]
            set err [read $fh]
            close $fh
            check {($status == 1 && [regexp -line $msg $err]) || ($status == 0 && $err eq "")} \
                "$name: unreadable standard input ends at once (1 and the message, or 0 as empty input)"
        }
    }

    # R25, R13: sqrsh piped: no banner and no prompts; failures on standard
    # error; status 1 when any command failed, the script running on.
    proc t_sqrsh_streams {} {
        variable sqrsh
        set db [fresh_db sqrsh_streams]
        set r [run [utf8 "open $db\ncreate t\ncol a int\ndone\ntables\nquit\n"] $sqrsh]
        check {[dict get $r status] == 0 && [dict get $r err] eq ""} "sqrsh: a clean script ends with 0, nothing on stderr"
        check {![regexp {sqr> |cols> |row> |sqr shell} [dict get $r out]]} "sqrsh: piped, no banner and no prompts"
        check {[regexp -line {^  t  \(0 rows, 1 cols\)$} [dict get $r out]]} "sqrsh: results on stdout"
        set r [run [utf8 "open $db\nuse t\nget 9\ninsert 5\nselect\nquit\n"] $sqrsh]
        check {[dict get $r status] == 1} "sqrsh: a failing command ends with 1"
        check {[regexp -line {^no such row: 9$} [dict get $r err]] && ![regexp {no such row} [dict get $r out]]} \
            "sqrsh: the failure is on stderr, not stdout"
        check {[regexp -line {^inserted row 1$} [dict get $r out]]} "sqrsh: the script runs on after a failure"
        set r [run [utf8 "tables\nquit\n"] $sqrsh]
        check {[dict get $r status] == 1 && [regexp {no database open} [dict get $r err]]} \
            "sqrsh: no database open is a failure"
        set r [run [utf8 "nosuch\nquit\n"] $sqrsh]
        check {[dict get $r status] == 1} "sqrsh: an unknown command ends with 1"
        # A database directory cannot be made beneath a regular file, whoever
        # is asking and on whatever platform.
        variable work
        set blocker [file join $work not_a_directory]
        close [open $blocker w]
        set r [run_args $sqrsh [file join $blocker x y]]
        file delete $blocker
        check {[dict get $r status] == 1 && [regexp {^open .* failed: } [dict get $r err]]} \
            "sqrsh: a failed start-up open is reported on stderr and ends with 1"
    }

    # R21: each message names the cause.
    proc t_sqrsh_reasons {} {
        variable sqrsh
        set db [fresh_db sqrsh_reasons]
        set cmds "open $db\ncreate t\ncol id int\ncol k char 4\ndone\nuse t\n"
        append cmds "insert 1 a\ninsert 2 b\nfind id 1\nindex unique id\n"
        append cmds "insert 1 c\nupdate 2 1 b\nupdate 9 3 c\ngetk k a\ngetk id 7\nfind id 7\n"
        append cmds "back\nreadonly\nuse t\ninsert 3 c\ndelete 1\nindex k\ncompact\nquit\n"
        set r [run [utf8 $cmds] $sqrsh]
        set err [dict get $r err]
        check {[regexp -line {^no index on that column$} $err]} "sqrsh reasons: find with no index says so"
        check {[regexp -line {^insert failed: unique-key violation on index over "id"} $err]} \
            "sqrsh reasons: insert names the unique violation"
        check {[regexp -line {^update failed: unique-key violation on index over "id"} $err]} \
            "sqrsh reasons: update names the unique violation, not a missing row"
        check {[regexp -line {^update failed: no such row 9$} $err]} "sqrsh reasons: update of a missing row"
        check {[regexp -line {^no unique index over those columns$} $err]} "sqrsh reasons: getk with no unique index says so"
        check {[regexp -line {^no row for that key$} $err]} "sqrsh reasons: getk of a missing key"
        check {[regexp -line {^no match$} $err]} "sqrsh reasons: find with an index and no row"
        check {[regexp -line {^insert failed: .*read-only} $err]
               && [regexp -line {^delete failed: .*read-only} $err]
               && [regexp -line {^create_index failed: .*read-only} $err]
               && [regexp -line {^compact failed: .*read-only} $err]} \
            "sqrsh reasons: read-only failures say read-only"
    }

    # R20: a char value too long for its column is refused, not cut; names
    # and values longer than a field are shown in full.
    proc t_sqrsh_char {} {
        variable sqrsh
        set db [fresh_db sqrsh_char]
        set long "abcdefghijklmnopqrstuvwxyz"
        set cmds "open $db\ncreate t\ncol id int\ncol k char 3\ncol a_very_long_column_name char 32\ndone\nuse t\n"
        append cmds "insert 1 abcd x\nindex k\nfind k abcd\ninsert 2 \"é\" $long\nupdate 2 2 wxyz q\nselect\nquit\n"
        set r [run [utf8 $cmds] $sqrsh]
        set out [dict get $r out]
        set err [dict get $r err]
        check {[regexp -all -line {^value too long for column "k"$} $err] == 3} \
            "sqrsh char: over-long value refused by insert, find and update"
        check {[regexp -line {^inserted row 1$} $out] && ![regexp {inserted row 2} $out]
               && [regexp -all -line {^ +[0-9]+ +[0-9]+ } $out] == 1} \
            "sqrsh char: the refused insert wrote nothing (the next row is row 1)"
        check {[regexp {a_very_long_column_name} $out] && [regexp "  $long\$" [lindex [split [string trim $out] "\n"] end]]} \
            "sqrsh char: a long name and a long value are shown in full"
        check {[regexp -line {^       1                 2                 é  abcdefghij} $out]} \
            "sqrsh char: a non-ASCII value is right-justified by characters"
    }

    # R26, R27, R14 and the command-word messages (cmdgraph 1.5.0): a quoted
    # token is text, either quote character quotes, `1e` is not a number,
    # and `unknown:` is on standard error.
    proc t_sqrsh_tokens {} {
        variable sqrsh
        set db [fresh_db sqrsh_tokens]
        set cmds "open $db\ncreate t\ncol id int\ncol r real\ncol k char 12\ndone\nuse t\n"
        append cmds "insert 1 1.5 \"null\"\ninsert 2 1.5 null\ninsert 3 1.5 'NULL'\n"
        append cmds "insert 4 1.5 'two words'\ninsert 5 1.5 '\"'\ninsert 6 1.5 \"it's\"\n"
        append cmds "insert 7 1.5 'it''s'\ninsert 8 1.5 O'Brien\n"
        append cmds "insert 20 1e x\ninsert 21 \"1.5\" x\ninsert 22 1.5 \"a\"b\ninsert 23 1.5 'open\n"
        append cmds "nosuch\nselect\nquit\n"
        set r [run [utf8 $cmds] $sqrsh]
        set out [dict get $r out]
        set err [dict get $r err]
        check {[regexp -line {^ +1 +1 +\S+ +null$} $out] && [regexp -line {^ +3 +3 +\S+ +NULL$} $out]} \
            "sqrsh tokens: a quoted null is the text, in either quote character"
        check {[regexp -line {^ +2 +2 +\S+ +NULL$} $out] && [regexp -all -line { NULL$} $out] == 2} \
            "sqrsh tokens: an unquoted null is NULL"
        check {[regexp -line {^ +4 +4 +\S+ +two words$} $out]} "sqrsh tokens: single quotes hold a space"
        check {[regexp -line {^ +5 +5 +\S+ +"$} $out]}         "sqrsh tokens: a double quote inside single quotes"
        check {[regexp -line {^ +6 +6 +\S+ +it's$} $out] && [regexp -line {^ +7 +7 +\S+ +it's$} $out]} \
            "sqrsh tokens: a single quote inside double quotes, and doubled"
        check {[regexp -line {^ +8 +8 +\S+ +O'Brien$} $out]}   "sqrsh tokens: a quote inside a word is ordinary"
        check {[regexp -all -line {^col 2: expected number$} $err] == 2} \
            "sqrsh tokens: 1e and a quoted number are not numbers"
        check {[regexp -line {^unexpected character after closing quote$} $err]
               && [regexp -line {^unmatched quote in arguments$} $err]} \
            "sqrsh tokens: a malformed quoted value is refused"
        check {[regexp -line {^unknown: nosuch$} $err] && ![regexp {unknown} $out]} \
            "sqrsh tokens: an unknown command is reported on stderr"
        check {![regexp -line {^ +[0-9]+ +2[0-3] } $out] && [dict get $r status] == 1} \
            "sqrsh tokens: the refused lines wrote nothing, and the status is 1"
    }

    # R24, R28, R30: ind(ex); what desc counts; 256 columns.
    proc t_sqrsh_misc {} {
        variable sqrsh
        set db [fresh_db sqrsh_misc]
        set cmds "open $db\ncreate t\n"
        for {set i 1} {$i <= 256} {incr i} { append cmds "col c$i int\n" }
        append cmds "col c257 int\ndone\ndesc t\nuse t\nind c1\nin [string repeat {1 } 256]\nback\ndesc t\nquit\n"
        set r [run [utf8 $cmds] $sqrsh]
        set out [dict get $r out]
        check {[regexp -line {^too many columns$} [dict get $r err]]
               && [regexp -line {^created table t with 256 columns$} $out]
               && [regexp -line {^  col 256 c256 \(int, 4 bytes\)$} $out]} \
            "sqrsh: create takes 256 columns, as CREATE TABLE does, and no more"
        check {[regexp -line {^indexed 1 column\(s\)$} $out]} "sqrsh: ind runs index"
        check {[regexp -line {^inserted row 1$} $out]} "sqrsh: in is now insert"
        check {[regexp -line {^  index on        c1 \(1 entries, deleted rows included\)$} $out]} \
            "sqrsh: desc says the entry count includes deleted rows"
    }

    # R23: match - a NULL matches nothing.  Only in a shell built with
    # `make sqrsh-regex`; the ordinary build has no match command.
    proc t_sqrsh_match {} {
        variable sqrsh
        set db [fresh_db sqrsh_match]
        set probe [run [utf8 "open $db\ncreate t\ncol id int\ncol c char 4\ndone\nuse t\nhelp\nquit\n"] $sqrsh]
        if {![regexp {m\(atch\)} [dict get $probe out]]} {
            puts "  SKIP sqrsh match: this sqrsh has no match command (make sqrsh-regex)"
            return
        }
        set r [run [utf8 "open $db\nuse t\ninsert 1 null\ninsert 2 \"\"\ninsert 3 ab\nmatch c ^\$\nmatch c (\nmatch nope x\nquit\n"] $sqrsh]
        set rows [regexp -all -line {^ +[0-9]+ +[0-9]+ } [dict get $r out]]
        check {$rows == 1 && [regexp -line {^       2                 2 *$} [dict get $r out]]} \
            "sqrsh match: ^\$ lists the empty value, not the NULL"
        check {[regexp -line {^bad regex: \( \(syntax error in the regular expression\)$} [dict get $r err]]
               && [regexp -line {^no such column: nope$} [dict get $r err]]} \
            "sqrsh match: failures on stderr, a bad pattern with its reason"
        check {![regexp -nocase {syntax error} [dict get $r out]]} \
            "sqrsh match: the regex engine writes nothing of its own to stdout"
    }

    proc t_utf8_bom {} {
        variable BOM_UTF8
        set db [fresh_db utf8bom]
        set sql ".open $db\nCREATE TABLE t(a INTEGER, b CHAR(8))\nINSERT INTO t VALUES(1,'délta')\nSELECT b FROM t\n.quit\n"
        set r [run $BOM_UTF8[utf8 $sql]]
        check {[dict get $r status] == 0}                 "utf-8 bom: exit status 0"
        check {![regexp {error|unknown} [dict get $r err]]} "utf-8 bom: first line runs (mark ignored)"
        check {[regexp -line "^délta *$" [dict get $r out]]} "utf-8 bom: non-ASCII value round-trips"
    }

    proc t_utf8_bom_alone {} {
        variable BOM_UTF8
        set db [fresh_db utf8alone]
        set r [run $BOM_UTF8[utf8 "\n.open $db\nCREATE TABLE t(a INTEGER)\n.quit\n"]]
        check {[dict get $r status] == 0 && ![regexp {error|unknown} [dict get $r err]]} \
            "utf-8 bom: a mark on an otherwise empty first line is harmless"
    }

    # The line every refusal must contain, for the encoding called `name`.
    proc refusal {name} {
        return "unsupported encoding ($name); expected ASCII or UTF-8"
    }

    # sqlsh fed `bytes`, which open with the signature of an encoding it does
    # not read and would create database `db` if they were run.
    proc t_refused {label name db bytes} {
        set r [run $bytes]
        check {[dict get $r status] == 1}                              "$label: exit status 1"
        check {[string first [refusal $name] [dict get $r err]] >= 0}  "$label: message names the encoding"
        check {[dict get $r out] eq ""}                                "$label: nothing run"
        check {![file exists $db]}                                     "$label: no database created"
    }

    proc t_wide {} {
        variable BOM_UTF16LE
        variable BOM_UTF16BE
        variable BOM_UTF32LE
        variable BOM_UTF32BE
        variable SIG_GB18030
        set db  [fresh_db wide]
        set sql ".open $db\nCREATE TABLE t(a INTEGER)\n.quit\n"
        t_refused "utf-16le" "UTF-16 / UCS-2, little-endian" $db $BOM_UTF16LE[utf16 $sql 0]
        t_refused "utf-16be" "UTF-16 / UCS-2, big-endian"    $db $BOM_UTF16BE[utf16 $sql 1]
        t_refused "utf-32le" "UTF-32 / UCS-4, little-endian" $db $BOM_UTF32LE[utf32 $sql 0]
        t_refused "utf-32be" "UTF-32 / UCS-4, big-endian"    $db $BOM_UTF32BE[utf32 $sql 1]
        t_refused "gb18030"  "GB18030"                       $db $SIG_GB18030[utf8 $sql]
        t_refused "utf-16le, no mark" \
            "NUL bytes; probably UTF-16 / UCS-2 or UTF-32 / UCS-4 without a byte-order mark" \
            $db [utf16 $sql 0]
    }

    # UTF-16 LE is what PowerShell 5.1 ">" writes: sqlsh adds the remedy.
    proc t_powershell_hint {} {
        variable BOM_UTF16LE
        variable BOM_UTF16BE
        set r [run $BOM_UTF16LE[utf16 ".quit\n" 0]]
        check {[regexp {Out-File -Encoding utf8} [dict get $r err]]}  "utf-16le: PowerShell remedy given"
        set r [run $BOM_UTF16BE[utf16 ".quit\n" 1]]
        check {![regexp {Out-File} [dict get $r err]]}                "utf-16be: no PowerShell remedy"
    }

    # A wide encoding that starts part-way through: the lines before it run,
    # then sqlsh stops.
    proc t_nul_later {} {
        set db [fresh_db nullater]
        set r [run [utf8 ".open $db\nCREATE TABLE t(a INTEGER)\n"][utf16 "CREATE TABLE u(a INTEGER)\n" 0]]
        check {[dict get $r status] == 1}        "nul on line 3: exit status 1"
        check {[regexp {NUL bytes} [dict get $r err]]} "nul on line 3: message says why"
        check {[file exists [file join $db t.dat]] && ![file exists [file join $db u.dat]]} \
            "nul on line 3: earlier lines ran, that line did not"
    }

    # ---- sqrsh: the same input classes, read by cmdgraph ----

    proc t_sqrsh_utf8_bom {} {
        variable sqrsh
        variable BOM_UTF8
        set r [run $BOM_UTF8[utf8 "help\nquit\n"] $sqrsh]
        check {[dict get $r status] == 0}                      "sqrsh utf-8 bom: exit status 0"
        check {![regexp {unknown} [dict get $r out][dict get $r err]]} "sqrsh utf-8 bom: first line runs (mark ignored)"
        check {[regexp {o\(pen\)} [dict get $r out]]}           "sqrsh utf-8 bom: help listed the commands"
    }

    proc t_sqrsh_refused {label name bytes} {
        variable sqrsh
        set r [run $bytes $sqrsh]
        check {[dict get $r status] == 1}                              "sqrsh $label: exit status 1"
        check {[string first [refusal $name] [dict get $r err]] >= 0}  "sqrsh $label: message names the encoding"
        check {![regexp {o\(pen\)} [dict get $r out]]}                  "sqrsh $label: nothing run"
    }

    proc t_sqrsh_wide {} {
        variable BOM_UTF16LE
        variable BOM_UTF16BE
        variable BOM_UTF32LE
        variable BOM_UTF32BE
        set cmds "help\nquit\n"
        t_sqrsh_refused "utf-16le" "UTF-16 / UCS-2, little-endian" $BOM_UTF16LE[utf16 $cmds 0]
        t_sqrsh_refused "utf-16be" "UTF-16 / UCS-2, big-endian"    $BOM_UTF16BE[utf16 $cmds 1]
        t_sqrsh_refused "utf-32le" "UTF-32 / UCS-4, little-endian" $BOM_UTF32LE[utf32 $cmds 0]
        t_sqrsh_refused "utf-32be" "UTF-32 / UCS-4, big-endian"    $BOM_UTF32BE[utf32 $cmds 1]
        t_sqrsh_refused "utf-16le, no mark" \
            "NUL bytes; probably UTF-16 / UCS-2 or UTF-32 / UCS-4 without a byte-order mark" \
            [utf16 $cmds 0]
    }

    proc t_sqrsh_plain {} {
        variable sqrsh
        set r [run [utf8 "help\nquit\n"] $sqrsh]
        check {[dict get $r status] == 0 && [dict get $r err] eq ""} "sqrsh plain: exit status 0, nothing on stderr"
    }

    # ---- the command line ----

    # A rejected command line: status 2, the reason and the usage on stderr,
    # and no directory named after the argument.
    proc t_rejected {shell prog label reason args} {
        variable work
        set r [run_args $shell {*}$args]
        check {[dict get $r status] == 2}                           "$prog $label: exit status 2"
        check {[string first "$prog: $reason" [dict get $r err]] >= 0} "$prog $label: says why"
        check {[string first "usage: $prog" [dict get $r err]] >= 0}   "$prog $label: prints the usage"
        set made 0
        foreach a $args { if {[file exists [file join $work $a]]} { set made 1 } }
        check {!$made}                                              "$prog $label: no directory created"
    }

    proc t_options {} {
        variable sqlsh
        variable sqrsh
        variable work
        set r [run_args $sqlsh --version]
        check {[dict get $r status] == 0}                       "sqlsh --version: exit status 0"
        check {[string trim [dict get $r out]] eq "sqlsh [fpm_version]"} \
            "sqlsh --version: prints the release in fpm.toml"
        set r [run_args $sqlsh --help]
        check {[dict get $r status] == 0 && [string first "usage: sqlsh" [dict get $r out]] >= 0} \
            "sqlsh --help: usage on stdout, exit status 0"
        check {![file exists [file join $work --version]] && ![file exists [file join $work --help]]} \
            "sqlsh --version, --help: no directory created"

        t_rejected $sqlsh sqlsh "--bogus"  "unknown option: --bogus" --bogus
        t_rejected $sqlsh sqlsh "-x"       "unknown option: -x"      -x
        t_rejected $sqlsh sqlsh "two args" "too many arguments"      one two
        t_rejected $sqrsh sqrsh "--version" "unknown option: --version" --version

        # A directory that really starts with '-' is still reachable.
        set r [run_args $sqlsh ./-odd]
        check {[dict get $r status] == 0 && [file isdirectory [file join $work -odd]]} \
            "sqlsh ./-odd: a directory starting with '-' opens when given as a path"
        file delete -force [file join $work -odd]
    }

    proc main {argv} {
        variable sqlsh
        variable work
        variable npass
        variable nfail
        variable sqrsh
        if {[llength $argv] != 2} {
            puts stderr "usage: tclsh run_shell.tcl <sqlsh-binary> <sqrsh-binary>"
            return 2
        }
        set sqlsh [file normalize [lindex $argv 0]]
        set sqrsh [file normalize [lindex $argv 1]]
        set work  [file join /tmp sqlsh_shelltest_[pid]]
        file delete -force $work
        file mkdir $work

        try {
            t_tables
            t_txn_fail
            t_reserved_name
            t_table_names
            t_sql_reasons
            t_display
            t_schema
            t_exit_status
            t_unreadable_stdin
            t_close_failure
            t_like_long
            t_add_text_column
            t_readonly_counters
            t_pack_too_large
            t_pack_unpack
            t_utf8_bom
            t_utf8_bom_alone
            t_wide
            t_powershell_hint
            t_nul_later
            t_sqrsh_plain
            t_sqrsh_streams
            t_sqrsh_reasons
            t_sqrsh_char
            t_sqrsh_misc
            t_sqrsh_tokens
            t_sqrsh_match
            t_sqrsh_utf8_bom
            t_sqrsh_wide
            t_options
        } finally {
            file delete -force $work
        }
        puts "shell tests: $npass passed, $nfail failed"
        return [expr {$nfail > 0}]
    }
}

exit [shelltest::main $argv]
