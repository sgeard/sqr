#!/usr/bin/env tclsh
# kate: syntax Tcl/Tk;
#
# run_shell.tcl — shell-level tests of the `sqlsh` and `sqrsh` REPLs.
#
# run_sqlt.tcl covers SQL through sqlsh; the Fortran unit tests cover the
# library. This covers what sits between them and is reachable from neither:
# the meta-commands (".tables", ...) and how the two shells treat the encoding
# of their input stream (UTF-8 byte-order mark ignored; UTF-16, UTF-32 and
# the other recognisable encodings refused by name, with exit status 1).
# sqlsh reads its own input and sqrsh's is read by cmdgraph, so the same
# cases are run through both to show that they give the same answer.
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
            t_utf8_bom
            t_utf8_bom_alone
            t_wide
            t_powershell_hint
            t_nul_later
            t_sqrsh_plain
            t_sqrsh_utf8_bom
            t_sqrsh_wide
        } finally {
            file delete -force $work
        }
        puts "shell tests: $npass passed, $nfail failed"
        return [expr {$nfail > 0}]
    }
}

exit [shelltest::main $argv]
