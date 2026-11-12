#!/usr/bin/env tclsh
# kate: syntax Tcl/Tk;
#
# run_shell.tcl — shell-level tests of the `sqlsh` REPL.
#
# run_sqlt.tcl covers SQL through sqlsh; the Fortran unit tests cover the
# library. This covers what sits between them and is reachable from neither:
# the meta-commands (".tables", ...) and how sqlsh treats the encoding of its
# input stream (UTF-8 byte-order mark ignored, UTF-16 refused).
#
# Each case writes a script as raw bytes, runs `sqlsh < script` and checks the
# exit status, stdout and stderr.
#
# Usage:   tclsh run_shell.tcl <sqlsh-binary>
# Exit:    0 if every check passed, 1 otherwise.

namespace eval shelltest {
    variable sqlsh  ""
    variable work   ""
    variable npass  0
    variable nfail  0

    # Byte-order marks, as the bytes that open a file.
    variable BOM_UTF8    [binary format H* EFBBBF]
    variable BOM_UTF16LE [binary format H* FFFE]
    variable BOM_UTF16BE [binary format H* FEFF]

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

    # Run sqlsh with `bytes` (a byte string) as its standard input. Returns a
    # dict: status (exit code), out (stdout), err (stderr).
    proc run {bytes} {
        variable sqlsh
        variable work
        set script [file join $work script.sql]
        set outf   [file join $work out.txt]
        set errf   [file join $work err.txt]
        set fh [open $script wb]
        puts -nonewline $fh $bytes
        close $fh
        set status 0
        if {[catch {exec $sqlsh < $script > $outf 2> $errf} msg opts]} {
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

    proc t_utf16 {label bom bigEndian} {
        set db [fresh_db utf16]
        set r [run $bom[utf16 ".open $db\nCREATE TABLE t(a INTEGER)\n.quit\n" $bigEndian]]
        check {[dict get $r status] == 1}                "$label: exit status 1"
        check {[regexp {UTF-16} [dict get $r err]]}      "$label: message names the encoding"
        check {[dict get $r out] eq ""}                  "$label: nothing run"
        check {![file exists $db]}                       "$label: no database created"
    }

    proc main {argv} {
        variable sqlsh
        variable work
        variable npass
        variable nfail
        variable BOM_UTF16LE
        variable BOM_UTF16BE
        if {[llength $argv] != 1} {
            puts stderr "usage: tclsh run_shell.tcl <sqlsh-binary>"
            return 2
        }
        set sqlsh [file normalize [lindex $argv 0]]
        set work  [file join /tmp sqlsh_shelltest_[pid]]
        file delete -force $work
        file mkdir $work

        try {
            t_tables
            t_utf8_bom
            t_utf8_bom_alone
            t_utf16 "utf-16le" $BOM_UTF16LE 0
            t_utf16 "utf-16be" $BOM_UTF16BE 1
        } finally {
            file delete -force $work
        }
        puts "shell tests: $npass passed, $nfail failed"
        return [expr {$nfail > 0}]
    }
}

exit [shelltest::main $argv]
