#!/usr/bin/env tclsh
# kate: syntax Tcl/Tk;
#
# run_sqlt.tcl — a sqllogictest-subset harness driving the `sqlsh` REPL.
#
# sqllogictest (https://www.sqlite.org/sqllogictest/) is the engine-agnostic
# record/replay format Richard Hipp built to cross-check SQLite against other
# SQL engines, so it carries no SQLite-specific assumptions — which makes it the
# natural body of black-box tests to point at sqr's SQL subset (`sqlsh`).
#
# It implements the slice of the format that sqr's subset can actually exercise
# (no JOIN / aggregate / subquery / LIKE / IN), and parses sqlsh's human-readable
# table output rather than a machine mode (see LIMITATIONS in sqlt/README.md).
# It is pure Tcl + the system `md5sum`; no tcllib needed.
#
# Usage:   tclsh run_sqlt.tcl <sqlsh-binary> <file.test> [more.test ...]
# Exit:    0 if every record passed, 1 otherwise.
#
# How it drives sqlsh
# -------------------
# Each record is run as its OWN sqlsh invocation against ONE database directory
# that persists on disk for the whole file (sqr auto-commits + fsyncs per
# mutator, so a later process sees an earlier one's writes). That makes output
# attribution trivial: an invocation's stdout is exactly that statement's result
# and its stderr ("error: ...") flags failure — sqlsh always exits 0.

set ::PASS 0
set ::FAIL 0
set ::SKIP 0
set ::failures {}        ;# list of human-readable failure reports

# --------------------------------------------------------------------------
# Driving sqlsh
# --------------------------------------------------------------------------

# Run one SQL statement against the file's database dir. Returns a dict with
#   out    : stdout as a list of lines (query result table / messages)
#   failed : 1 if sqlsh reported an "error: ..." on stderr, else 0
#   err    : the raw stderr text
proc run_sqlsh {sql} {
    set errfile [file join [file dirname $::dbdir] sqlt_err_[pid].txt]
    set rc [catch {
        exec $::sqlsh $::dbdir << "$sql\n" 2> $errfile
    } out]
    set err ""
    if {[file exists $errfile]} {
        set fh [open $errfile r]; set err [read $fh]; close $fh
        file delete -force $errfile
    }
    # exec raises on non-zero exit; sqlsh exits 0 even on SQL errors, so a
    # raised rc is a real crash — surface it as a failure with the message.
    if {$rc && $err eq ""} { set err $out }
    set failed [regexp -line {^error:} $err]
    set lines [split $out "\n"]
    return [dict create out $lines failed $failed err $err]
}

# --------------------------------------------------------------------------
# Parsing sqlsh's rendered result table
# --------------------------------------------------------------------------
# A SELECT renders as:
#     <header>
#     <dashes>            e.g. "-  --  --------------"
#     <row> ...
#     (N row(s))
# The dashes line gives the exact column slices (runs of '-' separated by two
# spaces), so we cut each data row at those fixed offsets — robust to spaces
# inside values. Empty cells trim to "".

# Column [start end] character ranges (0-based, inclusive) from the dashes line.
proc dash_columns {dash} {
    set cols {}
    set n [string length $dash]
    set i 0
    while {$i < $n} {
        if {[string index $dash $i] eq "-"} {
            set start $i
            while {$i < $n && [string index $dash $i] eq "-"} { incr i }
            lappend cols [list $start [expr {$i - 1}]]
        } else {
            incr i
        }
    }
    return $cols
}

# Parse stdout lines of a SELECT into a list of rows (each a list of cell text).
proc parse_table {lines} {
    set di -1
    for {set i 0} {$i < [llength $lines]} {incr i} {
        if {[regexp {^-+(  -+)*[ ]*$} [lindex $lines $i]]} { set di $i; break }
    }
    if {$di < 0} { error "no result table in sqlsh output" }
    set cols [dash_columns [lindex $lines $di]]
    set rows {}
    for {set i [expr {$di + 1}]} {$i < [llength $lines]} {incr i} {
        set ln [lindex $lines $i]
        if {[regexp {^\(\d+ row\(s\)\)} $ln]} break
        set row {}
        foreach c $cols {
            lassign $c s e
            lappend row [string trimright [string range $ln $s $e]]
        }
        lappend rows $row
    }
    return $rows
}

# --------------------------------------------------------------------------
# sqllogictest value canonicalisation
# --------------------------------------------------------------------------
# Per the spec, each value is rendered to text by its declared column type:
#   I  integer   R  real (3 d.p.)   T  text (empty -> "(empty)")
# NULL is the literal "NULL" in every type. sqlsh prints reals as es15.8, so the
# R path re-parses and reformats to the canonical %.3f.
proc fmt_cell {val type} {
    if {$val eq "NULL"} { return "NULL" }
    switch -- $type {
        I {
            if {[string is double -strict $val]} { return [expr {int($val)}] }
            return $val
        }
        R {
            if {[string is double -strict $val]} { return [format %.3f $val] }
            return $val
        }
        default {                      ;# T (and anything unspecified)
            if {$val eq ""} { return "(empty)" }
            return $val
        }
    }
}

# Rows -> flat list of canonical values, applying the sort mode.
#   nosort    : result order
#   rowsort   : sort whole rows, then flatten
#   valuesort : flatten, then sort individual values
proc result_values {rows types sortmode} {
    set frows {}
    foreach row $rows {
        set fr {}
        set j 0
        foreach v $row {
            lappend fr [fmt_cell $v [string index $types $j]]
            incr j
        }
        lappend frows $fr
    }
    if {$sortmode eq "rowsort"} { set frows [lsort $frows] }
    set flat {}
    foreach fr $frows { foreach v $fr { lappend flat $v } }
    if {$sortmode eq "valuesort"} { set flat [lsort $flat] }
    return $flat
}

# md5 hex of a string via the system md5sum (avoids a tcllib dependency).
proc md5hex {s} {
    set h [exec md5sum << $s]
    return [lindex $h 0]
}

# --------------------------------------------------------------------------
# Record runners
# --------------------------------------------------------------------------

proc record_loc {file lineno} { return "$file:$lineno" }

proc fail {loc sql msg} {
    incr ::FAIL
    lappend ::failures "FAIL $loc\n    SQL: $sql\n    $msg"
}

proc run_statement {loc sql expect} {
    set r [run_sqlsh $sql]
    set failed [dict get $r failed]
    if {$expect eq "ok" && $failed} {
        fail $loc $sql "expected success, got error: [string trim [dict get $r err]]"
    } elseif {$expect eq "error" && !$failed} {
        fail $loc $sql "expected an error, but the statement succeeded"
    } else {
        incr ::PASS
    }
}

proc run_query {loc sql types sortmode expected} {
    set r [run_sqlsh $sql]
    if {[dict get $r failed]} {
        fail $loc $sql "query failed: [string trim [dict get $r err]]"
        return
    }
    if {[catch {parse_table [dict get $r out]} rows]} {
        fail $loc $sql "could not parse result: $rows"
        return
    }
    set got [result_values $rows $types $sortmode]

    # Expected block is either a hash line or a list of literal values.
    if {[regexp {^(\d+) values hashing to ([0-9a-fA-F]+)$} \
             [string trim [join $expected "\n"]] -> nv hash]} {
        set buf ""
        foreach v $got { append buf $v "\n" }
        if {[llength $got] != $nv} {
            fail $loc $sql "hash count mismatch: expected $nv values, got [llength $got]"
        } elseif {[md5hex $buf] ne [string tolower $hash]} {
            fail $loc $sql "hash mismatch: expected $hash, got [md5hex $buf]"
        } else {
            incr ::PASS
        }
        return
    }

    if {$got eq $expected} {
        incr ::PASS
    } else {
        fail $loc $sql "result mismatch:\n      expected: [list $expected]\n      got:      [list $got]"
    }
}

# --------------------------------------------------------------------------
# .test file parser
# --------------------------------------------------------------------------

proc run_file {file} {
    set fh [open $file r]
    set lines [split [read $fh] "\n"]
    close $fh

    # Fresh database directory for this file.
    set ::dbdir [file join /tmp sqlt_db_[pid]_[file rootname [file tail $file]]]
    file delete -force $::dbdir

    set n [llength $lines]
    set i 0
    set cond {}                ;# pending skipif/onlyif for the next record
    while {$i < $n} {
        set line [lindex $lines $i]
        set lineno [expr {$i + 1}]
        set trimmed [string trim $line]

        # Blank lines and comments separate records.
        if {$trimmed eq "" || [string index $trimmed 0] eq "#"} { incr i; continue }

        set tok [split $trimmed]
        set kw [lindex $tok 0]

        switch -- $kw {
            halt { break }
            hash-threshold { incr i; continue }
            skipif { lappend cond skipif [lindex $tok 1]; incr i; continue }
            onlyif { lappend cond onlyif [lindex $tok 1]; incr i; continue }
        }

        # Decide whether to skip this record for our engine name "sqr".
        set skip 0
        foreach {k v} $cond {
            if {$k eq "skipif" && $v eq "sqr"} { set skip 1 }
            if {$k eq "onlyif" && $v ne "sqr"} { set skip 1 }
        }
        set cond {}

        if {$kw eq "statement"} {
            set expect [lindex $tok 1]          ;# ok | error
            incr i
            set sqllines {}
            while {$i < $n && [string trim [lindex $lines $i]] ne ""} {
                lappend sqllines [string trim [lindex $lines $i]]
                incr i
            }
            set sql [join $sqllines " "]
            if {$skip} { incr ::SKIP } else {
                run_statement [record_loc $file $lineno] $sql $expect
            }
        } elseif {$kw eq "query"} {
            set types [lindex $tok 1]
            set sortmode "nosort"
            if {[llength $tok] >= 3} {
                set m [lindex $tok 2]
                if {$m in {nosort rowsort valuesort}} { set sortmode $m }
            }
            incr i
            set sqllines {}
            while {$i < $n && [string trim [lindex $lines $i]] ne "----"} {
                lappend sqllines [string trim [lindex $lines $i]]
                incr i
            }
            incr i                              ;# skip the ---- separator
            set sql [join $sqllines " "]
            set expected {}
            while {$i < $n && [string trim [lindex $lines $i]] ne ""} {
                lappend expected [string trim [lindex $lines $i]]
                incr i
            }
            if {$skip} { incr ::SKIP } else {
                run_query [record_loc $file $lineno] $sql $types $sortmode $expected
            }
        } else {
            puts stderr "warning: $file:$lineno: unknown record \"$kw\" (skipped)"
            incr i
        }
    }
    file delete -force $::dbdir
}

# --------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------

if {[llength $argv] < 2} {
    puts stderr "usage: tclsh run_sqlt.tcl <sqlsh-binary> <file.test> \[more.test ...\]"
    exit 2
}
set ::sqlsh [lindex $argv 0]
if {![file executable $::sqlsh]} {
    puts stderr "error: sqlsh binary not found or not executable: $::sqlsh"
    exit 2
}

foreach f [lrange $argv 1 end] {
    puts "==> $f"
    run_file $f
}

puts "----------------------------------------"
puts "sqlt: $::PASS passed, $::FAIL failed, $::SKIP skipped"
if {$::FAIL > 0} {
    puts ""
    foreach r $::failures { puts $r; puts "" }
    exit 1
}
exit 0
