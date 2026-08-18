# test_calc.tcl — functional test for the Calc client's Tcl director.
#
# Runs the real sqr_calc.tcl against (a) a mock servant — the six servant
# commands over a dict-backed grid instead of pyuno — and (b) a real spawned
# sqrd.  This exercises everything except the ~100 pyuno lines in
# tclcalc.py, which get a manual smoke test in Calc (see README.md).
#
# Usage: tclsh test_calc.tcl <path-to-sqrd>

if {$::argc != 1} {
    puts stderr "usage: tclsh test_calc.tcl <path-to-sqrd>"
    exit 2
}
set exe [lindex $::argv 0]
set dbdir calc_test_db
file delete -force $dbdir

set npass 0
set nfail 0
proc check {cond label} {
    if {[uplevel 1 [list expr $cond]]} {
        incr ::npass
        puts "  OK   $label"
    } else {
        incr ::nfail
        puts "  FAIL $label"
    }
}

# ---- mock servant: the six commands over a dict-backed grid ----
namespace eval mock {
    variable sheets [dict create]   ;# name -> dict "r,c" -> tagged cell
    variable cur ""
    variable msgs {}
}
proc sheet {name} {
    set created 0
    if {![dict exists $::mock::sheets $name]} {
        dict set ::mock::sheets $name [dict create]
        set created 1
    }
    set ::mock::cur $name
    return $created
}
proc used {} {
    set grid [dict get $::mock::sheets $::mock::cur]
    set maxr 1
    set maxc 1
    dict for {rc -} $grid {
        lassign [split $rc ,] r c
        if {$r > $maxr} { set maxr $r }
        if {$c > $maxc} { set maxc $c }
    }
    list 1 1 $maxr $maxc
}
proc getcells {r1 c1 r2 c2} {
    set grid [dict get $::mock::sheets $::mock::cur]
    set out {}
    for {set r $r1} {$r <= $r2} {incr r} {
        for {set c $c1} {$c <= $c2} {incr c} {
            if {[dict exists $grid $r,$c]} {
                lappend out [dict get $grid $r,$c]
            } else {
                lappend out {N}
            }
        }
    }
    return $out
}
proc putcells {r1 c1 nr nc block} {
    set k 0
    for {set r $r1} {$r < $r1 + $nr} {incr r} {
        for {set c $c1} {$c < $c1 + $nc} {incr c} {
            set cell [lindex $block $k]
            incr k
            if {[lindex $cell 0] eq "N"} {
                dict unset ::mock::sheets $::mock::cur $r,$c
            } else {
                dict set ::mock::sheets $::mock::cur $r,$c $cell
            }
        }
    }
}
proc clearcells {} {
    dict set ::mock::sheets $::mock::cur [dict create]
}
proc msg {text} {
    lappend ::mock::msgs $text
    puts "  (msg) $text"
}
# test-side accessors
proc cellat {sheetname r c} {
    set grid [dict get $::mock::sheets $sheetname]
    if {[dict exists $grid $r,$c]} {
        return [dict get $grid $r,$c]
    }
    return {N}
}
proc setcell {sheetname r c cell} {
    sheet $sheetname
    putcells $r $c 1 1 [list $cell]
}

# ---- the director under test ----
source [file join [file dirname [info script]] sqr_calc.tcl]

# ---- start sqrd, create the test table over the wire ----
set srv [open [list | $exe $dbdir 0] r]
set port [lindex [gets $srv] 1]
check {$port > 0} "sqrd started (port $port)"

sqr::dial 127.0.0.1 $port
sqr::run "CREATE TABLE t (id INTEGER, x REAL, name CHAR(12), note TEXT)"
sqr::run "CREATE UNIQUE INDEX ON t (id)"
sqr::run "INSERT INTO t VALUES (2, 0.1, 'Bob', 'second')"
sqr::run "INSERT INTO t VALUES (1, -1.5e-300, 'Alice', 'first')"
sqr::run "INSERT INTO t (id) VALUES (3)"
sqr::hangup
check {1} "test table created and seeded"

# ---- 1: first pull bootstraps the configuration sheet ----
sqr::main pull
check {[dict exists $::mock::sheets sqr]} "pull created the config sheet"
check {[string match *configuration* [lindex $::mock::msgs end]]} \
    "user told to fill the config in"
check {[cellat sqr 2 2] eq {V 7477}} "default port 7477 prefilled"
check {![dict exists $::mock::sheets t]} "no data sheet yet"

# point the config at the live server
sheet sqr
putcells 1 1 3 2 [list {S host} {S 127.0.0.1} {S port} [list V $port] {S table} {S t}]

# ---- 2: pull fills the data sheet ----
sqr::main pull
check {[dict exists $::mock::sheets t]} "pull created the data sheet"
check {[cellat t 1 1] eq {S id} && [cellat t 1 4] eq {S note}} "header row written"
check {[lindex [cellat t 2 1] 1] == 1 && [lindex [cellat t 3 1] 1] == 2} \
    "rows arrive in key order"
check {[lindex [cellat t 3 2] 1] == 0.1} "REAL survives exactly (0.1)"
check {[lindex [cellat t 2 2] 1] == -1.5e-300} "REAL survives exactly (-1.5e-300)"
check {[cellat t 2 3] eq {S Alice} && [cellat t 2 4] eq {S first}} "CHAR and TEXT cells"
check {[cellat t 4 2] eq {N} && [cellat t 4 4] eq {N}} "NULLs arrive as empty cells"
check {[string match "*pulled 3*" [lindex $::mock::msgs end]]} "pull reported 3 rows"

# ---- 3: mutate the grid and push ----
set third [expr {1.0 / 3.0}]
setcell t 3 2 [list V $third]              ;# id 2: new x
setcell t 2 4 {N}                          ;# id 1: note -> NULL
sheet t
putcells 5 1 1 4 [list {V 4} {V 0.25} {S Dave} {N}]   ;# new row id 4
sqr::main push
check {[string match "*pushed 4*" [lindex $::mock::msgs end]]} "push reported 4 rows"

sqr::dial 127.0.0.1 $port
lassign [sqr::fetch "SELECT * FROM t ORDER BY id"] names types nr nc cells
check {$nr == 4} "table now holds 4 rows"
check {[lindex $cells 5 1] == $third} "pushed REAL is bit-exact (1/3)"
check {[lindex $cells 3] eq {N}} "note NULLed on push"
check {[lindex $cells 14] eq {S Dave} && [lindex $cells 15] eq {N}} "new row landed"
sqr::hangup

# ---- 4: a failing push rolls back to the intact table ----
setcell t 2 1 {S notanumber}               ;# id column, unparseable
set rc [catch {sqr::main push} err]
check {$rc == 1 && [string match "*unchanged*" $err]} "bad push raises: $err"
sqr::dial 127.0.0.1 $port
lassign [sqr::fetch "SELECT id FROM t ORDER BY id"] - - nr - cells
check {$nr == 4 && [lindex $cells 0 1] == 1} "table unchanged after rollback"
sqr::hangup
setcell t 2 1 {V 1}                        ;# restore

# ---- 5: header validation ----
setcell t 1 2 {S bogus}
set rc [catch {sqr::main push} err]
check {$rc == 1 && [string match "*not a column*" $err]} "unknown header rejected: $err"
setcell t 1 2 {S x}

# ---- shut down ----
exec kill [pid $srv]
catch {close $srv}
file delete -force $dbdir

puts "calc client functional: $npass passed, $nfail failed"
exit [expr {$nfail > 0}]
