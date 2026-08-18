# sqr_calc.tcl — the sqr side of the LibreOffice Calc client (the director).
#
# This script contains everything sqr: the wire-protocol client
# (reports/DESIGN-wire-protocol.md), the configuration convention and the
# pull/push actions.  It is host-agnostic: the interpreter that sources it
# must provide six "servant" commands for the spreadsheet surface —
#
#   sheet <name>                  select a sheet, creating it if absent;
#                                 returns 1 if it was created, 0 otherwise
#   used                          -> {r1 c1 r2 c2}, the selected sheet's
#                                 used area (1-based; {1 1 1 1} when empty)
#   getcells r1 c1 r2 c2          -> flat row-major list of tagged cells
#   putcells r1 c1 nr nc <cells>  write a flat row-major block
#   clearcells                    clear the selected sheet's used area
#   msg <text>                    user-visible notice (message box)
#
# A tagged cell is {V <number>} (numeric), {S <text>} (string) or {N}
# (empty / SQL NULL).  Numbers cross as Tcl doubles/ints and are formatted
# shortest-round-trip by Tcl itself, so values stay exact end to end and a
# {V} value used verbatim as a SQL literal satisfies the protocol's
# 17-significant-digit write rule.
#
# Hosts: tclcalc.py registers the servant commands over pyuno (the real
# LibreOffice macro); test_calc.tcl registers them over a dict-backed grid
# (the functional test).  Entry point: sqr::main pull|push.
#
# Configuration lives in a sheet named "sqr": key cells in column A
# (host/port/table), values in column B.  Pull creates it with defaults on
# first use.  The data sheet is named after the table and is owned by this
# script: row 1 headers, data rows below, nothing else.

namespace eval sqr {
    variable sock ""
    variable actions {pull ::sqr::pull push ::sqr::push}
}

proc sqr::main {verb} {
    variable actions
    if {![dict exists $actions $verb]} {
        error "unknown action \"$verb\" (expected pull or push)"
    }
    [dict get $actions $verb]
}

# ---------------------------------------------------------------- wire client

proc sqr::sendline {line} {
    variable sock
    puts -nonewline $sock "$line\n"
    flush $sock
}

proc sqr::getline {} {
    variable sock
    if {[gets $sock line] < 0} {
        error "sqrd closed the connection"
    }
    return $line
}

proc sqr::sendsql {sql} {
    variable sock
    sendline "SQL [string length $sql]"
    puts -nonewline $sock $sql
    flush $sock
}

# Read one non-ROWS reply -> {header payload}.
proc sqr::reply {} {
    variable sock
    set hdr [getline]
    set payload ""
    if {[lindex $hdr 0] in {MSG ERR}} {
        set payload [::read $sock [lindex $hdr end]]
    }
    list $hdr $payload
}

# Execute one statement, error (with the server's message) on ERR.
proc sqr::run {sql} {
    sendsql $sql
    lassign [reply] hdr payload
    if {[lindex $hdr 0] eq "ERR"} {
        error $payload
    }
    list $hdr $payload
}

# Little-endian binary cells (i = LE int32, q = LE double).
proc sqr::cint {bytes} {
    binary scan $bytes i v
    return $v
}
proc sqr::creal {bytes} {
    binary scan $bytes q v
    return $v
}

# Read a complete ROWS response -> {names types nr nc cells} where cells is
# a flat row-major list of servant-tagged cells decoded per column type.
# An ERR response raises with the server's message.
proc sqr::rows {} {
    variable sock
    set hdr [getline]
    if {[lindex $hdr 0] eq "ERR"} {
        error [::read $sock [lindex $hdr end]]
    }
    if {[lindex $hdr 0] ne "ROWS"} {
        error "protocol: expected ROWS, got \"$hdr\""
    }
    lassign [lrange $hdr 1 2] nr nc
    set names {}
    set types {}
    for {set j 0} {$j < $nc} {incr j} {
        set col [getline]                     ;# COL name type csize null key
        lappend names [lindex $col 1]
        lappend types [lindex $col 2]
    }
    set cells {}
    for {set k 0} {$k < $nr * $nc} {incr k} {
        set c [getline]
        if {$c eq "N"} {
            lappend cells {N}
            continue
        }
        set bytes [::read $sock [lindex $c 1]]
        switch [lindex $types [expr {$k % $nc}]] {
            INT     { lappend cells [list V [cint $bytes]] }
            REAL    { lappend cells [list V [creal $bytes]] }
            default { lappend cells [list S $bytes] }
        }
    }
    if {[getline] ne "END"} {
        error "protocol: missing END"
    }
    list $names $types $nr $nc $cells
}

# Run a SELECT and read its result set.
proc sqr::fetch {sql} {
    sendsql $sql
    rows
}

# Catalogue: COLUMNS <table> -> {names types keyords}.  The meta result is
# an ordinary ROWS frame (name, type, csize, nullable, key per column), so
# rows() does the reading; this just re-shapes it.
proc sqr::columns {table} {
    sendline "COLUMNS $table"
    lassign [rows] - - nr nc cells
    set names {}
    set types {}
    set keys {}
    for {set base 0} {$base < [llength $cells]} {incr base $nc} {
        lappend names [lindex $cells $base 1]
        lappend types [lindex $cells [expr {$base + 1}] 1]
        lappend keys  [lindex $cells [expr {$base + 4}] 1]
    }
    list $names $types $keys
}

proc sqr::dial {host port} {
    variable sock
    set sock [socket $host $port]
    fconfigure $sock -translation binary -blocking 1
    sendline "HELLO 1 calc"
    set ok [getline]
    if {[lrange $ok 0 1] ne "OK 1"} {
        error "sqrd handshake failed: $ok"
    }
}

proc sqr::hangup {} {
    variable sock
    if {$sock eq ""} return
    catch {
        sendline QUIT
        reply
    }
    catch {close $sock}
    set sock ""
}

# ------------------------------------------------------------- configuration

# Read the configuration from sheet "sqr" (key/value pairs in columns A/B).
# Creates the sheet with defaults on first use.  Returns a dict with host,
# port and table, or "" after telling the user what to fill in.
proc sqr::config {} {
    if {[sheet sqr]} {
        putcells 1 1 3 2 [list {S host} {S 127.0.0.1} {S port} {V 7477} {S table} {N}]
        msg "sqr: created configuration sheet \"sqr\" - set the table name next to \"table\" and run again."
        return ""
    }
    lassign [used] r1 c1 r2 c2
    set nc [expr {$c2 - $c1 + 1}]
    set flat [getcells $r1 $c1 $r2 $c2]
    set cfg [dict create host 127.0.0.1 port 7477 table ""]
    set base 0
    for {set r $r1} {$r <= $r2} {incr r} {
        lassign [lindex $flat $base] ktag key
        lassign [lindex $flat [expr {$base + 1}]] vtag val
        incr base $nc
        if {$ktag ne "S" || $vtag eq "N" || $vtag eq ""} continue
        set key [string tolower [string trim $key]]
        if {$key in {host port table}} {
            dict set cfg $key [string trim $val]
        }
    }
    if {[dict get $cfg table] eq ""} {
        msg "sqr: no table configured - fill in the \"table\" value on sheet \"sqr\"."
        return ""
    }
    # a numeric cell arrives as a double ("7477.0"): normalise to an integer
    dict set cfg port [expr {entier([dict get $cfg port])}]
    return $cfg
}

# ------------------------------------------------------------------- actions

proc sqr::pull {} {
    set cfg [config]
    if {$cfg eq ""} return
    set table [dict get $cfg table]
    dial [dict get $cfg host] [dict get $cfg port]
    lassign [columns $table] cnames ctypes keys
    # deterministic order: the designated key, in ordinal order
    set order {}
    foreach ord [lsort -integer -unique $keys] {
        if {$ord > 0} {
            lappend order [lindex $cnames [lsearch -exact $keys $ord]]
        }
    }
    set sql "SELECT * FROM [qid $table]"
    if {[llength $order]} {
        set qorder [lmap o $order {qid $o}]
        append sql " ORDER BY [join $qorder {, }]"
    }
    lassign [fetch $sql] names types nr nc cells
    hangup
    sheet $table
    clearcells
    set block {}
    foreach name $names {
        lappend block [list S $name]
    }
    lappend block {*}$cells
    putcells 1 1 [expr {$nr + 1}] $nc $block
    msg "sqr: pulled $nr row(s) of $table"
}

proc sqr::push {} {
    set cfg [config]
    if {$cfg eq ""} return
    set table [dict get $cfg table]
    dial [dict get $cfg host] [dict get $cfg port]
    lassign [columns $table] cnames ctypes keys
    if {[sheet $table]} {
        hangup
        error "no data sheet \"$table\" to push (pull first)"
    }
    lassign [used] r1 c1 r2 c2
    set nc [expr {$c2 - $c1 + 1}]
    set flat [getcells $r1 $c1 $r2 $c2]
    # row 1: headers name the target columns (any order; the server checks them)
    set headers {}
    set types {}
    for {set j 0} {$j < $nc} {incr j} {
        lassign [lindex $flat $j] tag text
        if {$tag ne "S" || [string trim $text] eq ""} {
            hangup
            error "row 1 of sheet \"$table\" must hold column names (cell [expr {$j + 1}] does not)"
        }
        set text [string trim $text]
        set at [lsearch -exact $cnames $text]
        if {$at < 0} {
            hangup
            error "\"$text\" is not a column of $table"
        }
        lappend headers $text
        lappend types [lindex $ctypes $at]
    }
    # full-replace inside one transaction: a failure rolls back to the old table
    set qheaders [lmap h $headers {qid $h}]
    set npushed 0
    run BEGIN
    if {[catch {
        run "DELETE FROM [qid $table]"
        for {set base $nc} {$base < [llength $flat]} {incr base $nc} {
            set row [lrange $flat $base [expr {$base + $nc - 1}]]
            if {[empty_row $row]} continue
            set lits {}
            foreach cell $row type $types {
                lappend lits [literal $cell $type]
            }
            run "INSERT INTO [qid $table] ([join $qheaders {, }]) VALUES ([join $lits {, }])"
            incr npushed
        }
        run COMMIT
    } err]} {
        catch {run ROLLBACK}
        hangup
        error "push failed, table unchanged: $err"
    }
    hangup
    msg "sqr: pushed $npushed row(s) to $table"
}

# ------------------------------------------------------------------- helpers

# A row is empty (used-range slack) when every cell is N or a blank string.
# A name -> a double-quoted (delimited) SQL identifier, so table and column
# names with spaces or punctuation ("Rate %", "Account#") are always legal
# in the statements this client builds.  Embedded quotes double.
proc sqr::qid {name} {
    return "\"[string map {\" \"\"} $name]\""
}

proc sqr::empty_row {row} {
    foreach cell $row {
        lassign $cell tag v
        if {$tag eq "V"} { return 0 }
        if {$tag eq "S" && [string trim $v] ne ""} { return 0 }
    }
    return 1
}

# One tagged cell -> one SQL literal, guided by the column type.  Empty
# strings are NULL (blanks are never significant); numbers into CHAR/TEXT
# columns become their text; numeric-looking text into INT/REAL columns is
# passed through for the server to parse.
proc sqr::literal {cell type} {
    lassign $cell tag v
    switch $tag {
        N {
            return NULL
        }
        S {
            set v [string trim $v]
            if {$v eq ""} { return NULL }
            if {$type in {INT REAL}} { return $v }
            return "'[string map {' ''} $v]'"
        }
        V {
            if {$type eq "INT"} {
                if {$v != entier($v)} {
                    error "non-integer value $v for INTEGER column"
                }
                return [expr {entier($v)}]
            }
            if {$type in {CHAR TEXT}} { return "'$v'" }
            return $v
        }
    }
    error "bad cell \"$cell\""
}
