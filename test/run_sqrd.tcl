# Functional test for sqrd: a complete client session over the wire
# protocol (reports/DESIGN-wire-protocol.md), driven from Tcl.
#
# Deliberately an independent implementation — nothing here shares code with
# sqr_net/sqr_serve, so it doubles as a protocol conformance check and as the
# reference for other clients (the LibreOffice macro speaks the protocol the
# same way).  A real sqrd process is spawned on an ephemeral port; the port
# is read back from the LISTENING line on its stdout.
#
# Usage: tclsh run_sqrd.tcl <path-to-sqrd>

if {$::argc != 1} {
    puts stderr "usage: tclsh run_sqrd.tcl <path-to-sqrd>"
    exit 2
}
set exe [lindex $::argv 0]
set dbdir sqrd_func_db
file delete -force $dbdir

set npass 0
set nfail 0
# cond arrives braced and is evaluated in the caller's scope.
proc check {cond label} {
    if {[uplevel 1 [list expr $cond]]} {
        incr ::npass
        puts "  OK   $label"
    } else {
        incr ::nfail
        puts "  FAIL $label"
    }
}

# ---- start the server, read the port back ----
set srv [open [list | $exe $dbdir 0] r]
fconfigure $srv -blocking 1
set line [gets $srv]
check {[lindex $line 0] eq "LISTENING"} "server announces LISTENING"
set port [lindex $line 1]

# ---- protocol plumbing ----
proc dial {port} {
    set s [socket 127.0.0.1 $port]
    fconfigure $s -translation binary -blocking 1
    return $s
}
# Both senders return the socket so a request/response pair composes as
# [reply [sendsql $s ...]].
proc sendline {s line} {
    puts -nonewline $s "$line\n"
    flush $s
    return $s
}
proc sendsql {s sql} {
    sendline $s "SQL [string length $sql]"
    puts -nonewline $s $sql
    flush $s
    return $s
}
# Read one non-ROWS reply: returns {header payload}.
proc reply {s} {
    set hdr [gets $s]
    set payload ""
    if {[lindex $hdr 0] in {MSG ERR}} {
        set payload [read $s [lindex $hdr end]]
    }
    list $hdr $payload
}
# Read a full ROWS response: returns {nrows ncols collines cells} where
# cells is a flat row-major list of {N} or {C <bytes>} pairs.
proc rows {s} {
    set hdr [gets $s]
    if {[lindex $hdr 0] ne "ROWS"} {
        error "expected ROWS, got: $hdr"
    }
    lassign [lrange $hdr 1 2] nr nc
    set collines {}
    for {set j 0} {$j < $nc} {incr j} {
        lappend collines [gets $s]
    }
    set cells {}
    for {set k 0} {$k < $nr * $nc} {incr k} {
        set c [gets $s]
        if {$c eq "N"} {
            lappend cells [list N ""]
        } else {
            lappend cells [list C [read $s [lindex $c 1]]]
        }
    }
    if {[gets $s] ne "END"} {
        error "missing END"
    }
    list $nr $nc $collines $cells
}
proc cell {rows r c} {
    lassign $rows nr nc collines cells
    lindex $cells [expr {$r * $nc + $c}] 1
}
# Little-endian binary cell decoders (i = LE int32, q = LE double).
proc cint {bytes} {
    binary scan $bytes i v
    return $v
}
proc creal {bytes} {
    binary scan $bytes q v
    return $v
}

# ---- the session ----
set c [dial $port]
sendline $c "HELLO 1 run_sqrd.tcl"
set ok [gets $c]
check {[lrange $ok 0 2] eq "OK 1 sqrd"} "HELLO answered: $ok"
check {[lindex $ok end] eq $dbdir} "server names the database"

lassign [reply [sendsql $c "CREATE TABLE readings (id INTEGER, temp REAL, station CHAR(16), remark TEXT)"]] hdr payload
check {[lindex $hdr 0] eq "MSG"} "CREATE TABLE -> MSG"
lassign [reply [sendsql $c "CREATE UNIQUE INDEX ON readings (id)"]] hdr payload
check {[lindex $hdr 0] eq "MSG"} "CREATE UNIQUE INDEX -> MSG"

# doubles chosen to break any formatted round-trip that isn't exact
set temps {0.1 -1.5e-300 12345.678900000001 3.141592653589793}
set id 0
foreach t $temps {
    incr id
    lassign [reply [sendsql $c "INSERT INTO readings VALUES ($id, $t, 'S$id', 'note $id')"]] hdr payload
    check {$hdr eq "COUNT 1"} "INSERT $id -> COUNT 1"
}
lassign [reply [sendsql $c "INSERT INTO readings (id) VALUES (99)"]] hdr payload
check {$hdr eq "COUNT 1"} "partial INSERT (NULL row)"

sendsql $c "SELECT * FROM readings ORDER BY id"
set r [rows $c]
lassign $r nr nc collines cells
check {$nr == 5 && $nc == 4} "SELECT frames 5x4"
check {[lindex $collines 0] eq "COL id INT 4 null 0"} "INT column line"
check {[lindex $collines 1] eq "COL temp REAL 8 null 0"} "REAL column line"
set exact 1
set id 0
foreach t $temps {
    incr id
    if {[cint [cell $r [expr {$id - 1}] 0]] != $id} { set exact 0 }
    if {[creal [cell $r [expr {$id - 1}] 1]] != $t} { set exact 0 }
}
check $exact "binary INT and REAL cells are exact (incl. 0.1 and -1.5e-300)"
check {[cell $r 0 2] eq "S1"} "CHAR arrives trimmed"
check {[cell $r 0 3] eq "note 1"} "TEXT arrives verbatim"
lassign [lindex $cells [expr {4 * $nc + 1}]] kind bytes
check {$kind eq "N"} "NULL travels as bare N"

# write path: driver-style 17-significant-digit literal must round-trip
set v [expr {1.0 / 3.0}]
lassign [reply [sendsql $c "UPDATE readings SET temp = [format %.17g $v] WHERE id = 1"]] hdr payload
check {$hdr eq "COUNT 1"} "UPDATE with %.17g literal"
sendsql $c "SELECT temp FROM readings WHERE id = 1"
set r [rows $c]
check {[creal [cell $r 0 0]] == $v} "%.17g literal round-trips bit-exact"

# catalogue
sendline $c "TABLES"
set r [rows $c]
check {[lindex $r 0] == 1 && [cell $r 0 0] eq "readings"} "TABLES lists the table"
sendline $c "COLUMNS readings"
set r [rows $c]
lassign $r nr nc collines cells
check {$nr == 4 && $nc == 5} "COLUMNS is 4x5"
check {[cell $r 0 0] eq "id" && [cell $r 0 1] eq "INT" && [cint [cell $r 0 4]] == 1} \
    "id is the key (ordinal 1)"
check {[cint [cell $r 1 4]] == 0} "temp is not part of the key"

# errors and liveness
lassign [reply [sendsql $c "SELEC oops"]] hdr payload
check {[lindex $hdr 0] eq "ERR" && [string length $payload] > 0} "bad SQL -> ERR + message"
sendline $c "PING"
lassign [reply $c] hdr payload
check {$hdr eq "NONE"} "PING -> NONE"

# transactions + BUSY from a second connection
set c2 [dial $port]
sendline $c2 "HELLO 1 second"
gets $c2
lassign [reply [sendsql $c "BEGIN"]] hdr payload
check {[lindex $hdr 0] eq "MSG" && $payload eq "transaction started"} "BEGIN -> MSG"
lassign [reply [sendsql $c2 "DELETE FROM readings WHERE id = 1"]] hdr payload
check {[lrange $hdr 0 1] eq "ERR 100"} "cross-connection write -> ERR 100 (BUSY)"
lassign [reply [sendsql $c "ROLLBACK"]] hdr payload
check {[lindex $hdr 0] eq "MSG" && $payload eq "rolled back"} "ROLLBACK -> MSG"
sendline $c2 "QUIT"
lassign [reply $c2] hdr payload
check {$hdr eq "NONE"} "QUIT -> NONE"
close $c2

sendline $c "QUIT"
reply $c
close $c

# ---- shut down ----
exec kill [pid $srv]
catch {close $srv}
file delete -force $dbdir

puts "sqrd functional: $npass passed, $nfail failed"
exit [expr {$nfail > 0}]
