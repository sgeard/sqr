# corrupt_pack.tcl — destruction testing of the .sqr single-file container.
#
# Container layout:
#   "SQRP" | int32 version | int32 BOM | int32 nfiles | int32 checksum
#   nfiles x [int32 namelen | name | int64 size | int64 offset]
#   payload
#
#   tclsh corrupt_pack.tcl <pristine.sqr> [filter]

set PACK  [lindex $argv 0]
set FILTER [lindex $argv 1]
if {$FILTER eq ""} { set FILTER * }

set KEEP   [expr {[info exists env(KEEPDIR)] ? $env(KEEPDIR) : "findings-pack"}]
set PROBE  [expr {[info exists env(PROBE)]   ? $env(PROBE)   : "./unpack_probe"}]
set VLIMIT 4194304
set TMO    20
file mkdir $KEEP

proc readfile {p} { set f [open $p rb]; set d [read $f]; close $f; return $d }
proc writefile {p d} { set f [open $p wb]; puts -nonewline $f $d; close $f }
proc i32 {v} { binary format i $v }
proc i64 {v} { binary format w $v }

proc patch {p off bytes} {
    set d [readfile $p]
    if {$off >= [string length $d]} { error "past-eof" }
    set n [string length $bytes]
    writefile $p [string replace $d $off [expr {$off + $n - 1}] $bytes]
}
proc p32 {p off v} { patch $p $off [i32 $v] }
proc p64 {p off v} { patch $p $off [i64 $v] }

set ::ncase 0
set ::nbad 0
set ::nskip 0
set ::summary {}

proc run_case {name script} {
    global PACK KEEP PROBE VLIMIT TMO FILTER
    if {![string match $FILTER $name]} return
    incr ::ncase
    file delete -force wpack.sqr wpackdir
    file copy $PACK wpack.sqr
    if {[catch {uplevel 1 [list eval $script]} err]} {
        incr ::nskip
        if {$err ne "past-eof"} { puts "SETUP-FAIL $name: $err" }
        return
    }
    set cmd "ulimit -v $VLIMIT; ulimit -c 0; exec timeout $TMO $PROBE wpack.sqr wpackdir"
    set rc 0
    if {[catch {exec sh -c $cmd 2>@1} out]} {
        set ec $::errorCode
        if {[lindex $ec 0] eq "CHILDSTATUS"} { set rc [lindex $ec 2] } \
        elseif {[lindex $ec 0] eq "CHILDKILLED"} { set rc "SIG[lindex $ec 2]" } \
        else { set rc "ERR:$ec" }
    } else { set out $out }
    set verdict ok
    if {$rc == 0} {
        if {![string match "*SURVIVED*" $out]} { set verdict no-survive-marker }
    } elseif {$rc == 124} { set verdict HANG
    } elseif {[string match "SIG*" $rc]} { set verdict "CRASH-$rc"
    } else { set verdict "ABORT-rc$rc" }
    if {$verdict ne "ok"} {
        incr ::nbad
        puts "!! $verdict  $name"
        puts "     [join [lrange [split [string trim $out] "\n"] 0 8] "\n     "]"
        file copy -force wpack.sqr [file join $KEEP $name.sqr]
        writefile [file join $KEEP $name.log] $out
        lappend ::summary [list $name $verdict]
    }
}

set WILD {0 1 -1 2147483647 -2147483648 1000000 1000001 100000000}

# header fields
foreach {fld off} {magic 0 version 4 bom 8 nfiles 12 cksum 16} {
    foreach v $WILD { run_case "pk-$fld-$v" [list p32 wpack.sqr $off $v] }
}
run_case pk-magic-text {patch wpack.sqr 0 "ZZZZ"}

# first TOC entry: namelen @20, name, size, offset
foreach v {0 -1 1 4096 4097 2147483647 1000000} {
    run_case "pk-namelen-$v" [list p32 wpack.sqr 20 $v]
}
run_case pk-name-traversal {patch wpack.sqr 24 "../../ESC"}
run_case pk-name-absolute  {patch wpack.sqr 24 "/tmp/esc0"}
run_case pk-name-nul       {patch wpack.sqr 24 "\x00\x00\x00\x00\x00\x00\x00\x00\x00"}
run_case pk-name-dotdot    {patch wpack.sqr 24 "..x/y/zzz"}

# size / offset of the first entry sit right after the name; the pristine first
# name is "_catalog.dat" (12 bytes) -> size @36, offset @44
foreach v {0 -1 2147483647 1000000000 9223372036854775807 -9223372036854775808} {
    run_case "pk-size-$v"   [list p64 wpack.sqr 36 $v]
    run_case "pk-offset-$v" [list p64 wpack.sqr 44 $v]
}

run_case pk-truncated-hdr   {writefile wpack.sqr [string range [readfile wpack.sqr] 0 9]}
run_case pk-truncated-toc   {writefile wpack.sqr [string range [readfile wpack.sqr] 0 60]}
run_case pk-truncated-body  {writefile wpack.sqr [string range [readfile wpack.sqr] 0 5000]}
run_case pk-empty           {writefile wpack.sqr ""}
run_case pk-junk            {writefile wpack.sqr [string repeat "\xFF" 8192]}
run_case pk-extended        {writefile wpack.sqr [readfile wpack.sqr][string repeat "\xA5" 4096]}

expr {srand([expr {[info exists env(DESTRUCT_SEED)] ? $env(DESTRUCT_SEED) : 20260806}])}
proc rand_int {n} { return [expr {int(rand()*$n)}] }
for {set i 0} {$i < 300} {incr i} {
    run_case "pk-rnd-$i" {
        set d [readfile wpack.sqr]
        set n [string length $d]
        for {set k 0} {$k < 3} {incr k} {
            set o [rand_int $n]
            binary scan [string index $d $o] c b
            set d [string replace $d $o $o [binary format c [expr {($b ^ (1 << [rand_int 8])) & 0xFF}]]]
        }
        writefile wpack.sqr $d
    }
}

puts ""
puts "=== pack: $::ncase cases, $::nskip skipped, $::nbad failures ==="
foreach s $::summary { puts "   [lindex $s 1]  [lindex $s 0]" }
