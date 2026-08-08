# corrupt.tcl — destruction testing for sqr.
#
# For every case: copy the pristine database, inject a deliberate defect into
# one on-disk artefact, then drive the whole public API over it with `hammer`.
# The engine is allowed to return any error it likes; it is NOT allowed to
# crash, abort, hang or run away.
#
#   tclsh corrupt.tcl <pristine-dir> [filter-glob]

set PRISTINE [lindex $argv 0]
if {$PRISTINE eq ""} { set PRISTINE pristine }
set FILTER [lindex $argv 1]
if {$FILTER eq ""} { set FILTER * }

set WORK   work
set KEEP   [expr {[info exists env(KEEPDIR)] ? $env(KEEPDIR) : "findings"}]
set HAMMER [expr {[info exists env(HAMMER)]  ? $env(HAMMER)  : "./hammer"}]
# Section 8's blind mutations are seeded so a run is reproducible; override
# DESTRUCT_SEED to sweep ground the default seed never reaches.
set SEED   [expr {[info exists env(DESTRUCT_SEED)] ? $env(DESTRUCT_SEED) : 20260806}]
# ASan reserves a huge shadow VA range, so `ulimit -v` must be off for it;
# hard_rss_limit_mb keeps a runaway allocation from touching real memory.
set ASAN   [info exists env(ASAN)]
set VLIMIT 4194304          ;# 4 GiB address space — a wild allocate fails, not swaps
set TMO    20
if {$ASAN} { set TMO 60 }

file mkdir $KEEP

# ---------- binary file helpers -------------------------------------------

proc readfile {p} {
    set f [open $p rb]
    set d [read $f]
    close $f
    return $d
}

proc writefile {p d} {
    set f [open $p wb]
    puts -nonewline $f $d
    close $f
}

proc patch {p off bytes} {
    set d [readfile $p]
    if {$off >= [string length $d]} { error "past-eof" }
    set n [string length $bytes]
    set d [string replace $d $off [expr {$off + $n - 1}] $bytes]
    writefile $p $d
}

proc readfile_range {p a b} {
    return [string range [readfile $p] $a $b]
}

proc i32 {v} { binary format i $v }
proc i64 {v} { binary format w $v }

proc p32 {p off v} { patch $p $off [i32 $v] }
proc p64 {p off v} { patch $p $off [i64 $v] }
proc p8  {p off v} { patch $p $off [binary format c $v] }

proc get32 {p off} {
    set d [readfile $p]
    binary scan [string range $d $off [expr {$off+3}]] i v
    return $v
}

proc truncate_to {p n} {
    set d [readfile $p]
    writefile $p [string range $d 0 [expr {$n - 1}]]
}

proc append_junk {p n} {
    set d [readfile $p]
    writefile $p $d[string repeat "\xA5\x5A\xFF\x00" [expr {($n+3)/4}]]
}

# sqr's rolling payload checksum (sqr_base::checksum)
proc sqr_checksum {buf} {
    set acc 0
    binary scan $buf c* bytes
    foreach b $bytes {
        set acc [expr {($acc * 31 + ($b & 0xFF)) % 2147483647}]
    }
    return $acc
}

# ---------- case runner ----------------------------------------------------

set ::ncase 0
set ::nbad  0
set ::nskip 0
set ::summary {}
array set ::verdicts {}

proc run_case {name script {mode rw}} {
    global PRISTINE WORK KEEP HAMMER VLIMIT TMO FILTER
    if {![string match $FILTER $name]} return
    incr ::ncase
    file delete -force $WORK $WORK.pack $WORK.unpacked
    exec cp -a $PRISTINE $WORK
    if {[catch {uplevel 1 [list eval $script]} err]} {
        incr ::nskip
        if {$err ne "past-eof"} { puts "SETUP-FAIL $name: $err" }
        return
    }
    set arg [expr {$mode eq "ro" ? "ro" : ""}]
    if {$::ASAN} {
        set cmd "ulimit -c 0; ASAN_OPTIONS=detect_leaks=0:hard_rss_limit_mb=3000 exec timeout $TMO $HAMMER $WORK $arg"
    } else {
        set cmd "ulimit -v $VLIMIT; ulimit -c 0; exec timeout $TMO $HAMMER $WORK $arg"
    }
    set rc 0
    set out ""
    if {[catch {exec sh -c $cmd 2>@1} out]} {
        set ec $::errorCode
        if {[lindex $ec 0] eq "CHILDSTATUS"} {
            set rc [lindex $ec 2]
        } elseif {[lindex $ec 0] eq "CHILDKILLED"} {
            set rc "SIG[lindex $ec 2]"
        } else {
            set rc "ERR:$ec"
        }
    }
    set verdict ok
    if {$rc == 0} {
        if {![string match "*SURVIVED*" $out]} { set verdict "no-survive-marker" }
    } elseif {$rc == 3} {
        set verdict runaway
    } elseif {$rc == 124} {
        set verdict HANG
    } elseif {[string match "SIG*" $rc]} {
        set verdict "CRASH-$rc"
    } else {
        set verdict "ABORT-rc$rc"
    }
    set ::verdicts($name) $verdict
    if {$verdict ne "ok"} {
        incr ::nbad
        puts "!! $verdict  $name"
        set tail [lrange [split [string trim $out] "\n"] end-14 end]
        puts "     [join $tail "\n     "]"
        set d [file join $KEEP $name]
        file delete -force $d
        catch {exec cp -a $WORK $d}
        writefile [file join $KEEP $name.log] $out
        lappend ::summary [list $name $verdict]
    }
}

# ==========================================================================
# Layout constants of the pristine victim database
# ==========================================================================
# _catalog.dat : magic(4) bom(4) ver(4) ntables(4) then ntables*32-byte names
# <t>.schema   : magic(4) bom(4) ver(4) ncols(4) recsize(4) next_id(4)
#                live(4) nindices(4) | per col: name(32) dtype(4) csize(4) off(4)
#                | per index: ncols(4) names(32*n) key_size(4) unique(4)
# <t>__iN.idx  : page 1 meta: magic(4) bom(4) fmt(4) page_size(4) key_len(4)
#                root(4) free(4) npages(4) first_leaf(4) nentries(8)
#                body page: kind(1) nkeys(4) [leaf: next(4) entries...]
#                                            [int: children(4*)... seps...]
# people.dat   : recl 58; byte0 status, byte1 null bitmap, then columns;
#                note TEXT descriptor at 1-based 47 => off64@46, len32@54

set WILD {0 1 -1 2147483647 -2147483648 65536 1000000 1000000000 7}

# ---------- 1. catalog -----------------------------------------------------

foreach {fld off} {magic 0 bom 4 ver 8 ntables 12} {
    foreach v $WILD {
        run_case "cat-$fld-$v" [list p32 $WORK/_catalog.dat $off $v]
    }
}
run_case cat-magic-text   {patch $WORK/_catalog.dat 0 "XXXX"}
run_case cat-name-nul     {patch $WORK/_catalog.dat 16 [string repeat "\x00" 32]}
run_case cat-name-slash   {patch $WORK/_catalog.dat 16 [format %-32s "../../etc/x"]}
run_case cat-name-dotdot  {patch $WORK/_catalog.dat 16 [format %-32s ".."]}
run_case cat-name-abs     {patch $WORK/_catalog.dat 16 [format %-32s "/tmp/zzz"]}
run_case cat-name-ctrl    {patch $WORK/_catalog.dat 16 [string repeat "\x01" 32]}
run_case cat-name-high    {patch $WORK/_catalog.dat 16 [string repeat "\xFF" 32]}
run_case cat-name-dup     {patch $WORK/_catalog.dat 48 [readfile_range $WORK/_catalog.dat 16 47]}
run_case cat-truncated    {truncate_to $WORK/_catalog.dat 20}
run_case cat-empty        {writefile $WORK/_catalog.dat ""}
run_case cat-huge         {append_junk $WORK/_catalog.dat 4096}
run_case cat-deleted      {file delete $WORK/_catalog.dat}

# ---------- 2. schema headers ---------------------------------------------

foreach t {people events} {
    foreach {fld off} {magic 0 bom 4 ver 8 ncols 12 recsize 16 next_id 20 live 24 nindices 28} {
        foreach v $WILD {
            run_case "sch-$t-$fld-$v" [list p32 $WORK/$t.schema $off $v]
        }
    }
    run_case "sch-$t-truncated"  [list truncate_to $WORK/$t.schema 40]
    run_case "sch-$t-trunc-mid"  [list truncate_to $WORK/$t.schema 200]
    run_case "sch-$t-empty"      [list writefile   $WORK/$t.schema ""]
    run_case "sch-$t-deleted"    [list file delete $WORK/$t.schema]
    run_case "sch-$t-junk"       [list writefile   $WORK/$t.schema [string repeat "\xFF" 400]]
}

# per-column fields of people.schema (5 columns, 44 bytes each from offset 32)
for {set c 0} {$c < 5} {incr c} {
    set base [expr {32 + 44*$c}]
    foreach {fld d} [list name 0 dtype 32 csize 36 offset 40] {
        foreach v {0 -1 2147483647 5 99 3 8 65537} {
            if {$fld eq "name"} continue
            run_case "sch-col$c-$fld-$v" [list p32 $WORK/people.schema [expr {$base+$d}] $v]
        }
    }
    run_case "sch-col$c-name-empty" [list patch $WORK/people.schema $base [string repeat "\x00" 32]]
    run_case "sch-col$c-name-dup"   [list patch $WORK/people.schema $base [format %-32s "id"]]
    run_case "sch-col$c-name-path"  [list patch $WORK/people.schema $base [format %-32s "../x"]]
}

# index records of people.schema start after the columns
set IXBASE [expr {32 + 44*5}]
foreach v {0 -1 2 5 2147483647 1000000} {
    run_case "sch-ix0-ncols-$v"  [list p32 $WORK/people.schema $IXBASE $v]
}
# index 0 is single-column: ncols(4) name(32) key_size(4) unique(4)
foreach v {0 -1 3 2147483647 1000000 65536} {
    run_case "sch-ix0-keysize-$v" [list p32 $WORK/people.schema [expr {$IXBASE+36}] $v]
}
run_case sch-ix0-unique-on   [list p32 $WORK/people.schema [expr {$IXBASE+40}] 1]
run_case sch-ix0-member-bad  [list patch $WORK/people.schema [expr {$IXBASE+4}] [format %-32s "nosuch"]]
run_case sch-ix0-member-text [list patch $WORK/people.schema [expr {$IXBASE+4}] [format %-32s "note"]]
run_case sch-ix0-member-nul  [list patch $WORK/people.schema [expr {$IXBASE+4}] [string repeat "\x00" 32]]

# ---------- 3. B+-tree index files ----------------------------------------

foreach ix {people__i1 people__i2 people__i3 events__i1} {
    foreach {fld off} {magic 0 bom 4 fmt 8 page_size 12 key_len 16 root 20 free 24 npages 28 first_leaf 32} {
        foreach v {0 1 -1 2 3 63 64 4095 4097 2147483647 1000000} {
            run_case "idx-$ix-$fld-$v" [list p32 $WORK/$ix.idx $off $v]
        }
    }
    foreach v {-1 2147483647 1000000000} {
        run_case "idx-$ix-nentries-$v" [list p64 $WORK/$ix.idx 36 $v]
    }
    run_case "idx-$ix-truncated"  [list truncate_to $WORK/$ix.idx 4096]
    run_case "idx-$ix-trunc-half" [list truncate_to $WORK/$ix.idx 2048]
    run_case "idx-$ix-empty"      [list writefile   $WORK/$ix.idx ""]
    run_case "idx-$ix-deleted"    [list file delete $WORK/$ix.idx]
    run_case "idx-$ix-zero"       [list writefile   $WORK/$ix.idx [string repeat "\x00" 16384]]

    # body pages: kind byte, nkeys, child/next pointers
    foreach pg {2 3 4} {
        set b [expr {($pg-1)*4096}]
        run_case "idx-$ix-p$pg-kind0"   [list p8  $WORK/$ix.idx $b 0]
        run_case "idx-$ix-p$pg-kind2"   [list p8  $WORK/$ix.idx $b 2]
        run_case "idx-$ix-p$pg-kind1"   [list p8  $WORK/$ix.idx $b 1]
        run_case "idx-$ix-p$pg-kind99"  [list p8  $WORK/$ix.idx $b 99]
        foreach v {-1 0 1 2147483647 1000000 100000 340 500} {
            run_case "idx-$ix-p$pg-nkeys-$v" [list p32 $WORK/$ix.idx [expr {$b+1}] $v]
        }
        # leaf next-pointer / first child pointer (same slot)
        foreach v {-1 0 1 2 2147483647 1000000} {
            run_case "idx-$ix-p$pg-ptr-$v"   [list p32 $WORK/$ix.idx [expr {$b+5}] $v]
        }
        run_case "idx-$ix-p$pg-selfloop"    [list p32 $WORK/$ix.idx [expr {$b+5}] $pg]
        run_case "idx-$ix-p$pg-child2-wild" [list p32 $WORK/$ix.idx [expr {$b+9}] 2147483647]
        run_case "idx-$ix-p$pg-garbage"     [list patch $WORK/$ix.idx $b [string repeat "\xFF" 512]]
    }
}

# ---------- 4. data records ------------------------------------------------

set RECL 58
foreach r {1 2 3 100 599 600} {
    set b [expr {($r-1)*$RECL}]
    foreach v {0 1 2 3 127 -128 99} {
        run_case "dat-r$r-status-$v" [list p8 $WORK/people.dat $b $v]
    }
    run_case "dat-r$r-nullmask-ff" [list p8 $WORK/people.dat [expr {$b+1}] -1]
    # note TEXT descriptor: int64 offset at +46, int32 length at +54
    foreach v {0 -1 2147483647 1000000000} {
        run_case "dat-r$r-textoff-$v" [list p64 $WORK/people.dat [expr {$b+46}] $v]
        run_case "dat-r$r-textlen-$v" [list p32 $WORK/people.dat [expr {$b+54}] $v]
    }
    run_case "dat-r$r-textoff-huge64" [list p64 $WORK/people.dat [expr {$b+46}] 9223372036854775807]
    run_case "dat-r$r-textlen-neg"    [list p32 $WORK/people.dat [expr {$b+54}] -2147483648]
    run_case "dat-r$r-allff"          [list patch $WORK/people.dat $b [string repeat "\xFF" $RECL]]
}
run_case dat-truncated    {truncate_to $WORK/people.dat 1000}
run_case dat-trunc-partial {truncate_to $WORK/people.dat 34790}
run_case dat-empty        {writefile $WORK/people.dat ""}
run_case dat-deleted      {file delete $WORK/people.dat}
run_case dat-extended     {append_junk $WORK/people.dat 4096}

# ---------- 5. blob --------------------------------------------------------

run_case blob-empty      {writefile $WORK/people.blob ""}
run_case blob-truncated  {truncate_to $WORK/people.blob 100}
run_case blob-deleted    {file delete $WORK/people.blob}
run_case blob-extended   {append_junk $WORK/people.blob 65536}
run_case blob-nulls      {writefile $WORK/people.blob [string repeat "\x00" 1023]}

# ---------- 6. lock --------------------------------------------------------

run_case lock-deleted    {file delete $WORK/_lock}
run_case lock-junk       {writefile $WORK/_lock [string repeat "Z" 4096]}
run_case lock-dir        {file delete $WORK/_lock; file mkdir $WORK/_lock}

# ---------- 7. crafted hot journals ---------------------------------------
# header: magic(4) fmt(4) state(4) nrec(4) cksum(4) plen(8), payload at 64.
# record: kind(4) pathlen(4) path orig_len(8) offset(8) length(8) byteslen(8) bytes

proc jrec {kind path orig off len bytes} {
    set r [i32 $kind]
    append r [i32 [string length $path]] $path
    append r [i64 $orig] [i64 $off] [i64 $len] [i64 [string length $bytes]] $bytes
    return $r
}

# A record with an arbitrary (possibly lying) pathlen / byteslen field.
proc jrec_raw {kind pathlen path orig off len byteslen bytes} {
    set r [i32 $kind]
    append r [i32 $pathlen] $path
    append r [i64 $orig] [i64 $off] [i64 $len] [i64 $byteslen] $bytes
    return $r
}

proc jwrite {dir nrec payload {state 1} {fmt 1} {ck ""} {plen ""}} {
    if {$ck eq ""}   { set ck   [sqr_checksum $payload] }
    if {$plen eq ""} { set plen [string length $payload] }
    set hdr SQRJ
    append hdr [i32 $fmt] [i32 $state] [i32 $nrec] [i32 $ck] [i64 $plen]
    append hdr [string repeat "\x00" [expr {64 - [string length $hdr]}]]
    writefile [file join $dir _journal.dat] $hdr$payload
}

run_case jrn-empty-hot        {jwrite $WORK 0 {}}
run_case jrn-nrec-huge        {jwrite $WORK 2147483647 {}}
run_case jrn-nrec-1e9         {jwrite $WORK 1000000000 {}}
run_case jrn-nrec-1e6         {jwrite $WORK 1000000 {}}
run_case jrn-nrec-1e5         {jwrite $WORK 100000 {}}
run_case jrn-nrec-neg         {jwrite $WORK -1 {}}
run_case jrn-plen-huge        {jwrite $WORK 1 {} 1 1 {} 2147483647}
run_case jrn-plen-neg         {jwrite $WORK 1 {} 1 1 {} -1}
run_case jrn-plen-64bit       {jwrite $WORK 1 {} 1 1 {} 9223372036854775807}
run_case jrn-badcksum         {jwrite $WORK 1 [jrec 1 people.dat 34800 1 4 ABCD] 1 1 12345}
run_case jrn-pathlen-huge     {jwrite $WORK 1 [jrec_raw 1 2147483647 x 0 0 0 0 {}]}
run_case jrn-pathlen-big      {jwrite $WORK 1 [jrec_raw 1 1000000 x 0 0 0 0 {}]}
run_case jrn-pathlen-neg      {jwrite $WORK 1 [jrec_raw 1 -1 x 0 0 0 0 {}]}
run_case jrn-pathlen-max      {jwrite $WORK 1 [jrec_raw 1 2147483640 x 0 0 0 0 {}]}
run_case jrn-byteslen-huge    {jwrite $WORK 1 [jrec_raw 1 4 abcd 0 0 0 2147483647 {}]}
run_case jrn-byteslen-neg     {jwrite $WORK 1 [jrec_raw 1 4 abcd 0 0 0 -1 {}]}
run_case jrn-byteslen-64max   {jwrite $WORK 1 [jrec_raw 1 4 abcd 0 0 0 9223372036854775807 {}]}
run_case jrn-truncated-rec    {jwrite $WORK 3 [jrec 1 people.dat 34800 1 4 ABCD]}
run_case jrn-path-traversal   {jwrite $WORK 1 [jrec 1 ../../ESCAPED 0 1 4 PWND]}
run_case jrn-path-absolute    {jwrite $WORK 1 [jrec 1 /tmp/sqr-escape-test 0 1 4 PWND]}
run_case jrn-path-nul         {jwrite $WORK 1 [jrec 1 "people.dat\x00zz" 34800 1 4 ABCD]}
run_case jrn-region-wildoff   {jwrite $WORK 1 [jrec 1 people.dat 34800 9223372036854775000 4 ABCD]}
run_case jrn-region-negoff    {jwrite $WORK 1 [jrec 1 people.dat 34800 -4096 4 ABCD]}
run_case jrn-region-bigoff    {jwrite $WORK 1 [jrec 1 people.dat 34800 1000000000 4 ABCD]}
run_case jrn-extend-neg       {jwrite $WORK 1 [jrec 2 people.dat -1 0 0 {}]}
run_case jrn-extend-zero      {jwrite $WORK 1 [jrec 2 people.dat 0 0 0 {}]}
run_case jrn-extend-idx       {jwrite $WORK 1 [jrec 2 people__i1.idx 0 0 0 {}]}
run_case jrn-extend-sch       {jwrite $WORK 1 [jrec 2 people.schema 0 0 0 {}]}
run_case jrn-kind-wild        {jwrite $WORK 1 [jrec 99 people.dat 34800 1 4 ABCD]}
run_case jrn-many-recs        {
    set p {}
    for {set i 0} {$i < 500} {incr i} { append p [jrec 1 people.dat 34800 1 4 ABCD] }
    jwrite $WORK 500 $p
}
run_case jrn-junk             {writefile $WORK/_journal.dat [string repeat "\xFF" 4096]}
run_case jrn-short            {writefile $WORK/_journal.dat "SQRJ"}
run_case jrn-ro-hot           {jwrite $WORK 1 [jrec 1 people.dat 34800 1 4 ABCD]} ro

# ---------- 8. randomised mutation ----------------------------------------
# Blind bit-flips, byte-runs and truncations across every artefact — the
# targeted cases above only cover fields I thought of.

expr {srand($SEED)}

proc rand_int {n} { return [expr {int(rand()*$n)}] }

set FILES {_catalog.dat people.schema events.schema people.dat people.blob
           people__i1.idx people__i2.idx people__i3.idx events__i1.idx}

foreach f $FILES {
    for {set i 0} {$i < 60} {incr i} {
        run_case "rnd-flip-$f-$i" [format {
            set d [readfile %s]
            set n [string length $d]
            for {set k 0} {$k < 3} {incr k} {
                set o [rand_int $n]
                binary scan [string index $d $o] c b
                set d [string replace $d $o $o [binary format c [expr {($b ^ (1 << [rand_int 8])) & 0xFF}]]]
            }
            writefile %s $d
        } $WORK/$f $WORK/$f]
    }
    for {set i 0} {$i < 12} {incr i} {
        run_case "rnd-run-$f-$i" [format {
            set d [readfile %s]
            set n [string length $d]
            set o [rand_int $n]
            set l [expr {1 + [rand_int 64]}]
            set d [string replace $d $o [expr {$o+$l-1}] [string repeat "\xFF" $l]]
            writefile %s $d
        } $WORK/$f $WORK/$f]
    }
    for {set i 0} {$i < 8} {incr i} {
        run_case "rnd-trunc-$f-$i" [format {
            set d [readfile %s]
            writefile %s [string range $d 0 [expr {[rand_int [string length $d]] - 1}]]
        } $WORK/$f $WORK/$f]
    }
}

puts ""
puts "=== $::ncase cases run, $::nskip skipped, $::nbad failures ==="
foreach s $::summary { puts "   [lindex $s 1]  [lindex $s 0]" }
