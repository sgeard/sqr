# sqlfuzz.tcl — destruction testing of the SQL front-end (lexer/parser/executor)
# and the sqlsh meta commands.  Bad *text* rather than bad bytes on disk.
#
#   tclsh sqlfuzz.tcl <sqlsh-binary> <pristine-db> [filter]

set SH   [lindex $argv 0]
set PRIS [lindex $argv 1]
set FILTER [lindex $argv 2]
if {$FILTER eq ""} { set FILTER * }

set KEEP   [expr {[info exists env(KEEPDIR)] ? $env(KEEPDIR) : "findings-sql"}]
set VLIMIT 4194304
set TMO    20
file mkdir $KEEP

proc writefile {p d} { set f [open $p wb]; puts -nonewline $f $d; close $f }

set ::ncase 0
set ::nbad 0
set ::summary {}

proc run_sql {name script} {
    global SH PRIS KEEP VLIMIT TMO FILTER
    if {![string match $FILTER $name]} return
    incr ::ncase
    file delete -force sqlwork sqlwork.pack sqlwork.unpacked
    exec cp -a $PRIS sqlwork
    writefile sqlin.sql "$script\n"
    set cmd "ulimit -v $VLIMIT; ulimit -c 0; exec timeout $TMO $SH sqlwork < sqlin.sql"
    set rc 0
    if {[catch {exec sh -c $cmd 2>@1} out]} {
        set ec $::errorCode
        if {[lindex $ec 0] eq "CHILDSTATUS"} { set rc [lindex $ec 2] } \
        elseif {[lindex $ec 0] eq "CHILDKILLED"} { set rc "SIG[lindex $ec 2]" } \
        else { set rc "ERR:$ec" }
    }
    set verdict ok
    if {$rc == 124} { set verdict HANG } \
    elseif {[string match "SIG*" $rc]} { set verdict "CRASH-$rc" } \
    elseif {$rc != 0} { set verdict "ABORT-rc$rc" }
    if {$verdict ne "ok"} {
        incr ::nbad
        puts "!! $verdict  $name"
        puts "     input: [string range $script 0 160]"
        puts "     [join [lrange [split [string trim $out] "\n"] 0 6] "\n     "]"
        writefile [file join $KEEP $name.sql] $script
        writefile [file join $KEEP $name.log] $out
        lappend ::summary [list $name $verdict]
    }
}

# ---------- handcrafted nasties -------------------------------------------

set N 0
proc nasty {s} { run_sql "sql-[incr ::N]" $s }

# lexer
nasty "select * from people where name = 'unterminated"
nasty "select * from people where name = \"unterminated"
nasty "select * from people where name = ''''''''''"
nasty "select * from people where name = '\\'"
nasty "select * from people where name = '[string repeat A 100000]'"
nasty "select [string repeat x 100000] from people"
nasty "select * from [string repeat t 100000]"
nasty [string repeat "select * from people; " 2000]
nasty "select * from people where id = [string repeat 9 400]"
nasty "select * from people where id = -[string repeat 9 400]"
nasty "select * from people where score = 1e[string repeat 9 200]"
nasty "select * from people where score = 1.[string repeat 0 5000]1"
nasty "select * from people where score = .e."
nasty "select * from people where score = 1e"
nasty "select * from people where score = 1e+"
nasty "select * from people where id = 2147483648"
nasty "select * from people where id = -2147483649"
nasty "select * from people where id = 99999999999999999999999"
nasty "select * from people where id = 0x41"
nasty "select * from people where id = \x00\x01\x02"
nasty "select\t*\tfrom\tpeople\twhere\tid\t=\t1"
nasty "SELECT * FROM PEOPLE WHERE ID = 1"
nasty "\x00select * from people"
nasty "select * from people\x00; drop table people"
nasty "select * from people where name = 'a\x00b'"
nasty [string repeat " " 100000]
nasty [string repeat ";" 10000]
nasty [string repeat "(" 5000]
nasty "select * from people where [string repeat "(" 2000]id=1[string repeat ")" 2000]"
nasty "select * from people where [string repeat "not " 5000]id=1"
nasty "select * from people where id=1[string repeat " and id=1" 5000]"
nasty "select * from people where id=1[string repeat " or id=1" 5000]"

# parser / grammar
nasty "select"
nasty "select from"
nasty "select * from"
nasty "select * from where"
nasty "select , from people"
nasty "select *, from people"
nasty "select * from people where"
nasty "select * from people where id"
nasty "select * from people where id ="
nasty "select * from people where = 1"
nasty "select * from people order by"
nasty "select * from people order by 999999"
nasty "select * from people order by -1"
nasty "select * from people order by nosuch"
nasty "select * from people limit"
nasty "select * from people limit -1"
nasty "select * from people limit 2147483647"
nasty "select * from people limit 99999999999999999999"
nasty "select * from people limit 1 limit 2"
nasty "select * from people where id between"
nasty "select * from people where id between 1"
nasty "select * from people where id between 1 and"
nasty "select * from people where id between 5 and 1"
nasty "select * from people where id is"
nasty "select * from people where id is not"
nasty "select * from people where score is null and"
nasty "select nosuchcol from people"
nasty "select id, id, id, id, id, id from people"
nasty "select * from nosuchtable"
nasty "select * from people p"

# DDL
nasty "create table"
nasty "create table t"
nasty "create table t ()"
nasty "create table t (a)"
nasty "create table t (a nosuchtype)"
nasty "create table t (a char(0))"
nasty "create table t (a char(-1))"
nasty "create table t (a char(2147483647))"
nasty "create table t (a char(99999999999999999999))"
nasty "create table t (a char())"
nasty "create table t (a char(1x))"
nasty "create table t (a int, a int)"
nasty "create table t ([string repeat c 100000] int)"
nasty "create table [string repeat t 100000] (a int)"
nasty "create table t (a int)\ncreate table t (a int)"
nasty "create table ../../escape (a int)"
nasty "create table \"../../escape\" (a int)"
nasty "create table t (a int, b int, c int, d int, e int, f int, g int, h int)\ninsert into t values (1,2,3,4,5,6,7,8)\nselect * from t"
set many {}
for {set i 0} {$i < 500} {incr i} { lappend many "c$i int" }
nasty "create table wide ([join $many ,])"
nasty "drop table"
nasty "drop table nosuch"
nasty "drop table people\nselect * from people"
nasty "create index"
nasty "create index on people"
nasty "create index i on people (nosuch)"
nasty "create index i on people (note)"
nasty "create index i on people ()"
nasty "create index i on people (id,id,id,id,id,id,id,id)"
nasty "drop index i on nosuch"

# DML
nasty "insert into people values"
nasty "insert into people values ()"
nasty "insert into people values (1)"
nasty "insert into people values (1,2,3,4,5,6,7,8,9,10)"
nasty "insert into people (id) values ('notanumber')"
nasty "insert into people (nosuch) values (1)"
nasty "insert into people (id) values ([string repeat 9 100])"
nasty "insert into people (name) values ('[string repeat Z 100000]')"
nasty "insert into people (note) values ('[string repeat Z 200000]')"
nasty "update people set"
nasty "update people set id"
nasty "update people set id ="
nasty "update people set nosuch = 1"
nasty "update people set id = 1 where"
nasty "update people set id = 'x'"
nasty "delete"
nasty "delete from"
nasty "delete from nosuch"
nasty "delete from people where"
nasty "delete from people"

# transactions
nasty "begin\nbegin\ncommit\ncommit"
nasty "commit"
nasty "rollback"
nasty "begin\ndrop table people\nrollback\nselect * from people"
nasty [string repeat "begin\n" 2000]

# meta commands
nasty ".open"
nasty ".open /nonexistent/path/deep/deeper"
nasty ".open ../../../../etc"
nasty ".open [string repeat a 100000]"
nasty ".close\n.close\n.tables"
nasty ".schema"
nasty ".schema nosuch"
nasty ".schema [string repeat s 100000]"
nasty ".pack"
nasty ".pack /nonexistent/dir/x.sqr"
nasty ".unpack"
nasty ".unpack /nonexistent.sqr /tmp/nowhere-out"
nasty ".unpack sqlin.sql /tmp/sqlfuzz-unpack-out"
nasty ".nosuchcommand"
nasty "."
nasty ".."
nasty ".[string repeat x 100000]"

# ---------- randomised token soup -----------------------------------------

expr {srand([expr {[info exists env(DESTRUCT_SEED)] ? $env(DESTRUCT_SEED) : 20260806}])}
proc rand_int {n} { return [expr {int(rand()*$n)}] }
proc pick {l} { return [lindex $l [rand_int [llength $l]]] }

set TOKENS {}
foreach tk {select from where insert into values update set delete create table
            drop index order by limit between and or not is null asc desc
            people events id name score tag note eid who val * , ( ) ; = < > <= >= <>
            + - . 0 1 -1 2147483647 1.5 int integer real text char t} {
    lappend TOKENS $tk
}
lappend TOKENS "'" "\"" "'x'" "'unclosed" "\x00"

for {set i 0} {$i < 400} {incr i} {
    set n [expr {2 + [rand_int 24]}]
    set s {}
    for {set k 0} {$k < $n} {incr k} { lappend s [pick $TOKENS] }
    run_sql "soup-$i" [join $s " "]
}

# raw byte garbage lines
for {set i 0} {$i < 150} {incr i} {
    set n [expr {1 + [rand_int 200]}]
    set s ""
    for {set k 0} {$k < $n} {incr k} {
        append s [binary format c [expr {1 + [rand_int 255]}]]
    }
    run_sql "bytes-$i" $s
}

puts ""
puts "=== sql: $::ncase cases, $::nbad failures ==="
foreach s $::summary { puts "   [lindex $s 1]  [lindex $s 0]" }
