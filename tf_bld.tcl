#!/usr/bin/env tclsh9.1
# SPDX-License-Identifier: MIT
# Copyright (c) 2026 Simon Geard
#
# tf_bld - scan free-form Fortran sources and write depends.mk (see DESIGN.md)
#
#   tclsh9.1 tf_bld.tcl -o depends.mk [options] FILE.f90 ... [@ARGFILE ...]
#
# The Makefile names every source; tf_bld scans those and nothing else.
# Pipeline, one namespace per stage, each knowing nothing of the later ones:
#   fsource::reader   physical lines, with include files spliced in
#   fsource::joiner   logical statements (continuation, ';', comments, strings)
#   tf_bld::dispatch  keyword table: module, submodule, use, program, end
#   tf_bld::graph     providers, edges, cycle checks, entry-point walks
#   tf_bld::writer    the text of depends.mk

package require Tcl 8.6-

# =============================================================================
# fsource: free-form source as logical statements; no build knowledge at all

namespace eval fsource {
    # Raise a source error; message "where: text"
    proc fail {where msg} {
        return -code error -errorcode FSOURCE "$where: $msg"
    }

    # A path without a leading "./", as glob and file join can produce
    proc tidy {path} {
        regsub {^(?:\./)+} $path {} path
        return $path
    }
}

# -----------------------------------------------------------------------------
# Reader: physical lines (DESIGN.md §4)

namespace eval fsource::reader {
    # An include line: the keyword in any case, a quoted name, optional comment
    variable include_re {(?i)^\s*include\s*(?:'([^']*)'|"([^"]*)")\s*(?:!.*)?$}
    # A preprocessor directive that opens or closes a conditional
    variable cond_re {^\s*#\s*(ifdef|ifndef|if|endif)\M}
}

# Read a source file, splicing in the files its include lines name.
# Returns a dict:
#   lines     {{text file line cond} ...}: every physical line with its origin;
#             cond is {} or {DIRECTIVE file:line}, the innermost open
#             preprocessor conditional (#if, #ifdef, #ifndef) holding the line
#   includes  the files included, directly or not, each once in the order met
#   warnings  include lines inside a conditional: their files are always read
proc fsource::reader::read_source {path incdirs} {
    set lines {}
    set includes [dict create]
    set warnings {}
    splice lines includes warnings $path $incdirs {} {}
    return [dict create lines $lines includes [dict keys $includes] warnings $warnings]
}

# Append the lines of path to linesVar, recursing into include files.
# trail is the chain of including files, {{normalized display} ...}; outer is
# the conditional open at the include line, {} if none. Conditionals opened in
# a file must close in it.
proc fsource::reader::splice {linesVar incsVar warnsVar path incdirs trail outer} {
    upvar 1 $linesVar lines $incsVar includes $warnsVar warnings
    variable include_re
    variable cond_re
    lappend trail [list [file normalize $path] $path]
    set conds {}        ;# conditionals open in this file, innermost last
    set lineno 0
    foreach text [physical_lines $path] {
        incr lineno
        set cond [expr {[llength $conds] ? [lindex $conds end] : $outer}]
        if {[regexp $cond_re $text -> directive]} {
            if {$directive ne "endif"} {
                lappend conds [list $directive $path:$lineno]
            } elseif {![llength $conds]} {
                fsource::fail "$path:$lineno" "#endif without #if"
            } else {
                set conds [lrange $conds 0 end-1]
            }
        }
        if {![regexp $include_re $text -> squoted dquoted]} {
            lappend lines [list $text $path $lineno $cond]
            continue
        }
        set name [expr {$squoted ne "" ? $squoted : $dquoted}]
        set found [locate $name $path $incdirs]
        if {$found eq ""} {
            fsource::fail "$path:$lineno" "include file '$name' not found"
        }
        if {[file normalize $found] in [lmap t $trail {lindex $t 0}]} {
            set chain [lmap t $trail {lindex $t 1}]
            fsource::fail "$path:$lineno" "include cycle: [join [list {*}$chain $found] { -> }]"
        }
        if {[llength $cond]} {
            lappend warnings "$path:$lineno: include '$name' is inside [describe $cond]; it is always read"
        }
        dict set includes $found {}
        splice lines includes warnings $found $incdirs $trail $cond
    }
    if {[llength $conds]} {
        lassign [lindex $conds end] directive where
        fsource::fail $where "#$directive without #endif"
    }
}

# A conditional in words: "#ifdef (file:line)"
proc fsource::reader::describe {cond} {
    lassign $cond directive where
    return "#$directive ($where)"
}

# The lines of a file. Line endings are the channel's (auto) business; the
# encoding is byte-transparent so no input can fail to decode.
proc fsource::reader::physical_lines {path} {
    if {[catch {open $path r} chan]} {
        fsource::fail $path "cannot read: $chan"
    }
    try {
        fconfigure $chan -encoding iso8859-1
        set text [read $chan]
    } finally {
        close $chan
    }
    set lines [split $text \n]
    if {[lindex $lines end] eq ""} {
        set lines [lrange $lines 0 end-1]
    }
    return $lines
}

# Find an include file: the including file's directory, then each -I directory
proc fsource::reader::locate {name from incdirs} {
    foreach dir [list [file dirname $from] {*}$incdirs] {
        set candidate [fsource::tidy [file join $dir $name]]
        if {[file isfile $candidate]} {
            return $candidate
        }
    }
    return ""
}

# -----------------------------------------------------------------------------
# Joiner: logical statements, free form (DESIGN.md §5, F2018 §6.3.2)

namespace eval fsource::joiner {}

# Join physical lines into statements.
# lines: {{text file line ?cond?} ...}, as the reader gives. Returns
# {{text file line cond} ...} where text is lowercased outside character
# literals, trimmed and without a leading label, file/line are where the
# statement starts and cond is the first conditional holding any of its lines.
proc fsource::joiner::join_lines {lines} {
    set stmts {}
    set stmt ""         ;# the statement so far
    set quote ""        ;# delimiter of the open character literal, "" if none
    set cont 0          ;# the previous line ended with &
    # {file line cond}: where the statement starts, and the first conditional
    # holding any of its lines
    set origin {}
    set file ""
    set lineno 0
    foreach rec $lines {
        lassign $rec text file lineno cond
        # Blank, comment and preprocessor lines: skipped, even mid-continuation
        if {[regexp {^\s*(?:[!#].*)?$} $text]} {
            continue
        }
        set pos 0
        if {$cont} {
            if {[regexp -indices {^\s*&} $text amp]} {
                set pos [expr {[lindex $amp 1] + 1}]
            } elseif {$quote ne ""} {
                fsource::fail "$file:$lineno" "character literal continued without a leading &"
            }
        } else {
            set origin [list $file $lineno $cond]
        }
        if {[lindex $origin 2] eq ""} {
            lset origin 2 $cond
        }
        set cont 0
        set n [string length $text]
        while {$pos < $n} {
            if {$quote ne ""} {
                set close [string first $quote $text $pos]
                if {$close < 0} {
                    # The literal reaches the line end, so it must be continued
                    set rest [string trimright [string range $text $pos end]]
                    if {[string index $rest end] ne "&"} {
                        fsource::fail "$file:$lineno" "unterminated character literal"
                    }
                    append stmt [string range $rest 0 end-1]
                    set cont 1
                    break
                }
                if {[string index $text $close+1] eq $quote} {
                    # A doubled delimiter is a quote inside the literal
                    append stmt [string range $text $pos $close+1]
                    set pos [expr {$close + 2}]
                } else {
                    append stmt [string range $text $pos $close]
                    set quote ""
                    set pos [expr {$close + 1}]
                }
                continue
            }
            if {![regexp -indices -start $pos {['"!;&]} $text special]} {
                append stmt [string tolower [string range $text $pos end]]
                break
            }
            set at [lindex $special 0]
            append stmt [string tolower [string range $text $pos $at-1]]
            set pos [expr {$at + 1}]
            switch -- [string index $text $at] {
                ' - \" {
                    set quote [string index $text $at]
                    append stmt $quote
                }
                ! {
                    break
                }
                \; {
                    emit stmts stmt $origin
                    set origin [list $file $lineno $cond]
                }
                & {
                    if {[regexp {^\s*(?:!.*)?$} [string range $text $pos end]]} {
                        set cont 1
                        break
                    }
                    append stmt &
                }
            }
        }
        if {!$cont} {
            emit stmts stmt $origin
        }
    }
    if {$cont} {
        fsource::fail [join [lrange $origin 0 1] :] "file ends inside a continued statement"
    }
    return $stmts
}

# Append the statement so far, unless empty, and start a new one
proc fsource::joiner::emit {stmtsVar stmtVar origin} {
    upvar 1 $stmtsVar stmts $stmtVar stmt
    set text [string trim $stmt]
    regsub {^\d{1,5}\s+} $text {} text
    if {$text ne ""} {
        lappend stmts [list $text {*}$origin]
    }
    set stmt ""
}

# =============================================================================
# tf_bld: the dependency generator proper

namespace eval tf_bld {
    # Raise a build error; the message may hold several lines
    proc fail {msg} {
        return -code error -errorcode TF_BLD $msg
    }
}

# -----------------------------------------------------------------------------
# Dispatcher: the statements that matter to the build (DESIGN.md §6)

namespace eval tf_bld::dispatch {
    # The first word of a statement selects its handler
    variable handlers {
        module       on_module
        submodule    on_submodule
        use          on_use
        program      on_program
        end          on_end
        endmodule    on_end
        endsubmodule on_end
        endprogram   on_end
    }

    # Whole-statement patterns; a statement that doesn't match is ignored,
    # e.g. "use = 3"
    variable re [apply {{} {
        dict map {key pat} {
            module    {^module\s+(ID)$}
            submodule {^submodule\s*\(\s*(ID)\s*(?::\s*(ID)\s*)?\)\s*(ID)$}
            use_nat   {^use\s*,\s*(intrinsic|non_intrinsic)\s*::\s*(ID)\s*(?:,.*)?$}
            use_colon {^use\s*::\s*(ID)\s*(?:,.*)?$}
            use       {^use\s+(ID)\s*(?:,.*)?$}
            program   {^program\s+(ID)$}
            end       {^end\s*(?:module|submodule|program)(?:\s+ID)?$}
            sep_proc  {^module\s+procedure\s*(?:::)?\s*(ID(?:\s*,\s*ID)*)\s*$}
            sep_head  {^((?:PREFIX\s+)+)(?:function|subroutine)\s+(ID)\M}
        } {
            string map {ID {[a-z][a-z0-9_]*}
                        PREFIX {[a-z_]+(?:\s*\((?:[^()]|\([^()]*\))*\))?}} $pat
        }
    }}]
}

# Find a file's program units and the modules they use.
# Returns a dict:
#   units {{kind name anc parent where} ...}: kind is module, submodule or
#         program; anc and parent are for a submodule ("" parent: the module)
#   uses  {{unit module where cond} ...}: unit is the key of the enclosing unit
#         (see unit_key), or {file PATH} outside any unit; cond as the joiner's
#   separate {{unit name where} ...}: separate module procedures, declared
#         (unit a module) or implemented (unit a submodule)
proc tf_bld::dispatch::scan {stmts path} {
    variable handlers
    set rec [dict create units {} uses {} separate {} path $path current [list file $path] cond {}]
    foreach stmt $stmts {
        lassign $stmt text file line cond
        if {[on_separate rec $text "$file:$line"]} {
            continue
        }
        if {[regexp {^[a-z_]+} $text word] && [dict exists $handlers $word]} {
            dict set rec cond $cond
            [dict get $handlers $word] rec $text "$file:$line"
        }
    }
    return [dict remove $rec path current cond]
}

# The key naming a unit: {module NAME}, {submodule ANC NAME} or {program NAME PATH}
proc tf_bld::dispatch::unit_key {unit path} {
    dict with unit {
        switch -- $kind {
            module    {return [list module $name]}
            submodule {return [list submodule $anc $name]}
            program   {return [list program $name $path]}
        }
    }
}

proc tf_bld::dispatch::open_unit {recVar kind name anc parent where} {
    upvar 1 $recVar rec
    set unit [dict create kind $kind name $name anc $anc parent $parent where $where]
    dict lappend rec units $unit
    dict set rec current [unit_key $unit [dict get $rec path]]
}

proc tf_bld::dispatch::on_module {recVar text where} {
    upvar 1 $recVar rec
    variable re
    if {[regexp [dict get $re module] $text -> name]} {
        open_unit rec module $name "" "" $where
    }
}

proc tf_bld::dispatch::on_submodule {recVar text where} {
    upvar 1 $recVar rec
    variable re
    if {[regexp [dict get $re submodule] $text -> anc parent name]} {
        open_unit rec submodule $name $anc $parent $where
    }
}

proc tf_bld::dispatch::on_program {recVar text where} {
    upvar 1 $recVar rec
    variable re
    if {[regexp [dict get $re program] $text -> name]} {
        open_unit rec program $name "" "" $where
    }
}

proc tf_bld::dispatch::on_use {recVar text where} {
    upvar 1 $recVar rec
    variable re
    if {[regexp [dict get $re use_nat] $text -> nature name]} {
        if {$nature eq "intrinsic"} {
            return
        }
    } elseif {![regexp [dict get $re use_colon] $text -> name]
              && ![regexp [dict get $re use] $text -> name]} {
        return
    }
    dict lappend rec uses [list [dict get $rec current] $name $where [dict get $rec cond]]
}

# A separate module procedure: a "module" prefixed function or subroutine
# statement declares one in a module and implements one in a submodule; a
# "module procedure" statement implements one in a submodule (in a module it
# is a generic interface's list, not a separate procedure). Returns 1 if the
# statement was one.
proc tf_bld::dispatch::on_separate {recVar text where} {
    upvar 1 $recVar rec
    variable re
    set unit [dict get $rec current]
    set kind [lindex $unit 0]
    if {$kind ni {module submodule}} {
        return 0
    }
    if {[regexp [dict get $re sep_head] $text -> prefix name]} {
        # The prefix's words, kind and type parameters removed
        regsub -all {\((?:[^()]|\([^()]*\))*\)} $prefix { } words
        if {"module" ni [regexp -all -inline {[a-z_]+} $words]} {
            return 0
        }
        set names [list $name]
    } elseif {$kind eq "submodule" && [regexp [dict get $re sep_proc] $text -> list]} {
        set names [regexp -all -inline {[a-z][a-z0-9_]*} $list]
    } else {
        return 0
    }
    foreach name $names {
        dict lappend rec separate [list $unit $name $where]
    }
    return 1
}

proc tf_bld::dispatch::on_end {recVar text where} {
    upvar 1 $recVar rec
    variable re
    if {[regexp [dict get $re end] $text]} {
        dict set rec current [list file [dict get $rec path]]
    }
}

# -----------------------------------------------------------------------------
# Graph: providers, edges, checks, entry-point walks (DESIGN.md §7)

namespace eval tf_bld::graph {
    # Modules the compiler supplies: a use of one that no scanned file
    # provides is neither a dependency nor an external module
    variable compiler_mods {
        iso_fortran_env iso_c_binding ieee_arithmetic ieee_exceptions
        ieee_features omp_lib omp_lib_kinds openacc
    }
}

# Build the dependency graph.
#   files    path -> {units uses separate includes}, as dispatch::scan plus the reader
#   entries  {{name path} ...}, every path one of the files
#   declared modules the Makefile declares external (-ext): a sibling's
# Returns a dict:
#   files      every path, sorted
#   obj        path -> object stem
#   deps       path -> paths of the objects it needs (other files only)
#   mods       path -> modules whose .mod it reads (from other files)
#   includes   path -> include files
#   provides   path -> modules it provides
#   externals  modules used but provided by no file, sorted
#   exts       path -> external modules it uses (or extends), sorted
#   entries    {{name {path ...}} ...}: the files each entry point links
#   warnings   gaps the link would otherwise find, and doubtful uses:
#              - a use inside a preprocessor conditional that gives a
#                dependency or an external module (kept whatever the condition)
#              - an external module not declared
#              - a separate module procedure no listed submodule implements
# Raises TF_BLD listing every problem found.
proc tf_bld::graph::build {files entries {declared {}}} {
    variable compiler_mods
    set errs {}

    # Objects: one per source, so no two sources may share a stem
    set obj [dict create]
    set byobj [dict create]
    foreach path [lsort [dict keys $files]] {
        set stem [file rootname [file tail $path]]
        if {[dict exists $byobj $stem]} {
            lappend errs "$path and [dict get $byobj $stem] would both compile to $stem.o; rename one"
        } else {
            dict set byobj $stem $path
        }
        dict set obj $path $stem
    }

    # Providers: each unit maps to its file
    set unitfile [dict create]      ;# unit key -> path
    set unitwhere [dict create]     ;# unit key -> file:line
    set provides [dict create]      ;# path -> modules
    set children [dict create]      ;# module -> paths holding its submodules
    dict for {path rec} $files {
        dict set provides $path {}
        foreach unit [dict get $rec units] {
            set key [tf_bld::dispatch::unit_key $unit $path]
            set at [dict get $unit where]
            if {[dict exists $unitfile $key]} {
                lappend errs "[describe $key] is provided twice: [dict get $unitwhere $key] and $at"
                continue
            }
            dict set unitfile $key $path
            dict set unitwhere $key $at
            switch -- [dict get $unit kind] {
                module    {dict lappend provides $path [dict get $unit name]}
                submodule {dict lappend children [dict get $unit anc] $path}
            }
        }
    }

    # Edges: unit to unit for the cycle check, file to file for make
    set uedges [dict create]        ;# unit key -> unit keys it needs
    set fedges [dict create]        ;# path -> needed path -> reasons
    set mods [dict create]          ;# path -> module -> {}
    set externals [dict create]     ;# external module -> where first met
    set exts [dict create]          ;# path -> external module -> {}
    set warnings {}
    dict for {path rec} $files {
        dict set fedges $path [dict create]
        dict set mods $path [dict create]
        dict set exts $path [dict create]
        foreach use [dict get $rec uses] {
            lassign $use ukey mod where cond
            set mkey [list module $mod]
            set kept "use $mod is inside [fsource::reader::describe $cond]; dependency kept"
            if {![dict exists $unitfile $mkey]} {
                if {$mod ni $compiler_mods} {
                    if {![dict exists $externals $mod]} {
                        dict set externals $mod "$where: use $mod"
                    }
                    dict set exts $path $mod {}
                    if {[llength $cond]} {
                        lappend warnings "$where: $kept"
                    }
                }
                continue
            }
            dict lappend uedges $ukey $mkey
            set target [dict get $unitfile $mkey]
            if {$target ne $path} {
                dict update fedges $path need {
                    dict lappend need $target "[describe $ukey] uses $mod"
                }
                dict set mods $path $mod {}
                if {[llength $cond]} {
                    lappend warnings "$where: $kept"
                }
            }
        }
        foreach unit [dict get $rec units] {
            dict with unit {}
            if {$kind ne "submodule"} {
                continue
            }
            set ukey [list submodule $anc $name]
            set pkey [expr {$parent eq "" ? [list module $anc] : [list submodule $anc $parent]}]
            if {![dict exists $unitfile $pkey]} {
                if {$parent eq ""} {
                    if {![dict exists $externals $anc]} {
                        dict set externals $anc "$where: submodule ($anc) $name extends $anc"
                    }
                    dict set exts $path $anc {}
                } else {
                    lappend errs "$where: submodule ($anc:$parent) $name: its parent, submodule $parent of $anc, is in no scanned file"
                }
                continue
            }
            dict lappend uedges $ukey $pkey
            set target [dict get $unitfile $pkey]
            if {$target ne $path} {
                dict update fedges $path need {
                    dict lappend need $target "[describe $ukey] extends [describe $pkey]"
                }
            }
            # The ancestor's .mod is read whichever parent the submodule has
            set akey [list module $anc]
            if {[dict exists $unitfile $akey] && [dict get $unitfile $akey] ne $path} {
                dict set mods $path $anc {}
            }
        }
    }

    # Cycles: units first; the file graph only if the units are acyclic
    set cycle [find_cycle $uedges]
    if {[llength $cycle]} {
        set chain [lmap key $cycle {describe $key}]
        lappend errs "module dependency cycle: [join $chain { -> }] ([dict get $unitwhere [lindex $cycle 0]])"
    } else {
        set cycle [find_cycle [dict map {path need} $fedges {dict keys $need}]]
        if {[llength $cycle]} {
            set lines [list "file dependency cycle; the modules alone are acyclic, so split a file holding several of them:"]
            foreach from [lrange $cycle 0 end-1] to [lrange $cycle 1 end] {
                lappend lines "    $from -> $to: [join [dict get $fedges $from $to] {; }]"
            }
            lappend errs [join $lines \n]
        }
    }

    # External modules the Makefile hasn't declared: a forgotten source or a
    # misspelt name, which the compile would otherwise find
    foreach mod [lsort [dict keys $externals]] {
        if {$mod ni $declared} {
            lappend warnings "[dict get $externals $mod]: no listed source provides\
                              module $mod and it is not declared -ext"
        }
    }

    # Separate module procedures: each declared in a module needs a listed
    # submodule of it implementing it, or the link fails. An entry's walk takes
    # every submodule of each module reached, so one check over all the files
    # covers every entry point.
    set implemented [dict create]   ;# module -> procedure -> {}
    dict for {path rec} $files {
        foreach sep [dict get $rec separate] {
            lassign $sep ukey name
            if {[lindex $ukey 0] eq "submodule"} {
                dict set implemented [lindex $ukey 1] $name {}
            }
        }
    }
    dict for {path rec} $files {
        foreach sep [dict get $rec separate] {
            lassign $sep ukey name where
            lassign $ukey kind mod
            if {$kind eq "module" && ![dict exists $implemented $mod $name]} {
                lappend warnings "$where: module $mod declares separate procedure\
                                  $name, but no listed submodule of $mod implements it"
            }
        }
    }

    # Entry points: everything reachable, plus every submodule of each module reached
    set walks {}
    foreach entry $entries {
        lassign $entry name epath
        set seen [dict create $epath {}]
        set work [list $epath]
        while {[llength $work]} {
            set work [lassign $work path]
            set next [dict keys [dict get $fedges $path]]
            foreach mod [dict get $provides $path] {
                if {[dict exists $children $mod]} {
                    lappend next {*}[dict get $children $mod]
                }
            }
            foreach n $next {
                if {![dict exists $seen $n]} {
                    dict set seen $n {}
                    lappend work $n
                }
            }
        }
        lappend walks [list $name [list $epath {*}[lsort [lrange [dict keys $seen] 1 end]]]]
    }

    if {[llength $errs]} {
        tf_bld::fail [join $errs \n]
    }
    return [dict create \
        files     [lsort [dict keys $files]] \
        obj       $obj \
        deps      [dict map {path need} $fedges {lsort [dict keys $need]}] \
        mods      [dict map {path m} $mods {lsort [dict keys $m]}] \
        includes  [dict map {path rec} $files {dict get $rec includes}] \
        provides  $provides \
        externals [lsort [dict keys $externals]] \
        exts      [dict map {path m} $exts {lsort [dict keys $m]}] \
        entries   $walks \
        warnings  $warnings]
}

# A unit key in words
proc tf_bld::graph::describe {key} {
    switch -- [lindex $key 0] {
        module    {return "module [lindex $key 1]"}
        submodule {return "submodule ([lindex $key 1]) [lindex $key 2]"}
        program   {return "program [lindex $key 1]"}
        file      {return [lindex $key 1]}
    }
}

# A cycle in a graph given as node -> nodes it needs, as {n0 n1 ... n0}, or {}.
# Nodes needing nothing are removed until none is left; any left over have
# an edge to another left over, so following those edges must repeat a node.
proc tf_bld::graph::find_cycle {edges} {
    set out [dict create]           ;# node -> needs not yet removed
    set rev [dict create]           ;# node -> nodes needing it
    dict for {node needs} $edges {
        if {![dict exists $out $node]} {
            dict set out $node 0
        }
        foreach t [lsort -unique $needs] {
            dict incr out $node
            dict lappend rev $t $node
            if {![dict exists $out $t]} {
                dict set out $t 0
            }
        }
    }
    set ready [dict keys [dict filter $out value 0]]
    while {[llength $ready]} {
        set ready [lassign $ready node]
        dict unset out $node
        if {[dict exists $rev $node]} {
            foreach p [dict get $rev $node] {
                dict incr out $p -1
                if {[dict get $out $p] == 0} {
                    lappend ready $p
                }
            }
        }
    }
    if {[dict size $out] == 0} {
        return {}
    }
    set node [lindex [dict keys $out] 0]
    set path {}
    while {[lsearch -exact $path $node] < 0} {
        lappend path $node
        foreach t [lsort -unique [dict get $edges $node]] {
            if {[dict exists $out $t]} {
                set node $t
                break
            }
        }
    }
    return [list {*}[lrange $path [lsearch -exact $path $node] end] $node]
}

# -----------------------------------------------------------------------------
# Writer: the text of depends.mk (DESIGN.md §9.2)

namespace eval tf_bld::writer {
    # Recipe of a missing-.mod guard (DESIGN.md §8.1); $| is the module's object
    variable guard {@test -f $@ || { echo "tf_bld: $@ is missing but $| exists (removed by hand?). $| deleted: re-run make." >&2; rm -f $|; exit 1; }}
    variable width 80
}

# depends.mk for a graph; vpaths are the directories make searches for sources
proc tf_bld::writer::render {graph vpaths} {
    variable guard
    set obj [dict get $graph obj]
    set out [list "# Generated by tf_bld - do not edit" ""]
    lappend out "vpath %.f90 [join $vpaths]" "vpath %.F90 [join $vpaths]" ""

    lappend out "# Each object: its source, the objects providing what it needs, their" \
                "# .mod files, a hook \$(TF_EXT_m) per external module m it uses (the" \
                "# Makefile may set it, e.g. to the sibling's .mod) and its include files"
    foreach path [dict get $graph files] {
        set prereqs [list [file tail $path]]
        foreach need [dict get $graph deps $path] {
            lappend prereqs [object [dict get $obj $need]]
        }
        foreach mod [dict get $graph mods $path] {
            lappend prereqs "\$(ODIR)/$mod.mod"
        }
        foreach mod [dict get $graph exts $path] {
            lappend prereqs "\$(TF_EXT_$mod)"
        }
        lappend prereqs {*}[lsort [dict get $graph includes $path]]
        lappend out [wrap "[object [dict get $obj $path]]:" $prereqs]
    }

    lappend out "" "# Missing-.mod guards: run only when a .mod is absent, after its object"
    foreach path [dict get $graph files] {
        foreach mod [lsort [dict get $graph provides $path]] {
            lappend out "\$(ODIR)/$mod.mod: | [object [dict get $obj $path]] ; $guard"
        }
    }

    lappend out "" "# Objects linked by each entry point"
    foreach entry [dict get $graph entries] {
        lassign $entry name paths
        lappend out [wrap "${name}_OBJS :=" [lmap p $paths {object [dict get $obj $p]}]]
    }

    lappend out "" "# Every object"
    lappend out [wrap "TF_OBJS :=" [lmap p [dict get $graph files] {object [dict get $obj $p]}]]
    lappend out "" "# Modules used but provided by no scanned source (siblings, external)"
    lappend out [wrap "TF_EXTERNAL_MODS :=" [dict get $graph externals]]
    lappend out "" "# Every file read, for the regeneration rule"
    set sources [dict get $graph files]
    foreach path [dict get $graph files] {
        lappend sources {*}[dict get $graph includes $path]
    }
    lappend out [wrap "TF_SOURCES :=" [lsort -unique $sources]]
    lappend out "" "# A file read here but since deleted then forces regeneration, not an error" \
                "\$(TF_SOURCES):"
    return "[join $out \n]\n"
}

proc tf_bld::writer::object {stem} {
    return "\$(ODIR)/$stem.o"
}

# "head item item ..." folded with backslash continuations at the page width
proc tf_bld::writer::wrap {head items} {
    variable width
    set lines {}
    set line $head
    foreach item $items {
        if {[string length "$line $item"] > $width - 2 && $line ne $head} {
            lappend lines "$line \\"
            set line "   "
        }
        append line " $item"
    }
    lappend lines $line
    return [join $lines \n]
}

# -----------------------------------------------------------------------------
# Command line and main

namespace eval tf_bld::cli {
    variable usage {usage: tf_bld.tcl -o FILE [options] SOURCE ... [@ARGFILE ...]
  SOURCE          a free-form source (.f90 or .F90) to scan; only these are scanned
  @ARGFILE        read more arguments from ARGFILE, separated by white space
  -o FILE         the depends.mk to write
  -I DIR          include-file search directory (repeatable)
  -entry NAME=F   a program or test: NAME_OBJS lists the objects it links (repeatable)
  -ext MODULE     a module some other project provides, e.g. a sibling library's (repeatable)
  -nowarn         do not report warnings
  -Werror         treat warnings as errors: report them and write nothing}
}

# Expand each @FILE argument into the words of FILE (not recursively)
proc tf_bld::cli::expand {argv} {
    set out {}
    foreach arg $argv {
        if {[string index $arg 0] ne "@"} {
            lappend out $arg
            continue
        }
        set path [string range $arg 1 end]
        if {[catch {open $path r} chan]} {
            tf_bld::fail "$arg: cannot read: $chan"
        }
        try {
            set words [regexp -all -inline {\S+} [read $chan]]
        } finally {
            close $chan
        }
        foreach word $words {
            if {[string index $word 0] eq "@"} {
                tf_bld::fail "$arg: holds '$word'; argument files do not nest"
            }
        }
        lappend out {*}$words
    }
    return $out
}

# Parse the command line into a dict: out srcs incs entries {{name file} ...}
# exts nowarn werror
proc tf_bld::cli::parse {argv} {
    variable usage
    set opts [dict create out "" srcs {} incs {} entries {} exts {} nowarn 0 werror 0]
    set argv [expand $argv]
    while {[llength $argv]} {
        set argv [lassign $argv opt]
        if {$opt in {-h -help --help}} {
            tf_bld::fail $usage
        }
        if {[string index $opt 0] ne "-"} {
            dict lappend opts srcs [fsource::tidy $opt]
            continue
        }
        if {$opt eq "-nowarn"} {
            dict set opts nowarn 1
            continue
        }
        if {$opt eq "-Werror"} {
            dict set opts werror 1
            continue
        }
        if {$opt ni {-o -I -entry -ext}} {
            tf_bld::fail "unknown option '$opt'\n$usage"
        }
        if {![llength $argv]} {
            tf_bld::fail "$opt needs a value\n$usage"
        }
        set argv [lassign $argv value]
        switch -- $opt {
            -o   {dict set opts out $value}
            -I   {dict lappend opts incs [fsource::tidy $value]}
            -ext {
                if {![regexp {^[A-Za-z][A-Za-z0-9_]*$} $value]} {
                    tf_bld::fail "-ext wants a module name, not '$value'"
                }
                dict lappend opts exts [string tolower $value]
            }
            -entry {
                if {![regexp {^([A-Za-z_][A-Za-z0-9_]*)=(.+)$} $value -> name file]} {
                    tf_bld::fail "-entry wants NAME=FILE with NAME a make variable prefix, not '$value'"
                }
                if {$name in [lmap e [dict get $opts entries] {lindex $e 0}]} {
                    tf_bld::fail "entry point '$name' given twice"
                }
                dict lappend opts entries [list $name [fsource::tidy $file]]
            }
        }
    }
    if {[dict get $opts out] eq ""} {
        tf_bld::fail "no output file (-o)\n$usage"
    }
    if {[dict get $opts nowarn] && [dict get $opts werror]} {
        tf_bld::fail "-nowarn and -Werror contradict each other"
    }
    if {![llength [dict get $opts srcs]] && ![llength [dict get $opts entries]]} {
        tf_bld::fail "nothing to scan: name the sources\n$usage"
    }
    return $opts
}

# Read, join and dispatch one source file
proc tf_bld::scan_file {path incdirs} {
    set src [fsource::reader::read_source $path $incdirs]
    set rec [dispatch::scan [fsource::joiner::join_lines [dict get $src lines]] $path]
    dict set rec includes [dict get $src includes]
    dict set rec warnings [dict get $src warnings]
    return $rec
}

# Scan the sources named by the options. Returns {files entries vpaths
# warnings}: entries with paths as the files are keyed, vpaths the source
# directories in the order first met, warnings the reader's and those of an
# entry that is not among the sources.
proc tf_bld::scan_all {opts} {
    set errs {}
    set warnings {}
    set paths {}
    set vpaths {}
    set known [dict create]         ;# normalized path -> path as given
    foreach path [dict get $opts srcs] {
        if {[file extension $path] ni {.f90 .F90}} {
            lappend errs "$path: not a free-form source (.f90 or .F90)"
            continue
        }
        if {![file isfile $path]} {
            lappend errs "$path: no such file"
            continue
        }
        set norm [file normalize $path]
        if {[dict exists $known $norm]} {
            continue
        }
        dict set known $norm $path
        lappend paths $path
        if {[file dirname $path] ni $vpaths} {
            lappend vpaths [file dirname $path]
        }
    }
    set entries {}
    foreach entry [dict get $opts entries] {
        lassign $entry name file
        set norm [file normalize $file]
        if {![dict exists $known $norm]} {
            if {![file isfile $file]} {
                lappend errs "-entry $name: no such file '$file'"
                continue
            }
            lappend warnings "-entry $name: $file is not among the sources; scanned anyway"
            dict set known $norm $file
            lappend paths $file
            if {[file dirname $file] ni $vpaths} {
                lappend vpaths [file dirname $file]
            }
        }
        lappend entries [list $name [dict get $known $norm]]
    }
    set files [dict create]
    foreach path $paths {
        try {
            dict set files $path [scan_file $path [dict get $opts incs]]
        } trap FSOURCE {msg} {
            lappend errs $msg
        }
    }
    if {[llength $errs]} {
        fail [join $errs \n]
    }
    dict for {path rec} $files {
        lappend warnings {*}[dict get $rec warnings]
    }
    return [list $files $entries $vpaths $warnings]
}

# Write text to a file, replacing it only once complete
proc tf_bld::write_file {path text} {
    set tmp "$path.tmp"
    set chan [open $tmp w]
    try {
        fconfigure $chan -encoding iso8859-1 -translation lf
        puts -nonewline $chan $text
    } finally {
        close $chan
    }
    file rename -force $tmp $path
}

# Returns the exit status
proc tf_bld::main {argv} {
    try {
        set opts [cli::parse $argv]
        lassign [scan_all $opts] files entries vpaths warnings
        set graph [graph::build $files $entries [dict get $opts exts]]
        lappend warnings {*}[dict get $graph warnings]
        if {![dict get $opts nowarn]} {
            foreach warning $warnings {
                puts stderr "tf_bld: warning: $warning"
            }
        }
        if {[dict get $opts werror] && [llength $warnings]} {
            fail "-Werror: [llength $warnings] warning(s); [dict get $opts out] not written"
        }
        write_file [dict get $opts out] [writer::render $graph $vpaths]
        set nmods 0
        dict for {path mods} [dict get $graph provides] {
            incr nmods [llength $mods]
        }
        puts "tf_bld: wrote [dict get $opts out]: [dict size $files] sources,\
              $nmods modules, [llength $entries] entry points"
    } trap TF_BLD {msg} {
        foreach line [split $msg \n] {
            puts stderr "tf_bld: $line"
        }
        return 1
    }
    return 0
}

if {[info exists ::argv0] && [file normalize $::argv0] eq [file normalize [info script]]} {
    exit [tf_bld::main $::argv]
}
