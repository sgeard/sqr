#!/usr/bin/env tclsh9.1
# SPDX-License-Identifier: MIT
# Copyright (c) 2026 Simon Geard
#
# fortran_deps.tcl — scan Fortran .f90 files and generate depends.mk
#
# Usage: tclsh9.1 fortran_deps.tcl [src_dir [out_file [entry.f90 ...]]]
#
# src_dir   — directory to scan (default: .)
# out_file  — output file       (default: depends.mk)
# entry.f90 — starting points for dependency traversal; if omitted, any
#             file containing a top-level 'program' statement is used.
#
# Scans all .f90/.F90 files, builds a dependency graph, then performs a
# BFS from the entry points to find all reachable files.  Only those files
# produce lines in depends.mk, so unrelated scratch files are excluded.
#
# Output lines have the form:
#   $(ODIR)/foo.o: $(ODIR)/bar.mod
# External/intrinsic modules (AVD, iso_fortran_env, …) are silently ignored.

namespace eval dependencies {
    variable fh {} ; # File handle, used to handle continuation lines
    
    proc module {} {
        variable fh
    }
    
    proc submodule {} {
        variable fh
    }
    
    proc use {} {
        variable fh
    }
    
    proc program {} {
        variable fh
    }
}


set src_dir  [expr {[llength $argv] > 0 ? [lindex $argv 0] : "."}]
set out_file [expr {[llength $argv] > 1 ? [lindex $argv 1] : "depends.mk"}]
set seed_args [lrange $argv 2 end]   ;# optional explicit entry files

# ---- Phase 1: scan every source file -----------------------------------

array set provides  {}   ;# fname  -> list of module names defined here
array set uses      {}   ;# fname  -> list of module names used here
array set parents   {}   ;# fname  -> list of parent module names (submodule)
array set is_prog   {}   ;# fname  -> 1 if file contains a program unit

# Collect paths: all .f90/.F90 in src_dir, plus any entry files outside it.
# src_names tracks which files came from src_dir — only those get dep lines emitted.
set scan_paths  [lsort [glob -directory $src_dir -nocomplain *.f90 *.F90]]
set src_names   [lmap p $scan_paths {file tail $p}]
foreach s $seed_args {
    if {[file exists $s] && [file tail $s] ni $src_names} {
        lappend scan_paths $s
    }
}

foreach path $scan_paths {
    set fname [file tail $path]
    set provides($fname) {}
    set uses($fname)     {}
    set parents($fname)  {}
    set is_prog($fname)  0

    set fh [open $path r]
    while {[gets $fh line] >= 0} {
        set line [string trim $line]
        if {[string index $line 0] eq "!"} continue
        set ci [string first "!" $line]
        if {$ci >= 0} { set line [string range $line 0 $ci-1] }
        set lc [string tolower [string trim $line]]

        # module <name>  — "module name" alone on the logical line
        if {[regexp {^module\s+([a-z_]\w*)\s*$} $lc -> mname]} {
            lappend provides($fname) $mname
            continue
        }

        # submodule (<parent>[:<ancestor>]) <name>
        if {[regexp {^submodule\s*\(\s*([a-z_]\w*)} $lc -> parent]} {
            if {$parent ni $parents($fname)} {
                lappend parents($fname) $parent
            }
            continue
        }

        # use <name>[, ...]
        if {[regexp {^use\s+([a-z_]\w*)} $lc -> mname]} {
            if {$mname ni $uses($fname)} { lappend uses($fname) $mname }
            continue
        }

        # use, <attribute> :: <name>[, ...]  (e.g. "use, intrinsic :: iso_fortran_env")
        if {[regexp {^use\s*,\s*[a-z_]\w*\s*::\s*([a-z_]\w*)} $lc -> mname]} {
            if {$mname ni $uses($fname)} { lappend uses($fname) $mname }
            continue
        }

        # program <name>
        if {[regexp {^program\s+[a-z_]\w*} $lc]} {
            set is_prog($fname) 1
            continue
        }
    }
    close $fh
}

# ---- Phase 2: build reverse maps ---------------------------------------

# module name -> file that provides it
array set mod_provider {}
foreach fname [array names provides] {
    foreach mname $provides($fname) {
        set mod_provider($mname) $fname
    }
}

# module name -> list of files that are submodules of it
array set submod_files {}
foreach fname [array names parents] {
    foreach parent $parents($fname) {
        lappend submod_files($parent) $fname
    }
}

# ---- Phase 3: BFS from entry points ------------------------------------

# Determine seeds
if {[llength $seed_args] > 0} {
    set seeds [lmap s $seed_args {file tail $s}]
} else {
    # No entry points given: list detected program files and exit with guidance
    set progs {}
    foreach fname [lsort [array names is_prog]] {
        if {$is_prog($fname)} { lappend progs $fname }
    }
    if {[llength $progs] == 0} {
        puts stderr "Error: no program files found in $src_dir"
    } else {
        puts stderr "No entry files specified.  Detected program files:"
        foreach p $progs { puts stderr "  $p" }
        puts stderr "Usage: tclsh9.1 fortran_deps.tcl \[src_dir \[out_file\]\] entry.f90 ..."
    }
    exit 1
}

set visited {}
set queue   $seeds

while {[llength $queue] > 0} {
    set fname [lindex $queue 0]
    set queue [lrange $queue 1 end]
    if {$fname in $visited} continue
    lappend visited $fname

    # Follow 'use' edges to the files that provide those modules
    if {![info exists uses($fname)]} continue
    foreach mname $uses($fname) {
        if {[info exists mod_provider($mname)]} {
            set pf $mod_provider($mname)
            if {$pf ni $visited} { lappend queue $pf }
        }
    }

    # For each module this file provides, pull in its submodule files
    foreach mname $provides($fname) {
        if {[info exists submod_files($mname)]} {
            foreach sf $submod_files($mname) {
                if {$sf ni $visited} { lappend queue $sf }
            }
        }
    }
}

# ---- Phase 4: emit depends.mk for reachable files only ----------------

set out [open $out_file w]
puts $out "# Auto-generated by fortran_deps.tcl — do not edit manually"
puts $out {# Run: tclsh fortran_deps.tcl [src_dir [out_file [entry.f90 ...]]}
puts $out ""

foreach fname [lsort $visited] {
    if {![info exists uses($fname)]} continue
    if {$fname ni $src_names} continue   ;# entry files outside src_dir: skip
    set stem [file rootname $fname]
    set obj  "\$(ODIR)/${stem}.o"
    set deps {}

    # Submodule depends on each parent's .mod
    foreach parent $parents($fname) {
        if {[info exists mod_provider($parent)]} {
            set dep "\$(ODIR)/${parent}.mod"
            if {$dep ni $deps} { lappend deps $dep }
        }
    }

    # 'use' dependencies on locally-defined modules
    foreach mname $uses($fname) {
        if {[info exists mod_provider($mname)]} {
            set dep "\$(ODIR)/${mname}.mod"
            if {$dep ni $deps} { lappend deps $dep }
        }
    }

    if {[llength $deps] > 0} {
        puts $out "${obj}: [join $deps { }]"
    }
}

close $out
puts "Generated $out_file ([llength $visited] files reachable from: [join $seeds {, }])"
