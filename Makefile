.PHONY: all check depcheck clean veryclean distclean utest sqlttest shelltest sqrdtest calctest install uninstall install-calc faulttest run-faulttest proctest bench run-bench destruct run-destruct coverage coverage-gcov coverage-clean docs docs-clean help windows win-build sqrsh-regex test-regex
.SUFFIXES:
.DEFAULT_GOAL := all

# Source layout
SRC_DIR   := src
APP_DIR   := app
TEST_DIR  := test
REGEX_DIR := regex
FAULT_DIR := fault
BENCH_DIR := bench
DESTRUCT_DIR := destruct

# Fault-injection variant (sqr_fault submodule). off = production
# (zero machinery, shared with fpm); on = coverage/fault test only.
FAULT ?= off

# Install location.  PREFIX=$(HOME)/.local installs without root (and
# ~/.local/bin is already on PATH); DESTDIR stages the tree for packaging.
# run_sqrd finds sqrd/sqrbak beside itself, so installing the four programs
# and the launcher into one directory is all the wiring there is.
PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin

# Per-compiler extras consumed by foptions_$(F).mk
F_EXTRA_GF  := -Wno-unused-dummy-argument
F_EXTRA_IFX := -assume byterecl -diag-disable=7712

# --- Compiler selection: default ifx (release), validated ------------------
F ?= ifx
VALID_F := gfortran ifx lfortran flang
ifeq ($(filter $(F),$(VALID_F)),)
  $(error Unknown Fortran compiler 'F=$(F)' -- choose one of: $(VALID_F))
endif

# Canonical compiler options, generated into foptions_$(F).mk by generate_fopts.tcl
OPTIONS_FNAME := foptions_$(F).mk
$(OPTIONS_FNAME): generate_fopts.tcl
	tclsh generate_fopts.tcl $(F) $(OPTIONS_FNAME)

-include $(OPTIONS_FNAME)

# Sibling tcl_re (opt-in regex search only: sqrsh-regex, test-regex). Its
# self-contained static engine, libtclInterface.a, bundles the regex engine
# (no libtcl). tcl_re shares the standardised build, so its ODIR matches
# $(ODIR). Built on demand if missing.
TCLRE_DIR  ?= ../tcl_re
TCLRE_ODIR ?= $(TCLRE_DIR)/$(ODIR)
TCLRE_LIB  ?= $(TCLRE_ODIR)/libtclInterface.a
TF_EXT_tcl_frx := $(TCLRE_ODIR)/tcl_frx.mod

$(TCLRE_LIB) $(TF_EXT_tcl_frx):
	$(MAKE) -C $(TCLRE_DIR) F=$(F)

F_OPTS := $(F_BASE) $(F_BUILD) $(MOD_OPTS) -I$(SRC_DIR)
LFLAGS := $(F_LOPTS)

# C shim (osshim.c) — the OS/platform split lives in C, where the C
# preprocessor reliably predefines _WIN32. A plain C compiler suffices and its
# objects are ABI-compatible with every Fortran compiler offered here.
CC     ?= cc
# -D_FILE_OFFSET_BITS=64: a 64-bit off_t on 32-bit glibc targets, so
# sqr_os_truncate's int64 length survives the cast (no-op on 64-bit hosts).
CFLAGS ?= -O2 -Wall -Wextra -D_FILE_OFFSET_BITS=64

# --- Sources ---
# Core (all in $(SRC_DIR), fpm-shared). The fault module interface and the
# production (off) submodule live here too, so fpm builds a zero-machinery
# library and the standard test suite unchanged.
# The sql_* modules are a front-end layer (lexer/parser/executor) that only
# call sqr's public API; they depend on sqr, never the reverse. Bundled in the
# same archive purely for convenience (fpm globs all of src/ regardless).
# cmdgraph + dlist are vendored from github.com/sgeard/cmdgraph (fortran/src) so
# the shell builds out-of-the-box with no sibling checkout; they go into libsqr.a
# alongside the engine (the linker pulls cmdgraph objects only into sqrsh).
LIB_SRC := clib_wrap.f90 clib_wrap_sm.f90 b_tree.f90 b_tree_sm.f90 sqr_fault.f90 sqr_consts.f90 sqr.f90 sqr_base.f90 sqr_table.f90 sqr_record.f90 sqr_index.f90 sqr_admin.f90 sqr_rowbuf.f90 sqr_journal.f90 sqr_pack.f90 sql.f90 sql_base.f90 sql_parse.f90 sql_exec.f90 sqr_net.f90 sqr_net_sm.f90 sqr_serve.f90 sqr_serve_sm.f90 dlist.f90 dlist_sm.f90 cmdgraph.f90 cmdgraph_sm.f90

# Selected fault submodule: off -> $(SRC_DIR) (production, fpm-shared);
# on -> $(FAULT_DIR) (Make-only coverage/faulttest, never seen by fpm).
ifeq ($(FAULT),on)
  FAULT_SM_SRC := $(FAULT_DIR)/sqr_fault_on_sm.f90
else
  FAULT_SM_SRC := $(SRC_DIR)/sqr_fault_off_sm.f90
endif
FAULT_SM_OBJ := $(ODIR)/sqr_fault_$(FAULT)_sm.o

# The platform shim is C; everything else is Fortran.
LIB_C_SRC := osshim.c
LIB_C_OBJ := $(addprefix $(ODIR)/,$(LIB_C_SRC:.c=.o))

LIB_OBJ := $(addprefix $(ODIR)/,$(LIB_SRC:.f90=.o)) $(LIB_C_OBJ) $(FAULT_SM_OBJ)
LIB     := $(ODIR)/libsqr.a

# Test binaries: one per test/utest_*.f90 (the fault sweep lives in
# $(FAULT_DIR), excluded from the default suite and from fpm).
# The opt-in regex test (utest_match) lives in $(REGEX_DIR), not test/, so it
# is out of both this glob and fpm's reach; it is built by `test-regex`.
TEST_SRC := $(wildcard $(TEST_DIR)/utest_*.f90)
TEST_BIN := $(patsubst $(TEST_DIR)/%.f90,$(ODIR)/%$(EXT),$(TEST_SRC))
FAULT_TEST_SRC := $(FAULT_DIR)/utest_fault.f90
FAULT_TEST_BIN := $(ODIR)/utest_fault$(EXT)

# Performance benchmark (Make-only, in bench/ — never seen by fpm).
# Built against the production FAULT=off optimised library.
BENCH_SRC := $(BENCH_DIR)/bench_sqr.f90
BENCH_BIN := $(ODIR)/bench_sqr$(EXT)

# Interactive shells: sqrsh (cmdgraph-driven engine shell) and sqlsh (the
# SQL-subset REPL — depends only on the library, not cmdgraph); sqrd is the
# wire-protocol server and sqrbak its backup client (library only, like sqlsh).
APP_SRC := $(wildcard $(APP_DIR)/sqrsh.f90 $(APP_DIR)/sqlsh.f90 $(APP_DIR)/sqrd.f90 \
                      $(APP_DIR)/sqrbak.f90)
APP_BIN := $(patsubst $(APP_DIR)/%.f90,$(ODIR)/%$(EXT),$(APP_SRC))

all: $(OPTIONS_FNAME) $(LIB) $(TEST_BIN) $(APP_BIN)

# --- Module dependencies: depends.mk, written by tf_bld.tcl ---
# The master in ../common/tf_bld, run in place; a standalone mirror (no
# ../common) carries its own copy beside this Makefile. The Makefile names
# the sources: the library's (both fault submodules, so depends.mk is the
# same for FAULT=off and on) and every program's. No entry points: every
# program links libsqr.a.
TF_BLD  ?= $(firstword $(wildcard ../common/tf_bld/tf_bld.tcl) tf_bld.tcl)
TF_SRCS := $(addprefix $(SRC_DIR)/,$(LIB_SRC)) \
           $(SRC_DIR)/sqr_fault_off_sm.f90 $(FAULT_DIR)/sqr_fault_on_sm.f90 \
           $(TEST_SRC) $(TEST_DIR)/proctest.f90 $(FAULT_TEST_SRC) \
           $(APP_SRC) $(APP_DIR)/sqrsh_extras.f90 $(APP_DIR)/sqrsh_extras_off_sm.f90 \
           $(wildcard $(REGEX_DIR)/*.f90 $(BENCH_DIR)/*.f90 $(DESTRUCT_DIR)/*.f90)

ifeq ($(filter clean veryclean distclean help depcheck docs docs-clean coverage-clean windows,$(MAKECMDGOALS)),)
include depends.mk
endif

# Each TF_EXT_m hook declares a sibling's module
TF_ARGS = $(TF_SRCS) \
    $(addprefix -ext ,$(patsubst TF_EXT_%,%,$(filter TF_EXT_%,$(.VARIABLES))))

# After the include: TF_SOURCES (every file the last scan read) comes from it
depends.mk: $(TF_BLD) Makefile $(TF_SRCS) $(TF_SOURCES)
	tclsh $(TF_BLD) -o $@ $(TF_ARGS)

# The same scan with warnings as errors: a forgotten source or an
# unimplemented separate procedure fails here, not at the link
depcheck:
	tclsh $(TF_BLD) -Werror -o depends.mk $(TF_ARGS)

# --- Compilation: one pattern rule; depends.mk supplies the prerequisites
# (and the vpath). Per-file extras come from <stem>_OPTS.
$(ODIR)/%.o: %.f90 | $(ODIR)
	$(F) -c $(F_OPTS) $($*_OPTS) -o $@ $<

# The regex sources compile against tcl_re's module
sqrsh_regex_OPTS := -I$(TCLRE_ODIR)
utest_match_OPTS := -I$(TCLRE_ODIR)

# Platform shim (C). No Fortran module dependencies; the bind(c) interfaces in
# clib_wrap resolve to these symbols at link time.
$(ODIR)/osshim.o: $(SRC_DIR)/osshim.c | $(ODIR)
	$(CC) $(CFLAGS) -c -o $@ $<

# fb_monitor (progress feedback for long loops) lives in $(BENCH_DIR) with its
# only consumer, so it is neither archived into libsqr nor globbed by fpm.

# Library archive. Rebuilt from scratch (not `ar r` onto a stale archive):
# the FAULT=off and FAULT=on fault submodules have distinct object names, so
# adding to an existing archive could leave BOTH in it and let the linker
# resolve io_check from the wrong one (silently disabling fault injection).
$(LIB): $(LIB_OBJ) | $(ODIR)
	@rm -f $@
	$(AR) rcs $@ $(LIB_OBJ)

# Tests: each links its own object and the library
$(TEST_BIN): $(ODIR)/%$(EXT): $(ODIR)/%.o $(LIB)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

# Shell — the cmdgraph engine is vendored into libsqr.a, so the library is all
# it links, with the default (empty) optional-command set; sqrsh-regex below
# links the regex one instead.
SQRSH_OBJ := $(ODIR)/sqrsh.o $(ODIR)/sqrsh_extras.o
$(ODIR)/sqrsh$(EXT): $(SQRSH_OBJ) $(ODIR)/sqrsh_extras_off_sm.o $(LIB)
	$(F) $(F_OPTS) -o $@ $(SQRSH_OBJ) $(ODIR)/sqrsh_extras_off_sm.o $(LIB) $(LFLAGS)

# The others link the library only: sqlsh, the SQL REPL (the sql_* front-end
# is in libsqr); sqrd, the wire-protocol server (sqr_net/sqr_serve are in
# libsqr); sqrbak, the backup/enquiry client for a running sqrd (INFO reports
# the served directory, PACK writes a container without closing the database).
$(ODIR)/sqlsh$(EXT) $(ODIR)/sqrd$(EXT) $(ODIR)/sqrbak$(EXT): $(ODIR)/%$(EXT): $(ODIR)/%.o $(LIB)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

# --- Optional regex-search shell (opt-in) ---------------------------------
# `make sqrsh-regex` relinks the shell with the `match <col> <regex>` command
# (regex scan over DT_CHAR columns). The command is the alternative
# implementation of module sqrsh_extras, $(REGEX_DIR)/sqrsh_regex.f90, linked
# in place of the default empty one: the selection is made here, at the link,
# and the app/ sources are the same either way. It links the sibling
# tcl_re project's static engine. All of $(REGEX_DIR) is kept out of the
# default build and out of the dirs fpm globs, so fpm and the Windows
# cross-build stay dependency-free; tcl_re cannot be built by fpm in any case.
sqrsh-regex: $(OPTIONS_FNAME) $(SQRSH_OBJ) $(ODIR)/sqrsh_regex.o $(LIB) $(TCLRE_LIB)
	$(F) $(F_OPTS) -o $(ODIR)/sqrsh$(EXT) $(SQRSH_OBJ) $(ODIR)/sqrsh_regex.o \
	    $(LIB) $(TCLRE_LIB) $(LFLAGS)
	@echo "Built $(ODIR)/sqrsh$(EXT) with regex search (match command)"

# Functional test for the regex search (opt-in, links tcl_re).
REGEX_TEST_BIN := $(ODIR)/utest_match$(EXT)
$(REGEX_TEST_BIN): $(ODIR)/utest_match.o $(LIB) $(TCLRE_LIB)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(TCLRE_LIB) $(LFLAGS)

test-regex: $(OPTIONS_FNAME) $(REGEX_TEST_BIN)
	@echo "==> $(REGEX_TEST_BIN)" && $(REGEX_TEST_BIN)

$(ODIR):
	mkdir -p $@

utest: $(TEST_BIN)
	@for t in $(TEST_BIN); do echo "==> $$t" && $$t || exit 1; done

# The quick gate: the dependency scan with warnings as errors, the unit
# tests and the two shell-level suites.
check: depcheck utest sqlttest shelltest

# sqllogictest-subset functional tests: a Tcl harness (no tcllib; uses the
# system md5sum) replays sqlt/tests/*.test records through the sqlsh REPL and
# diffs results. Black-box, complements the utest_sql unit tests. See sqlt/README.md.
SQLT_DIR    := sqlt
SQLT_TESTS  := $(wildcard $(SQLT_DIR)/tests/*.test)
sqlttest: $(ODIR)/sqlsh$(EXT)
	tclsh $(SQLT_DIR)/run_sqlt.tcl $(ODIR)/sqlsh$(EXT) $(SQLT_TESTS)

# Shell-level behaviour of sqlsh and sqrsh that is neither SQL nor reachable
# from the Fortran unit tests: meta-commands and the encoding of the input
# stream (the same cases through both shells).
shelltest: $(ODIR)/sqlsh$(EXT) $(ODIR)/sqrsh$(EXT)
	tclsh $(SQLT_DIR)/run_shell.tcl $(ODIR)/sqlsh$(EXT) $(ODIR)/sqrsh$(EXT)

# Functional test of the sqrd wire service: a Tcl client (an independent
# protocol implementation — see test/run_sqrd.tcl) drives a real spawned
# sqrd over a loopback socket and verifies the session end to end,
# including binary-cell exactness.
sqrdtest: $(ODIR)/sqrd$(EXT)
	tclsh $(TEST_DIR)/run_sqrd.tcl $(ODIR)/sqrd$(EXT)

# --- LibreOffice Calc client (calc/) ---------------------------------------
# calc/tclcalc.py is the generic pyuno macro host; calc/sqr_calc.tcl is the
# sqr director it embeds (tkinter.Tcl).  calctest runs the real director
# against a mock servant and a real spawned sqrd — everything except the
# pyuno lines, which get a manual smoke test in Calc (calc/README.md).
CALC_DIR     := calc
LO_SCRIPTS   := $(HOME)/.config/libreoffice/4/user/Scripts/python

calctest: $(ODIR)/sqrd$(EXT)
	tclsh $(CALC_DIR)/test_calc.tcl $(ODIR)/sqrd$(EXT)

# Install the programs and the launcher side by side, which is the only
# arrangement run_sqrd needs: it looks for sqrd/sqrbak in its own directory
# first, and falls back to the obj_* build directories when it is being run
# out of the source tree.  Nothing is symlinked and nothing has a path
# compiled into it.
install: $(APP_BIN)
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m 755 $(APP_BIN) $(DESTDIR)$(BINDIR)/
	install -m 755 run_sqrd $(DESTDIR)$(BINDIR)/
	@echo "Installed $(notdir $(APP_BIN)) run_sqrd to $(DESTDIR)$(BINDIR)"

uninstall:
	rm -f $(addprefix $(DESTDIR)$(BINDIR)/,$(notdir $(APP_BIN)) run_sqrd)
	@echo "Removed $(notdir $(APP_BIN)) run_sqrd from $(DESTDIR)$(BINDIR)"

install-calc:
	mkdir -p $(LO_SCRIPTS)
	install -m 644 $(CALC_DIR)/tclcalc.py $(CALC_DIR)/sqr_calc.tcl $(LO_SCRIPTS)/
	@echo "Installed tclcalc.py + sqr_calc.tcl to $(LO_SCRIPTS)"

# Fault-injection sweep. Built only here (and by coverage), always with
# FAULT=on, into a debug ODIR so the production release archive is never
# overwritten with the on submodule.
$(FAULT_TEST_BIN): $(ODIR)/utest_fault.o $(LIB)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

faulttest:
	$(MAKE) F=$(F) debug=1 FAULT=on run-faulttest

run-faulttest: $(FAULT_TEST_BIN)
	@echo "==> $(FAULT_TEST_BIN)" && $(FAULT_TEST_BIN)

# Two-process crash/recovery test (POSIX only). A real child process crashes
# with a hot journal; the parent recovers it. Links a tiny test-only C shim
# (procshim.c, fork/exec/waitpid) kept out of libsqr. Not in the default
# `utest` sweep (forks a subprocess, POSIX-only) — run explicitly.
PROC_TEST_BIN  := $(ODIR)/proctest$(EXT)
PROC_SHIM_OBJ  := $(ODIR)/procshim.o

$(PROC_SHIM_OBJ): $(TEST_DIR)/procshim.c | $(ODIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(PROC_TEST_BIN): $(ODIR)/proctest.o $(LIB) $(PROC_SHIM_OBJ)
	$(F) $(F_OPTS) -o $@ $< $(PROC_SHIM_OBJ) $(LIB) $(LFLAGS)

proctest: $(PROC_TEST_BIN)
	@echo "==> $(PROC_TEST_BIN)" && $(PROC_TEST_BIN)

# Performance benchmark. Always the production (FAULT=off) library and
# the default optimised build — never debug — so timings are
# representative. Recursive make pins F through unchanged.
$(BENCH_BIN): $(ODIR)/bench_sqr.o $(ODIR)/fb_monitor.o $(LIB)
	$(F) $(F_OPTS) -o $@ $< $(ODIR)/fb_monitor.o $(LIB) $(LFLAGS)

bench:
	$(MAKE) F=$(F) run-bench

run-bench: $(BENCH_BIN)
	@echo "==> $(BENCH_BIN)" && $(BENCH_BIN)

# Destruction testing: deliberate bad DATA, where faulttest injects bad I/O.
# Three corpora (on-disk corruption, .sqr container, SQL text) replay a few
# thousand malformed inputs through the public API and assert the engine
# always comes back with a status rather than crashing, hanging or running
# off a buffer. Debug build (-check all) so an out-of-bounds access is a
# diagnostic instead of silent; see destruct/README.md.
DESTRUCT_BINS := $(addprefix $(ODIR)/,mkdb$(EXT) hammer$(EXT) unpack_probe$(EXT) apiabuse$(EXT))
DESTRUCT_RUN  := $(ODIR)/destruct-run
# Seeds the blind-mutation sections of all three corpora.  The default keeps
# `make destruct` a reproducible regression run; override it
# (make destruct DESTRUCT_SEED=n) to reach cases the default never generates.
DESTRUCT_SEED ?= 20260806

$(ODIR)/mkdb$(EXT) $(ODIR)/hammer$(EXT) $(ODIR)/unpack_probe$(EXT) $(ODIR)/apiabuse$(EXT): \
  $(ODIR)/%$(EXT): $(ODIR)/%.o $(LIB)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

destruct:
	$(MAKE) F=$(F) debug=1 run-destruct

# The drivers work in the CWD, so they run inside a scratch directory and are
# handed absolute paths to the binaries they drive.
run-destruct: $(DESTRUCT_BINS) $(ODIR)/sqlsh$(EXT)
	@rm -rf $(DESTRUCT_RUN) && mkdir -p $(DESTRUCT_RUN)
	@cd $(DESTRUCT_RUN) && \
	  R=$(CURDIR) && B=$$R/$(ODIR) && \
	  export DESTRUCT_SEED=$(DESTRUCT_SEED) && \
	  echo "seed $$DESTRUCT_SEED" && \
	  $$B/mkdb$(EXT) pristine && \
	  echo "==> on-disk corruption" && \
	  HAMMER=$$B/hammer$(EXT) tclsh $$R/$(DESTRUCT_DIR)/corrupt.tcl pristine && \
	  rm -rf work work.pack work.unpacked && cp -a pristine work && \
	  $$B/hammer$(EXT) work > /dev/null && cp work.pack pristine.sqr && \
	  echo "==> .sqr container" && \
	  PROBE=$$B/unpack_probe$(EXT) tclsh $$R/$(DESTRUCT_DIR)/corrupt_pack.tcl pristine.sqr && \
	  echo "==> SQL text" && \
	  tclsh $$R/$(DESTRUCT_DIR)/sqlfuzz.tcl $$B/sqlsh$(EXT) pristine && \
	  echo "==> API arguments" && \
	  for k in 1 2 3 4 5 6 7 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37; do \
	      rm -rf wa && cp -a pristine wa && \
	      $$B/apiabuse$(EXT) wa $$k > /dev/null 2>&1 || echo "   api case $$k did not return"; \
	  done
	@echo "destruct: corpora complete (see $(DESTRUCT_RUN) for any findings)"

coverage: coverage-gcov

coverage-gcov:
	@rm -vf *.gcov obj_gfortran_debug/*.gcda obj_gfortran_debug/*.gcno obj_gfortran_debug/*.o obj_gfortran_debug/*.mod obj_gfortran_debug/*.smod obj_gfortran_debug/libsqr.a obj_gfortran_debug/utest_*_d
	$(MAKE) F=gfortran debug=1 FAULT=on F_EXTRA_GF='$(F_EXTRA_GF) --coverage' F_LOPTS_GF='$(F_LOPTS_GF) --coverage' utest run-faulttest
	@rm -vf *.gcov
	gcov -b -c -o obj_gfortran_debug $(addprefix $(SRC_DIR)/,$(LIB_SRC)) $(FAULT_DIR)/sqr_fault_on_sm.f90

coverage-clean:
	@rm -vf *.gcov obj_gfortran_debug/*.gcda obj_gfortran_debug/*.gcno

# --- Documentation (FORD) -------------------------------------------------
# Render the API docs from the sources + ford.md into ford_docs/ (output_dir
# is set in ford.md). ford_docs/ is generated, so it is unversioned and
# removed by distclean.
FORD     := ford
DOCS_DIR := ford_docs

docs:
	$(FORD) ford.md
	@echo "Docs in $(DOCS_DIR)/index.html"

docs-clean:
	@rm -vfr $(DOCS_DIR)

clean:
	@rm -vf $(ODIR)/*.o $(ODIR)/*.mod $(ODIR)/*.smod $(LIB) $(TEST_BIN) $(FAULT_TEST_BIN) $(PROC_TEST_BIN) $(PROC_SHIM_OBJ) $(BENCH_BIN) $(DESTRUCT_BINS) $(APP_BIN) $(REGEX_TEST_BIN) $(OPTIONS_FNAME) *~ *.mod *.smod

veryclean: clean
	@rm -vfr obj_* *.gcov

distclean: veryclean docs-clean
	@rm -vf depends.mk foptions_*.mk *~

# --- Windows cross-build (MinGW-w64, 64-bit) -------------------------------
# Cross-compile the library + the unit-test executables to standalone Windows
# .exe to exercise the Windows (osshim.c) branch (run them on Windows, or under
# wine). sqrsh is excluded — it links cmdgraph, which is not cross-built here.
#
# Two non-obvious points encoded below:
#   * The platform split now lives in osshim.c, compiled with the MinGW *C*
#     compiler, which DOES predefine _WIN32 — so no -D is needed anywhere and
#     the Fortran is compiled with no preprocessing at all. (The Win32
#     rename-replace semantics db_compact relies on come from MoveFileEx in
#     osshim.c, not from MinGW's POSIX-compat rename.)
#   * -static yields a self-contained .exe; -Wl,-u,__strcpy_chk -lssp resolves
#     a fortify symbol libgfortran's runtime pulls in (via its directory code)
#     under -static on this toolchain.
# 32-bit is intentionally not offered: the Win32 APIs are stdcall with
# @N-decorated names on i686, which a plain bind(c) (cdecl) interface does not
# match — that needs separate handling. 64-bit has one calling convention.
WIN_FC     := x86_64-w64-mingw32-gfortran
WIN_CC     := x86_64-w64-mingw32-gcc
WIN_AR     := x86_64-w64-mingw32-ar
WIN_ODIR   := obj_mingw64
WIN_FLAGS  := -O3
WIN_CFLAGS := -O2 -Wall -Wextra
# -lws2_32: Winsock, pulled in by the socket shim now in osshim.c/clib_wrap.
WIN_LINK   := -static -Wl,-u,__strcpy_chk -lssp -lws2_32
# Module/submodule order matters (parents before submodules).
WIN_SRC   := clib_wrap b_tree sqr_fault sqr_consts sqr clib_wrap_sm b_tree_sm sqr_fault_off_sm sqr_base sqr_table sqr_record sqr_index sqr_admin sqr_rowbuf sqr_journal sql sql_base sql_parse sql_exec sqr_net sqr_net_sm sqr_serve sqr_serve_sm
WIN_TESTS := utest_btree utest_sqr utest_sql

windows:
	@mkdir -p $(WIN_ODIR)
	@echo "  CC  osshim"
	$(WIN_CC) $(WIN_CFLAGS) -c $(SRC_DIR)/osshim.c -o $(WIN_ODIR)/osshim.o
	@for f in $(WIN_SRC); do \
	    echo "  FC  $$f"; \
	    $(WIN_FC) $(WIN_FLAGS) -J$(WIN_ODIR) -I$(WIN_ODIR) \
	        -c $(SRC_DIR)/$$f.f90 -o $(WIN_ODIR)/$$f.o || exit 1; \
	done
	$(WIN_AR) rcs $(WIN_ODIR)/libsqr.a $(WIN_ODIR)/*.o
	@for t in $(WIN_TESTS); do \
	    echo "  LD  $$t.exe"; \
	    $(WIN_FC) $(WIN_FLAGS) -J$(WIN_ODIR) -I$(WIN_ODIR) -o $(WIN_ODIR)/$$t.exe \
	        $(TEST_DIR)/$$t.f90 $(WIN_ODIR)/libsqr.a $(WIN_LINK) || exit 1; \
	done
	@echo "Built standalone Windows 64-bit exes in $(WIN_ODIR)/ (run on Windows or via wine):"
	@for t in $(WIN_TESTS); do echo "    $(WIN_ODIR)/$$t.exe"; done

help:
	@echo "Targets : all, check (depcheck utest sqlttest shelltest), depcheck"
	@echo "          utest, sqlttest, shelltest, sqrdtest, calctest, faulttest, destruct, bench"
	@echo "          install, uninstall (PREFIX=$(PREFIX)), install-calc"
	@echo "          clean, veryclean, distclean"
	@echo "          coverage, coverage-gcov, coverage-clean, docs, docs-clean, windows"
	@echo "          sqrsh-regex, test-regex (opt-in DT_CHAR regex search via tcl_re)"
	@echo "Options : F=gfortran|ifx|lfortran|flang (default ifx)  debug=1  valgrind=1 (ifx: AVX2 cap)"
	@echo "LIB_SRC = $(LIB_SRC)"
	@echo "TEST_BIN= $(TEST_BIN)"
	@echo "APP_BIN = $(APP_BIN)"
	@echo "ODIR    = $(ODIR)"
	@echo "F_OPTS  = $(F_OPTS)"
