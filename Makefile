.PHONY: all clean veryclean distclean utest sqlttest faulttest run-faulttest bench run-bench coverage coverage-gcov coverage-clean docs docs-clean help windows win-build sqrsh-regex test-regex
.SUFFIXES:
.DEFAULT_GOAL := all

# Source layout
SRC_DIR   := src
APP_DIR   := app
TEST_DIR  := test
REGEX_DIR := regex
FAULT_DIR := fault
BENCH_DIR := bench

# Fault-injection variant (sqr_fault submodule). off = production
# (zero machinery, shared with fpm); on = coverage/fault test only.
FAULT ?= off

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

F_OPTS := $(F_BASE) $(F_BUILD) $(MOD_OPTS) -I$(SRC_DIR)
LFLAGS := $(F_LOPTS)

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
LIB_SRC := clib_wrap.f90 clib_wrap_sm.f90 b_tree.f90 b_tree_sm.f90 sqr_fault.f90 sqr.f90 sqr_base.f90 sqr_table.f90 sqr_record.f90 sqr_index.f90 sqr_admin.f90 sqr_rowbuf.f90 sqr_journal.f90 sql.f90 sql_base.f90 sql_parse.f90 sql_exec.f90 dlist.f90 dlist_sm.f90 cmdgraph.f90 cmdgraph_sm.f90

# Selected fault submodule: off -> $(SRC_DIR) (production, fpm-shared);
# on -> $(FAULT_DIR) (Make-only coverage/faulttest, never seen by fpm).
ifeq ($(FAULT),on)
  FAULT_SM_SRC := $(FAULT_DIR)/sqr_fault_on_sm.f90
else
  FAULT_SM_SRC := $(SRC_DIR)/sqr_fault_off_sm.f90
endif
FAULT_SM_OBJ := $(ODIR)/sqr_fault_$(FAULT)_sm.o

LIB_OBJ := $(addprefix $(ODIR)/,$(LIB_SRC:.f90=.o)) $(FAULT_SM_OBJ)
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
# SQL-subset REPL — depends only on the library, not cmdgraph).
APP_SRC := $(wildcard $(APP_DIR)/sqrsh.f90 $(APP_DIR)/sqlsh.f90)
APP_BIN := $(patsubst $(APP_DIR)/%.f90,$(ODIR)/%$(EXT),$(APP_SRC))

all: $(OPTIONS_FNAME) $(LIB) $(TEST_BIN) $(APP_BIN)

# --- Module dependencies (auto-generated) ---
depends.mk: fortran_deps.tcl $(addprefix $(SRC_DIR)/,$(LIB_SRC)) $(TEST_SRC) $(APP_SRC)
	tclsh $< $(SRC_DIR) $@ $(LIB_SRC) $(notdir $(TEST_SRC)) $(notdir $(APP_SRC))

-include depends.mk

# --- Compilation rules ---
$(ODIR)/clib_wrap.o $(ODIR)/clib_wrap.mod &: $(SRC_DIR)/clib_wrap.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/clib_wrap.o $<
	@touch $(ODIR)/clib_wrap.mod

$(ODIR)/clib_wrap_sm.o: $(SRC_DIR)/clib_wrap_sm.f90 $(ODIR)/clib_wrap.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/b_tree.o $(ODIR)/b_tree.mod &: $(SRC_DIR)/b_tree.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/b_tree.o $<
	@touch $(ODIR)/b_tree.mod

$(ODIR)/b_tree_sm.o: $(SRC_DIR)/b_tree_sm.f90 $(ODIR)/b_tree.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr.o $(ODIR)/sqr.mod &: $(SRC_DIR)/sqr.f90 $(ODIR)/b_tree.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/sqr.o $<
	@touch $(ODIR)/sqr.mod

$(ODIR)/sqr_fault.o $(ODIR)/sqr_fault.mod &: $(SRC_DIR)/sqr_fault.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/sqr_fault.o $<
	@touch $(ODIR)/sqr_fault.mod

$(ODIR)/sqr_fault_off_sm.o: $(SRC_DIR)/sqr_fault_off_sm.f90 $(ODIR)/sqr_fault.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr_fault_on_sm.o: $(FAULT_DIR)/sqr_fault_on_sm.f90 $(ODIR)/sqr_fault.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

# sqr_base is the intermediate submodule of sqr holding shared engine
# internals; the feature submodules are its descendants and consume
# its submodule .smod (compiler-specific name, e.g. sqr@sqr_base.smod), so they
# must compile after sqr_base. The .o prerequisite enforces that order portably
# without naming the .smod.
$(ODIR)/sqr_base.o: $(SRC_DIR)/sqr_base.f90 $(ODIR)/sqr.mod $(ODIR)/b_tree.mod $(ODIR)/clib_wrap.mod $(ODIR)/sqr_fault.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr_admin.o: $(SRC_DIR)/sqr_admin.f90 $(ODIR)/sqr.mod $(ODIR)/sqr_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr_table.o: $(SRC_DIR)/sqr_table.f90 $(ODIR)/sqr.mod $(ODIR)/clib_wrap.mod $(ODIR)/sqr_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr_record.o: $(SRC_DIR)/sqr_record.f90 $(ODIR)/sqr.mod $(ODIR)/sqr_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr_index.o: $(SRC_DIR)/sqr_index.f90 $(ODIR)/sqr.mod $(ODIR)/sqr_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr_rowbuf.o: $(SRC_DIR)/sqr_rowbuf.f90 $(ODIR)/sqr.mod $(ODIR)/sqr_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sqr_journal.o: $(SRC_DIR)/sqr_journal.f90 $(ODIR)/sqr.mod $(ODIR)/clib_wrap.mod $(ODIR)/sqr_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

# --- SQL front-end layer (lexer/parser/executor; depends on sqr, not vice
# versa). sql_base is the intermediate submodule holding shared helpers + the
# lexer; sql_parse / sql_exec are its descendants and consume its submodule
# .smod, so they compile after sql_base (enforced via the .o prerequisite, as
# with sqr_base).
$(ODIR)/sql.o $(ODIR)/sql.mod &: $(SRC_DIR)/sql.f90 $(ODIR)/sqr.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/sql.o $<
	@touch $(ODIR)/sql.mod

$(ODIR)/sql_base.o: $(SRC_DIR)/sql_base.f90 $(ODIR)/sql.mod $(ODIR)/sqr.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sql_parse.o: $(SRC_DIR)/sql_parse.f90 $(ODIR)/sql.mod $(ODIR)/sql_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/sql_exec.o: $(SRC_DIR)/sql_exec.f90 $(ODIR)/sql.mod $(ODIR)/sqr.mod $(ODIR)/sql_base.o | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

# --- Vendored cmdgraph engine (github.com/sgeard/cmdgraph). dlist is the
# doubly-linked list cmdgraph uses; cmdgraph `use`s dlist, so dlist builds first.
$(ODIR)/dlist.o $(ODIR)/dlist.mod &: $(SRC_DIR)/dlist.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/dlist.o $<
	@touch $(ODIR)/dlist.mod

$(ODIR)/dlist_sm.o: $(SRC_DIR)/dlist_sm.f90 $(ODIR)/dlist.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR)/cmdgraph.o $(ODIR)/cmdgraph.mod &: $(SRC_DIR)/cmdgraph.f90 $(ODIR)/dlist.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $(ODIR)/cmdgraph.o $<
	@touch $(ODIR)/cmdgraph.mod

$(ODIR)/cmdgraph_sm.o: $(SRC_DIR)/cmdgraph_sm.f90 $(ODIR)/cmdgraph.mod | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

# Library archive. Rebuilt from scratch (not `ar r` onto a stale archive):
# the FAULT=off and FAULT=on fault submodules have distinct object names, so
# adding to an existing archive could leave BOTH in it and let the linker
# resolve io_check from the wrong one (silently disabling fault injection).
$(LIB): $(LIB_OBJ) | $(ODIR)
	@rm -f $@
	$(AR) rcs $@ $(LIB_OBJ)

# Tests
$(ODIR)/utest_%$(EXT): $(TEST_DIR)/utest_%.f90 $(LIB) | $(ODIR)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

# Shell — the cmdgraph engine is vendored into libsqr.a, so just link the lib.
$(ODIR)/sqrsh$(EXT): $(APP_DIR)/sqrsh.f90 $(LIB) | $(ODIR)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

# SQL REPL — links the library only (the sql_* front-end is in libsqr); no
# cmdgraph dependency.
$(ODIR)/sqlsh$(EXT): $(APP_DIR)/sqlsh.f90 $(LIB) | $(ODIR)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

# --- Optional regex-search shell (opt-in) ---------------------------------
# `make sqrsh-regex` rebuilds the shell with the `match <col> <regex>` command
# (regex scan over DT_CHAR columns). The match action lives in
# $(REGEX_DIR)/sqrsh_regex.f90 and is linked in here; app/sqrsh.f90 calls it as
# an external procedure under #ifdef SQR_WITH_REGEX. It links the sibling
# tcl_re project's self-contained static engine (libtclInterface.a — bundles
# the regex engine, no libtcl). All of $(REGEX_DIR) is kept out of the default
# build and out of the dirs fpm globs, so fpm and the Windows cross-build stay
# dependency-free; tcl_re cannot be built by fpm in any case.
# tcl_re shares the standardised build, so its ODIR matches $(ODIR).
TCLRE_DIR  ?= ../tcl_re
TCLRE_ODIR ?= $(TCLRE_DIR)/$(ODIR)
TCLRE_LIB  ?= $(TCLRE_ODIR)/libtclInterface.a

$(TCLRE_LIB):
	$(MAKE) -C $(TCLRE_DIR) F=$(F)

sqrsh-regex: $(OPTIONS_FNAME) $(LIB) $(TCLRE_LIB) | $(ODIR)
	$(F) $(F_OPTS) -DSQR_WITH_REGEX -I$(TCLRE_ODIR) -o $(ODIR)/sqrsh$(EXT) \
	    $(APP_DIR)/sqrsh.f90 $(REGEX_DIR)/sqrsh_regex.f90 \
	    $(LIB) $(TCLRE_LIB) $(LFLAGS)
	@echo "Built $(ODIR)/sqrsh$(EXT) with regex search (match command)"

# Functional test for the regex search (opt-in, links tcl_re).
REGEX_TEST_BIN := $(ODIR)/utest_match$(EXT)
test-regex: $(OPTIONS_FNAME) $(LIB) $(TCLRE_LIB) | $(ODIR)
	$(F) $(F_OPTS) -I$(TCLRE_ODIR) -o $(REGEX_TEST_BIN) \
	    $(REGEX_DIR)/utest_match.f90 $(LIB) $(TCLRE_LIB) $(LFLAGS)
	@echo "==> $(REGEX_TEST_BIN)" && $(REGEX_TEST_BIN)

$(ODIR):
	mkdir -p $@

utest: $(TEST_BIN)
	@for t in $(TEST_BIN); do echo "==> $$t" && $$t || exit 1; done

# sqllogictest-subset functional tests: a Tcl harness (no tcllib; uses the
# system md5sum) replays sqlt/tests/*.test records through the sqlsh REPL and
# diffs results. Black-box, complements the utest_sql unit tests. See sqlt/README.md.
SQLT_DIR    := sqlt
SQLT_TESTS  := $(wildcard $(SQLT_DIR)/tests/*.test)
sqlttest: $(ODIR)/sqlsh$(EXT)
	tclsh $(SQLT_DIR)/run_sqlt.tcl $(ODIR)/sqlsh$(EXT) $(SQLT_TESTS)

# Fault-injection sweep. Built only here (and by coverage), always with
# FAULT=on, into a debug ODIR so the production release archive is never
# overwritten with the on submodule.
$(FAULT_TEST_BIN): $(FAULT_TEST_SRC) $(LIB) | $(ODIR)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

faulttest:
	$(MAKE) F=$(F) debug=1 FAULT=on run-faulttest

run-faulttest: $(FAULT_TEST_BIN)
	@echo "==> $(FAULT_TEST_BIN)" && $(FAULT_TEST_BIN)

# Performance benchmark. Always the production (FAULT=off) library and
# the default optimised build — never debug — so timings are
# representative. Recursive make pins F through unchanged.
$(BENCH_BIN): $(BENCH_SRC) $(LIB) | $(ODIR)
	$(F) $(F_OPTS) -o $@ $< $(LIB) $(LFLAGS)

bench:
	$(MAKE) F=$(F) run-bench

run-bench: $(BENCH_BIN)
	@echo "==> $(BENCH_BIN)" && $(BENCH_BIN)

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
	@rm -vf $(ODIR)/*.o $(ODIR)/*.mod $(ODIR)/*.smod $(LIB) $(TEST_BIN) $(FAULT_TEST_BIN) $(BENCH_BIN) $(APP_BIN) depends.mk $(OPTIONS_FNAME) *~ *.mod *.smod

veryclean: clean
	@rm -vfr obj_* *.gcov

distclean: veryclean docs-clean
	@rm -vf depends.mk foptions_*.mk *~

# --- Windows cross-build (MinGW-w64, 64-bit) -------------------------------
# Cross-compile the library + the unit-test executables to standalone Windows
# .exe to exercise the Windows clib_wrap branch (run them on Windows, or under
# wine). sqrsh is excluded — its cmdgraph engine is not part of this test build.
#
# Two non-obvious points encoded below:
#   * MinGW gfortran's Fortran preprocessor does NOT predefine _WIN32 (that is
#     a C-preprocessor macro), so -D_WIN32 is passed explicitly to select the
#     Windows branch. Without it the POSIX branch compiles and links against
#     MinGW's POSIX-compat shims, but Windows rename() does not replace an open
#     target, so db_compact fails.
#   * -static yields a self-contained .exe; -Wl,-u,__strcpy_chk -lssp resolves
#     a fortify symbol libgfortran's runtime pulls in (via its directory code)
#     under -static on this toolchain.
# 32-bit is intentionally not offered: the Win32 APIs are stdcall with
# @N-decorated names on i686, which a plain bind(c) (cdecl) interface does not
# match — that needs separate handling. 64-bit has one calling convention.
WIN_FC    := x86_64-w64-mingw32-gfortran
WIN_AR    := x86_64-w64-mingw32-ar
WIN_ODIR  := obj_mingw64
WIN_FLAGS := -cpp -D_WIN32 -O3
WIN_LINK  := -static -Wl,-u,__strcpy_chk -lssp
# Module/submodule order matters (parents before submodules).
WIN_SRC   := clib_wrap b_tree sqr_fault sqr clib_wrap_sm b_tree_sm sqr_fault_off_sm sqr_base sqr_table sqr_record sqr_index sqr_admin sqr_rowbuf sqr_journal sql sql_base sql_parse sql_exec
WIN_TESTS := utest_btree utest_sqr utest_sql

windows:
	@mkdir -p $(WIN_ODIR)
	@for f in $(WIN_SRC); do \
	    echo "  FC  $$f"; \
	    $(WIN_FC) $(WIN_FLAGS) -J$(WIN_ODIR) -I$(WIN_ODIR) \
	        -c $(SRC_DIR)/$$f.f90 -o $(WIN_ODIR)/$$f.o || exit 1; \
	done
	$(WIN_AR) rcs $(WIN_ODIR)/libsqr.a $(WIN_ODIR)/*.o
	@for t in $(WIN_TESTS); do \
	    echo "  LD  $$t.exe"; \
	    $(WIN_FC) $(WIN_FLAGS) -I$(WIN_ODIR) -o $(WIN_ODIR)/$$t.exe \
	        $(TEST_DIR)/$$t.f90 $(WIN_ODIR)/libsqr.a $(WIN_LINK) || exit 1; \
	done
	@echo "Built standalone Windows 64-bit exes in $(WIN_ODIR)/ (run on Windows or via wine):"
	@for t in $(WIN_TESTS); do echo "    $(WIN_ODIR)/$$t.exe"; done

help:
	@echo "Targets : all, utest, sqlttest, faulttest, bench, clean, veryclean, distclean"
	@echo "          coverage, coverage-gcov, coverage-clean, docs, docs-clean, windows"
	@echo "          sqrsh-regex, test-regex (opt-in DT_CHAR regex search via tcl_re)"
	@echo "Options : F=gfortran|ifx|lfortran|flang (default ifx)  debug=1  valgrind=1 (ifx: AVX2 cap)"
	@echo "LIB_SRC = $(LIB_SRC)"
	@echo "TEST_BIN= $(TEST_BIN)"
	@echo "APP_BIN = $(APP_BIN)"
	@echo "ODIR    = $(ODIR)"
	@echo "F_OPTS  = $(F_OPTS)"
