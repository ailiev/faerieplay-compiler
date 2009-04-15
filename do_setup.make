# -*- makefile -*-

# Sets many variables for the build of the Faerieplay compiler.

#GHCFLAGS += -L$(HOME)/minime/x64/lib
GHCFLAGS += -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

GHCFLAGS += -v0

GHCFLAGS += -odir $(ODIR) -hidir $(ODIR) 

# GHCFLAGS += -static

# GHCFLAGS += -Wall

build_root = build_$(source_lang)

# for actually running the Faerieplay compiler, if a user of this makefile wants
# to do that.
include ../sfdl/shared.make
SFDLCFLAGS += +RTS -xc -RTS

ifeq ($(source_lang),Sfdl)
	GHCFLAGS += -DSYNTAX_SFDL
	exe_file = sfdlc
else ifeq ($(source_lang),Fcpp)
	GHCFLAGS += -DSYNTAX_C
	exe_file = fc++
else
$(error No recognized source language defined in make variable 'source_lang')
endif


#
# which build?
#

# BUMMER: the multiple-else syntax does not appear to work with make version
# 3.80 which is in all Fedora's now (Sept 2006)

ifdef NOOPT	    # non-optimized, fast build.
	ODIR = $(build_root)/noopt
else ifdef PROF
	ODIR = $(build_root)/prof
	GHCFLAGS += -prof -auto-all -O
else ifdef DBG			
	ODIR = $(build_root)/dbg

	GHCFLAGS += -DTRACE

# this compiles a profiling executable, which can be run with:
# +RTS -xc -RTS
# options to have it dump a cost-center stack if a runtime error occurs.
# 
# see also section 5.1.1 "Inserting cost centres by hand" for how to add cost
# centers for more detailed stack reports.
# GHCFLAGS += -prof -auto-all
	GHCFLAGS += -prof -auto -auto-all
#	GHCFLAGS += -O

# from the manual:
# 
# Link the program with a debugging version of the runtime system. The debugging
# runtime turns on numerous assertions and sanity checks, and provides extra
# options for producing debugging output at runtime (run the program with +RTS
# -? to see a list).
#	GHCFLAGS += -debug

else		    # by default, optimized build. quite slow to compile.
$(info Running an optimized build, will be quite slow. Use NOOPT=1 for a non-optimized but much faster build)

	ODIR = $(build_root)/opt
	GHCFLAGS += -O2
endif




PACKS = -package fgl -package Cabal


# package  where BNFC-generated files go.
BNFC_PACKAGE_ROOT = Faerieplay.Bnfc

BNFC_LANG_PACKAGE_ROOT = $(BNFC_PACKAGE_ROOT).$(source_lang)

BNFC_LANG_DIR = $(subst .,/,$(BNFC_LANG_PACKAGE_ROOT))
