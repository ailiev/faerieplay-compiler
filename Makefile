include config.make

GHCFLAGS =  -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

GHCFLAGS += -v0

GHCFLAGS += -odir $(ODIR) -hidir $(ODIR) 

# GHCFLAGS += -static

# GHCFLAGS += -Wall

ODIR = build

# for actually running the Faerieplay compiler, if a user of this makefile wants
# to do that.
include ../sfdl/shared.make
SFDLCFLAGS += +RTS -xc -RTS

ifeq ($(source_lang),Sfdl)
	GHCFLAGS += -DSYNTAX_SFDL
else ifeq ($(source_lang),Fcpp)
	GHCFLAGS += -DSYNTAX_C
else
$(error No recognized source language defined in make variable 'source_lang')
endif


#
# which build?
#

# BUMMER: the multiple-else syntax does not appear to work with make version
# 3.80 which is in all Fedora's now (Sept 2006)

ifdef NOOPT	    # non-optimized, fast build.
	ODIR := $(ODIR)/default
else ifdef PROF
	ODIR := $(ODIR)/prof
	GHCFLAGS += -prof -auto-all -O
else ifdef DBG			
	ODIR := $(ODIR)/dbg

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

	ODIR := $(ODIR)/opt
	GHCFLAGS += -O2
endif




PACKS = -package fgl -package Cabal


# package  where BNFC-generated files go.
BNFC_PACKAGE_ROOT = Faerieplay.Bnfc

BNFC_LANG_PACKAGE_ROOT = $(BNFC_PACKAGE_ROOT).$(source_lang)

BNFC_LANG_DIR = $(subst .,/,$(BNFC_LANG_PACKAGE_ROOT))

EXE = $(ODIR)/sfdlc

all: $(VERSFILE) $(EXE)
#	@echo srcs = $(SRCS)
#	@echo vers file = $(VERSFILE)

$(EXE):
#	HFLAGS="$(GHCFLAGS)" hmake -d$(ODIR) -ghc $(PACKS) sfdlc
	$(GHC) --make $(GHCFLAGS) -o $@ Faerieplay/sfdlc.hs


SRCS = $(shell find $(CURDIR)/Faerieplay -name '*.hs' -o -name '*.cf') 



##############################
## version management
##############################
versions: $(VERSFILE)

.PHONY: versions

VERSFILE = $(CURDIR)/Faerieplay/Version.hs

$(VERSFILE): $(VERSFILE).tok $(SRCS)
	$(CURDIR)/update-versions.pl < $(VERSFILE).tok > $(VERSFILE) $^






# Now using BNFC 2.3b features, though only for artifact layout.
BNFCROOTS=Abs Lex Print Test ErrM Par Skel
bnfc_files=$(patsubst %,$(BNFC_LANG_DIR)/%.hs,$(word 1,$(BNFCROOTS)))
$(bnfc_files): $(source_lang).cf
	bnfc -haskell -d -p $(BNFC_PACKAGE_ROOT) $<
	happy -gca $(BNFC_LANG_DIR)/Par.y
	alex -g $(BNFC_LANG_DIR)/Lex.x
#	(cd $(BNFC_LANG_DIR); latex Doc.tex; dvips Doc.dvi -o Doc.ps)
	ed $(BNFC_LANG_DIR)/Abs.hs < add-bnfc-derives.ed
#	ghc --make $(BNFC_LANG_DIR)/Test.hs -o Faerieplay/Bnfc/Sfdl/Test

bnfc: $(bnfc_files)



%.o: %.hs
	$(GHC) -c $(GHCFLAGS) $<


$(ODIR)/%.o: %.hs
#	HFLAGS="$(GHCFLAGS)" hmake -ghc $(PACKS) $<
	$(GHC) -c $(GHCFLAGS) $<

%.hi: %.hs
	$(GHC) -E -ddump-minimal-imports $(GHCFLAGS) $< > $@


# this make shouldnt look at the sfdlc file, hmake or ghc do that.
.PHONY: $(EXE) bnfc

clean:
	HFLAGS="$(GHCFLAGS)" hmake -package fgl -d$(ODIR) -clean sfdlc

install: $(ODIR)/sfdlc
	install -p $(ODIR)/sfdlc $(DIST_ROOT)/bin/

tags: TAGS

TAGS: $(SRCS)
	hasktags6 --etags $^

# to make postscript of a circuit (or any) gviz file:
#  dot -Tps cct.gviz -o cct.ps
