GHCFLAGS =  -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

# GHCFLAGS += -I$(HOME)/code/haskell
# GHCFLAGS += -I$(HOME)/work/code/lib/haskell

GHCFLAGS += -v0

GHCFLAGS += -odir $(ODIR) -hidir $(ODIR) 

# GHCFLAGS += -static

# GHCFLAGS += -Wall

# GHC = /home/sasho/minime/ghc/ghc-6.4.2/bin/i386-unknown-linux/ghc
GHC = $(HOME)/work/minime/ghc-6.6-RC2-sasho/bin/ghc
# GHC = ghc

ODIR = build


include ../sfdl/shared.make
SFDLCFLAGS += +RTS -xc -RTS


#
# which build?
#

# BUMMER: the multiple-else syntax does not appear to work with make version
# 3.80 which is in all Fedora's now (Sept 2006)
ifdef OPT
	ODIR := $(ODIR)/opt
	GHCFLAGS += -O2
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

else		    # non-optimized, non-debug, non-profiled, quickest to compile
	ODIR := $(ODIR)/default
endif




PACKS = -package fgl -package Cabal

all: $(ODIR)/sfdlc

# basename of our grammar (.cf) file
CF_ROOT = SFDL_C

BNFCROOTS=Abs Lex Print Test ErrM Par Skel
bnfc_files=$(patsubst %,$(CF_ROOT)/%.hs,$(word 1,$(BNFCROOTS)))
# bnfc produces a misnamed Makefile, hence specifying that name here.
$(bnfc_files): $(CF_ROOT).cf
	bnfc -haskell -m -d $<
	ed $(CF_ROOT)/Abs.hs < add-bnfc-derives.ed
	make -C $(CF_ROOT)

bnfc: $(bnfc_files)


%.o: %.hs
	$(GHC) -c $(GHCFLAGS) $<


$(ODIR)/%.o: %.hs
#	HFLAGS="$(GHCFLAGS)" hmake -ghc $(PACKS) $<
	$(GHC) -c $(GHCFLAGS) $<

%.hi: %.hs
	$(GHC) -E -ddump-minimal-imports $(GHCFLAGS) $< > $@

$(ODIR)/sfdlc:
#	HFLAGS="$(GHCFLAGS)" hmake -d$(ODIR) -ghc $(PACKS) sfdlc
	$(GHC) --make $(GHCFLAGS) -o $@ sfdlc.hs

# this make shouldnt look at the sfdlc file, hmake or ghc do that.
.PHONY: $(ODIR)/sfdlc bnfc

hat:
	PATH=$(PATH):$(HOME)/minime/hat/bin hmake -ghc -hat $(GHCFLAGS) $(PACKS) Generate

clean:
	HFLAGS="$(GHCFLAGS)" hmake -d$(ODIR) -clean sfdlc

install: $(ODIR)/sfdlc
	install -p $(ODIR)/sfdlc ~/leeds_root/bin/

tags: TAGS

TAGS: $(wildcard *.hs) $(wildcard SFDL/*.hs)
	hasktags6 --etags $^

# to make postscript of a circuit (or any) gviz file:
#  dot -Tps cct.gviz -o cct.ps
