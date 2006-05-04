GHCFLAGS =  -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

GHCFLAGS += -I$(HOME)/code/haskell
GHCFLAGS += -I$(HOME)/work/code/lib/haskell

# GHCFLAGS += -O2

# this compiles a profiling executable, which can be run with:
# +RTS -xc -RTS
# options to have it dump a cost-center stack if a runtime error occurs.
# 
# see also section 5.1.1 "Inserting cost centres by hand" for how to add cost
# centers for more detailed stack reports.
GHCFLAGS += -prof -auto-all

#GHCFLAGS += -v

# from the manual:
# 
# Link the program with a debugging version of the runtime system. The debugging
# runtime turns on numerous assertions and sanity checks, and provides extra
# options for producing debugging output at runtime (run the program with +RTS
# -? to see a list).
GHCFLAGS += -debug


PACKS = -package fgl -package Cabal

all: sfdlc

# basename of our grammar (.cf) file
CF_ROOT = SFDL

BNFCROOTS=Abs Lex Print Test ErrM Par Skel
bnfc_files=$(patsubst %,$(CF_ROOT)/%.hs,$(BNFCROOTS))
# bnfc produces a misnamed Makefile, hence specifying that name here.
$(bnfc_files): $(CF_ROOT).cf
	bnfc -haskell -m -d $<
	make -C SFDL -f Makefile. 

bnfc: $(bnfc_files)

%.o: %.hs
	HFLAGS="$(GHCFLAGS)" hmake -ghc $(PACKS) $<

sfdlc:
#	HFLAGS="$(GHCFLAGS)" hmake -ghc $(PACKS) sfdlc
	ghc --make $(GHCFLAGS) -o $@ sfdlc.hs

# this make shouldnt look at the sfdlc file, hmake or ghc do that.
.PHONY: sfdlc bnfc

hat:
	PATH=$(PATH):$(HOME)/minime/hat/bin hmake -ghc -hat $(GHCFLAGS) $(PACKS) Generate

clean:
	HFLAGS="$(GHCFLAGS)" hmake -clean sfdlc

install: sfdlc
	install -p sfdlc ~/leeds_root/bin/

tags: TAGS

TAGS: $(wildcard *.hs)
	hasktags6 --etags $^

# to make postscript of a circuit (or any) gviz file:
#  dot -Tps cct.gviz -o cct.ps
