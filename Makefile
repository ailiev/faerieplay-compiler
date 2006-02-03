GHCFLAGS =  -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

GHCFLAGS += -I$(HOME)/code/haskell
GHCFLAGS += -I$(HOME)/work/code/lib/haskell

#GHCFLAGS += -prof -auto-all
#GHCFLAGS += -v
GHCFLAGS += -debug


PACKS = -package fgl -package Cabal

all: sfdlc

bnfc:
	bnfc -haskell -d SFDL.cf
	$(MAKE) -C SFDL

sfdlc:
	HFLAGS="$(GHCFLAGS)" hmake -ghc $(PACKS) sfdlc
#	ghc --make $(GHCFLAGS) sfdlc

# this make shouldnt look at the sfdlc file
.PHONY: sfdlc

hat:
	PATH=$(PATH):$(HOME)/minime/hat/bin hmake -ghc -hat $(GHCFLAGS) $(PACKS) Generate

clean:
	HFLAGS="$(GHCFLAGS)" hmake -clean sfdlc

install: sfdlc
	install sfdlc ~/leeds_root/bin/

%.cct: %.sfdl
	sfdlc $< -o $@

# to make postscript of a circuit (or any) gviz file:
#  dot -Tps cct.gviz -o cct.ps
