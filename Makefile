GHCFLAGS =  -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

GHCFLAGS += -I$(HOME)/code/haskell
GHCFLAGS += -I$(HOME)/work/code/lib/haskell

#GHCFLAGS += -prof -auto-all
#GHCFLAGS += -v
GHCFLAGS += -debug


PACKS = -package fgl -package Cabal

all: ghc

bnfc:
	bnfc -haskell -d SFDL.cf
	$(MAKE) -C SFDL

ghc:
	HFLAGS="$(GHCFLAGS)" hmake -ghc $(PACKS) Generate
#	ghc --make $(GHCFLAGS) Generate

hat:
	PATH=$(PATH):$(HOME)/minime/hat/bin hmake -ghc -hat $(GHCFLAGS) $(PACKS) Generate

clean:
	HFLAGS="$(GHCFLAGS)" hmake -clean Generate

# hmake:
# hmake -package fgl -package Cabal -fglasgow-exts -fallow-overlapping-instances -I$HOME/work/code/lib/haskell -I$HOME/code/haskell Generate

# to make postscript of a circuit:
#  dot -Tps cct.gviz -o cct.ps
