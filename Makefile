GHCFLAGS = -v0 -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

GHCFLAGS += -i$(HOME)/code/haskell
GHCFLAGS += -i$(HOME)/work/code/lib/haskell

all:
	bnfc -haskell -d SFDL.cf
	$(MAKE) -C SFDL

ghc:
	ghc $(GHCFLAGS) \
	--make -o gen Generate.hs


# to make postscript of a circuit:
#  dot -Tps cct.gviz -o cct.ps
