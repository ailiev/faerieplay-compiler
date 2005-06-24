GHCFLAGS = -v0 -fglasgow-exts
GHCFLAGS += -fallow-overlapping-instances

all:
	bnfc -haskell -d SFDL.cf
	$(MAKE) -C SFDL

ghc:
	ghc $(GHCFLAGS) \
	-i$(HOME)/code/haskell \
	--make -o gen Generate.hs

