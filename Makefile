GHCFLAGS = -v0

all:
	bnfc -haskell -d SFDL.cf
	$(MAKE) -C SFDL

ghc:
	ghc $(GHCFLAGS) -fglasgow-exts -fallow-overlapping-instances \
	-i$(HOME)/code/haskell \
	--make -o gen Generate.hs

