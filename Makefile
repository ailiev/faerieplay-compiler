all:
	bnfc -haskell -d SFDL.cf
	$(MAKE) -C SFDL

ghc:
	ghc -fglasgow-exts -fallow-overlapping-instances -i$(HOME)/code/haskell --make -o gen Generate.hs