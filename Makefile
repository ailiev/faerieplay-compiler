all:
	bnfc -haskell -d SFDL.cf
	$(MAKE) -C SFDL
