all:
	bnfc -m -haskell -d SFDL.cf
	$(MAKE) -C SFDL
