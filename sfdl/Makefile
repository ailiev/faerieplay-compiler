all:
	happy -gcai Par.y
	alex -g Lex.x
#	latex Doc.tex
#	dvips Doc.dvi -o Doc.ps
#	(cd ..; ghc --make SFDL/Test.hs -o SFDL/Test)
clean:
	 rm -f *.log *.aux *.hi *.o *.dvi Doc.ps *.o
distclean: 
	 rm -rf ../SFDL
