all: sfdlc.info

%.info: %.texi
	makeinfo $<

%.pdf: %.texi
	texi2dvi --pdf $<
