all: sfdlc.info syntax

syntax: SFDL/Doc.pdf SFDL_C/Doc.pdf

vpath %.tex ../

%.pdf: %.tex
	rubber --into $(shell dirname $@) --pdf $<

sfdlc.info: sfdlc.texi values.texi version.texi
	makeinfo $<

%.pdf: %.texi
	texi2dvi --pdf $<

%.html: %.texi
	texi2html $<
