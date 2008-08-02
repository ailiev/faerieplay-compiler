all: sfdlc.info sfdlc.html syntax

syntax: SFDL/Doc.pdf SFDL_C/Doc.pdf

vpath %.tex ../

%.pdf: %.tex
	rubber --into $(shell dirname $@) --pdf $<

sfdlc.info: sfdlc.texi values.texi version.texi
	makeinfo $<

%.pdf: %.texi
	texi2dvi --pdf $<

# Want this step to be optional
%.html: %.texi
	texi2html $< || true

clean:
	$(RM) sfdlc.info sfdlc.html
