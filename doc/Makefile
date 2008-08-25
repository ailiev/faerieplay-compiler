all: sfdlc.info sfdlc.html


# not trying to build the syntax docs from here for now.
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
