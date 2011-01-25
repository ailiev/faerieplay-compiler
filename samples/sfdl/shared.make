# -*- makefile -*-

# this reading of MAKEFILE_LIST has to be done before any other files are
# include-d!
this_file := $(lastword $(MAKEFILE_LIST))
this_dir := $(dir $(realpath $(this_file)))


SRCEXT=sfdl
# SRCEXT=c

%.cct: %.$(SRCEXT)
	sfdlc $(SFDLCFLAGS) --dump-graph -c $<

%.gates: %.$(SRCEXT)
	sfdlc $(SFDLCFLAGS) --dump-gates -c $<

%.runtime: %.$(SRCEXT)
	sfdlc $(SFDLCFLAGS) -c $< -o $@

%.udg: %.cct
	sfdlc $(SFDLCFLAGS) --mkgraph $< -o $@

%.run: %.gates
	sfdlc $(SFDLCFLAGS) --run $< -o $@


include $(this_dir)/../rules.make


artifact_exts=cct gates runtime run udg ps pdf svg gviz pdf_t

# uses a variable called base.
# eg:
# make sfdlclean base=cct-101
sfdlclean:
	rm -f $(patsubst %,$(base).%,$(artifact_exts))

clean : base=*
clean: sfdlclean

# make sure 'sfdlclean' does not become the default goal, when a makefile
# includes this file!
.DEFAULT_GOAL := all

.PRECIOUS: %.cct %.gates
