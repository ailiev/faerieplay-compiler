# -*- makefile -*-

# set var SRCEXT before including this file
# values: sfdl or fcpp

# this reading of MAKEFILE_LIST has to be done before any other files are
# include-d!
this_file := $(lastword $(MAKEFILE_LIST))
this_dir := $(dir $(realpath $(this_file)))

ifndef SRCEXT
    $(error Need to set variable SRCEXT before including $(this_file))
endif

ifndef FaerieC
    $(error Need to set variable FaerieC to the Faerieplay compiler name before including $(this_file))
endif

%.cct: %.$(SRCEXT)
	$(FaerieC) $(SFDLCFLAGS) --dump-graph -c $<

%.gates: %.$(SRCEXT)
	$(FaerieC) $(SFDLCFLAGS) --dump-gates -c $<

%.runtime: %.$(SRCEXT)
	$(FaerieC) $(SFDLCFLAGS) -c $< -o $@

%.udg: %.cct
	$(FaerieC) $(SFDLCFLAGS) --mkgraph $< -o $@

%.run: %.gates
	$(FaerieC) $(SFDLCFLAGS) --run $< -o $@

%.ps: %.gviz
	dot $(DOTFLAGS) -Tps $< -o $@


%.svg: %.gviz
	dot $(DOTFLAGS) -Tsvg $< -o $@

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
