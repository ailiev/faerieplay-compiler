# this reading of MAKEFILE_LIST has to be done before any other files are
# include-d!
this_file := $(lastword $(MAKEFILE_LIST))
this_dir := $(dir $(realpath $(this_file)))

SRCEXT := fcpp
FaerieC := fc++

include ../samples/sfdl/shared.make

# SFDLCFLAGS += --dgt --dgr
# SFDLCFLAGS += +RTS -xc -RTS
#SFDLCFLAGS += +RTS -p -RTS

# SFDLCFLAGS += --gen-print

# DOTFLAGS += -v

srcs = $(wildcard *.fcpp)

all: $(subst .fcpp,.runtime,$(srcs))
