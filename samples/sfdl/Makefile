# this reading of MAKEFILE_LIST has to be done before any other files are
# include-d!
this_file := $(lastword $(MAKEFILE_LIST))
this_dir := $(dir $(realpath $(this_file)))

SRCEXT := sfdl
FaerieC := sfdlc

include $(this_dir)/shared.make

# SFDLCFLAGS += --dgt --dgr
# SFDLCFLAGS += +RTS -xc -RTS
#SFDLCFLAGS += +RTS -p -RTS

# SFDLCFLAGS += --gen-print

# DOTFLAGS += -v

srcs = $(wildcard *.sfdl)

all: $(subst .sfdl,.runtime,$(srcs))
