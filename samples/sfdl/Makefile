include shared.make

# SFDLCFLAGS += --dgt --dgr
# SFDLCFLAGS += +RTS -xc -RTS
#SFDLCFLAGS += +RTS -p -RTS

# SFDLCFLAGS += --gen-print

# DOTFLAGS += -v

srcs = $(wildcard *.sfdl)

all: $(subst .sfdl,.runtime,$(srcs))
