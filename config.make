# -*- makefile -*-

SHARED_DIR=../pir

# this defined DIST_ROOT, we will use the same installation directory.
include $(SHARED_DIR)/config.make

# the ghc compiler
GHC = ghc

# the source language for the Faerieplay compiler.
# these need to start with an upper case, as they are used as haskell package
# names at some stage.
#source_lang = Sfdl
source_lang = Fcpp

# Locations under $(TOOLS_DIR) of the haskell parsing tools, alex and happy.
ALEX_SUBDIR=usr/share/alex-2.1.0
HAPPY_SUBDIR=usr/share/happy-1.16
