###  -*-Makefile-*-

# *** DO NOT EDIT! ***
# *** This file is program-generated, not hand-written. ***
# ================================================================

REPO   ?= ../..
ARCH   ?= RV64ACIMU
PRIVS  ?= MU
CACHES ?= WT_L1

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_C  \
	-D ISA_PRIV_M  -D ISA_PRIV_U  \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH    \
	-D Near_Mem_Caches    \
	-D FABRIC64    \
	-D WATCH_TOHOST    \


# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator

include $(REPO)/builds/Resources/Include_bluesim.mk

