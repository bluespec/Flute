# Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved

# Makefile to create Test_PLIC and PLIC into a standalone unit-test executable.

# Edit the StmtFSM in Test_PLIC to create different tests.

# ----------------

REPO = $(HOME)/GitHub/Flute

# ----------------
# Top-level file and module

TOPFILE   ?= $(REPO)/src_Core/PLIC/Test_PLIC.bsv
TOPMODULE ?= mkTest_PLIC

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
        -D RV64 \
        -D ISA_PRIV_M  -D ISA_PRIV_U  -D ISA_PRIV_S  \
        -D SV39  \
        -D ISA_I  -D ISA_M  -D ISA_A  \
        -D SHIFT_BARREL    \
        -D MULT_SYNTH    \
        -D Near_Mem_Caches    \
        -D FABRIC64    \


# ================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

# ================================================================
# Makefile rules for building for specific simulator: bluesim

include $(REPO)/builds/Resources/Include_bluesim.mk

# ================================================================
