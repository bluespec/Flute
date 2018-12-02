###  -*-Makefile-*-

# Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

# This file is not a standalone Makefile, but 'include'd by 'Makefile' in the sub-directories

# ================================================================
# bsc flags to build for RV32 A, C, I, M, S, U

# Implementation choice: SHIFT_BARREL, SHIFT_SERIAL, SHIFT_MULT, SHIFT_NONE
# Implementation choice: MULT_SYNTH, MULT_SERIAL

BSC_FLAGS ?= -D RV32 \
	     -D SHIFT_BARREL \
	     -D MULT_SYNTH \
	     -D CSR_REGFILE_MSU \
	     -D ISA_PRIV_M \
	     -D ISA_PRIV_U \
	     -D ISA_PRIV_S  -D SV32 \
	     -D ISA_C \
	     -D ISA_M \
	     -D ISA_A

# Default ISA test

TEST ?= rv32ui-p-add

# ================================================================
