###  -*-Makefile-*-

# Copyright (c) 2022 Bluespec, Inc.  All Rights Reserved

# ================================================================
.PHONY: help
help:
	@echo "Usage:"
	@echo "  help                    Show this help info"
	@echo ""
	@echo "  all/generate_verilog    Generate RTL (using bsc) from Flute BSV sources"
	@echo ""
	@echo "  make clean          Remove intermediate files"
	@echo "  make full_clean     Restore to pristine state"
	@echo ""
	@echo "  Current PLATFORM_TARGET = $(PLATFORM_TARGET)"
	@echo "      (affects choice of clock in AWSteria_Core)"
	@echo ""
	@echo "Resources:"
	@echo "  FLUTE_REPO          = $(FLUTE_REPO)"
	@echo "  AMBA_FABRICS        = $(AMBA_FABRICS)"
	@echo "  BSV_ADDITIONAL_LIBS = $(BSV_ADDITIONAL_LIBS)"
	@echo "  DEBUG_MODULE        = $(DEBUG_MODULE)"
	@echo "  BSC_VERILOG_LIB     = $(BSC_VERILOG_LIB)"
	@echo ""
	@echo "  BSC_PATH = $(BSC_PATH)"
	@echo ""
	@echo "  BSC_COMPILATION_FLAGS = $(BSC_COMPILATION_FLAGS)"

# Convenience
.PHONY: all
all: generate_verilog

# Target platform (chooses clock speed etc.)
PLATFORM_TARGET = PLATFORM_AWSF1
# PLATFORM_TARGET = PLATFORM_VCU118

# ================================================================
# Defs of resources etc.
# Please change these as necessary for your environment.

FLUTE_REPO          ?= ../..

AMBA_FABRICS        ?= $(HOME)/Git/AMBA_Fabrics
BSV_ADDITIONAL_LIBS ?= $(HOME)/Git/BSV_Additional_Libs/src
DEBUG_MODULE        ?= $(HOME)/Git/RISCV_Debug_Module
BSC_VERILOG_LIB     ?= $(HOME)/NoBak/bsc/inst/lib/Verilog

# ================================================================
# Core/CPU architectural configs

RV = RV64
ARCH ?= RV64ACDFIMSU

# Alternative cache organizations:
# Default (no definition for CACHES)    L1 only, write-through policy
# Define  CACHES=WB_L1                  L1 only, with write-back policy
# Define  CACHES=WB_L1_L2               L1+L2, with write-back policy

CACHES=WB_L1_L2
NEAR_MEM_VM_DIR=Near_Mem_VM_WB_L1_L2

# Unprivileged ISA
BSC_COMPILATION_FLAGS += -D $(RV)
BSC_COMPILATION_FLAGS += -D ISA_I  -D ISA_M  -D ISA_A  -D ISA_C
BSC_COMPILATION_FLAGS += -D ISA_F  -D ISA_D  -D INCLUDE_FDIV  -D INCLUDE_FSQRT

# Unprivileged ISA implementation choices
BSC_COMPILATION_FLAGS += -D SHIFT_BARREL
BSC_COMPILATION_FLAGS += -D MULT_SYNTH

# Privileged ISA
BSC_COMPILATION_FLAGS += -D ISA_PRIV_M  -D ISA_PRIV_U  -D ISA_PRIV_S

# Virtual Memory scheme
# BSC_COMPILATION_FLAGS += -D SV32
BSC_COMPILATION_FLAGS += -D SV39

# System implementation choices
BSC_COMPILATION_FLAGS += -D Near_Mem_Caches
BSC_COMPILATION_FLAGS += -D FABRIC64
BSC_COMPILATION_FLAGS += -D WATCH_TOHOST
BSC_COMPILATION_FLAGS += -D MEM_512b

# With this flag, interpose clock-crossings on interface AWSteria_Core_IFC
BSC_COMPILATION_FLAGS += -D INCLUDE_AWSTERIA_CORE_IFC_CLOCK_CROSSING

BSC_COMPILATION_FLAGS += -D $(PLATFORM_TARGET)

# Debugging and tracing
# BSC_COMPILATION_FLAGS += -D INCLUDE_PC_TRACE
BSC_COMPILATION_FLAGS += -D INCLUDE_GDB_CONTROL
# BSC_COMPILATION_FLAGS += -D INCLUDE_TANDEM_VERIF

# ================================================================
# For LLCache    (only needed for WB_L1_L2)

# core size
CORE_SIZE ?= SMALL
# default 1 core
CORE_NUM ?= 1
# cache size
CACHE_SIZE ?= LARGE

BSC_COMPILATION_FLAGS += \
	-D CORE_$(CORE_SIZE) \
	-D NUM_CORES=$(CORE_NUM) \
	-D CACHE_$(CACHE_SIZE) \

LLCACHE_DIR   = $(FLUTE_REPO)/src_Core/Near_Mem_VM_WB_L1_L2/src_LLCache
PROCS_LIB_DIR = $(LLCACHE_DIR)/procs/lib
PROCS_OOO_DIR = $(LLCACHE_DIR)/procs/RV64G_OOO
COHERENCE_DIR = $(LLCACHE_DIR)/coherence/src

BSC_PATH := $(LLCACHE_DIR):$(PROCS_LIB_DIR):$(PROCS_OOO_DIR):$(COHERENCE_DIR)

# ================================================================
# BSV-file search path

# From Flute
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/AWSteria_Core
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/CPU
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/ISA
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/RegFiles
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/Cache_Config
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/$(NEAR_MEM_VM_DIR)
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/PLIC
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/Near_Mem_IO

# Additional libs
BSC_PATH := $(BSC_PATH):$(BSV_ADDITIONAL_LIBS)

# Fabrics
BSC_PATH := $(BSC_PATH):$(AMBA_FABRICS)/AXI4
BSC_PATH := $(BSC_PATH):$(AMBA_FABRICS)/AXI4_Lite

# From Debug Module
BSC_PATH := $(BSC_PATH):$(DEBUG_MODULE)/src

# bsc standard libs
BSC_PATH := $(BSC_PATH):+

# ================================================================
# Top-level BSV file and module

TOPFILE   = $(FLUTE_REPO)/src_Core/AWSteria_Core/AWSteria_Core.bsv
TOPMODULE = mkAWSteria_Core

# ================================================================
# bsc generic compilation flags

BSC_COMPILATION_FLAGS += -keep-fires -aggressive-conditions -no-warn-action-shadowing
BSC_COMPILATION_FLAGS += -no-show-timestamps -check-assert
BSC_COMPILATION_FLAGS += -suppress-warnings G0020
BSC_COMPILATION_FLAGS += +RTS -K128M -RTS  -show-range-conflict

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

VERILOG_RTL = RTL_Catamaran_Core_$(PLATFORM_TARGET)
RTL_GEN_DIRS = -vdir $(VERILOG_RTL)  -bdir build_dir  -info-dir build_dir

.PHONY: generate_verilog
generate_verilog:
	mkdir -p  build_dir
	mkdir -p  $(VERILOG_RTL)
	@echo  "INFO: RTL generation into $(VERILOG_RTL)/ ..."
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: RTL generation into $(VERILOG_RTL)/ finished."
	@echo  "INFO: Copying bsc library Verilog files from $(BSC_VERILOG_LIB)/"
	cp -p  $(BSC_VERILOG_LIB)/RevertReg.v  $(VERILOG_RTL)/
	cp -p  $(BSC_VERILOG_LIB)/BRAM2.v      $(VERILOG_RTL)/
	cp -p  $(BSC_VERILOG_LIB)/FIFOL1.v     $(VERILOG_RTL)/

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  build_dir

.PHONY: full_clean
full_clean: clean
	rm -r -f  Verilog_RTL_*

# ================================================================
