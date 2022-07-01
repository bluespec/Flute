###  -*-Makefile-*-

# Copyright (c) 2022 Bluespec, Inc.  All Rights Reserved

# ================================================================

.PHONY: help
help:
	@echo "Usage:"
	@echo "  make compile      Compile Flute to to RTL"
	@echo "  make clean        Remove build_dir (for intermediate files)"
	@echo "  make full_clean   Restore to pristine state"

# ================================================================
# Dependencies on other repos

BSV_ADDITIONAL_LIBS=$(HOME)/Git/BSV_Additional_Libs
AMBA_FABRICS=$(HOME)/Git/AMBA_Fabrics

ifndef FLUTE_REPO
  $(error ERROR: please define FLUTE_REPO, i.e., path to Flute repo)
else
  $(info  INFO: FLUTE_REPO is $(FLUTE_REPO))
endif

ifndef BSV_ADDITIONAL_LIBS
  $(error ERROR: please define BSV_ADDITIONAL_LIBS, i.e., path to repo)
else
  $(info  INFO: BSV_ADDITIONAL_LIBS is $(BSV_ADDITIONAL_LIBS))
endif

ifndef AMBA_FABRICS
  $(error ERROR: please define AMBA_FABRICS, i.e., path to repo)
else
  $(info  INFO: AMBA_FABRICS is $(AMBA_FABRICS))
endif

#================================================================
# Architectural configs

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

# Target platform (chooses clock speed etc.)
BSC_COMPILATION_FLAGS += -D AWSF1
# BSC_COMPILATION_FLAGS += -D VCU118

# Debugging and tracing
BSC_COMPILATION_FLAGS += -D INCLUDE_PC_TRACE
BSC_COMPILATION_FLAGS += -D INCLUDE_GDB_CONTROL
# BSC_COMPILATION_FLAGS += -D INCLUDE_TANDEM_VERIF

#================================================================
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

BSC_PATH := $(BSC_PATH):$(LLCACHE_DIR):$(PROCS_LIB_DIR):$(PROCS_OOO_DIR):$(COHERENCE_DIR)

#================================================================
# BSC_PATH for AWSteria/Flute/Virtio system

# From Flute
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/AWSteria_Core
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/CPU
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/ISA
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/RegFiles
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/Cache_Config
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/$(NEAR_MEM_VM_DIR)
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/PLIC
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/Near_Mem_IO
BSC_PATH := $(BSC_PATH):$(FLUTE_REPO)/src_Core/Debug_Module

# BSV_Additional_Libs
BSC_PATH := $(BSC_PATH):$(BSV_ADDITIONAL_LIBS)

# AXI
BSC_PATH := $(BSC_PATH):$(AMBA_FABRICS)/AXI4
BSC_PATH := $(BSC_PATH):$(AMBA_FABRICS)/AXI4_Lite

# bsc standard libs
BSC_PATH := $(BSC_PATH):+

#================================================================
# Top-level file and module

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

RTL_GEN_DIRS = -vdir Verilog_RTL  -bdir build_dir  -info-dir build_dir

.PHONY: compile
compile:
	mkdir -p  build_dir
	mkdir -p  Verilog_RTL
	@echo  "INFO: RTL generation ..."
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: RTL generation finished: Verilog_RTL/ is ready for FPGA build."

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  tmp_all_srcs  build_dir

.PHONY: full_clean
full_clean: clean
	rm -r -f  *~  tmp_all_srcs  build_dir  Verilog_RTL  example_AWSteria_HW  example_AWSteria_HW_reclocked $(FOR_GARNET_DIR)

# ================================================================
