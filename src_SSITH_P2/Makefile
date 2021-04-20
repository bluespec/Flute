###  -*-Makefile-*-

# ================================================================

.PHONY: help
help:
	@echo '    make  compile      Recompile Core (CPU, caches) into Verilog_RTL and copies into xilinx_ip/hdl'
	@echo '                           NOTE: needs Bluespec bsc compiler'
	@echo ''
	@echo '    make  clean        Remove intermediate build-files'
	@echo '    make  full_clean   Restore this directory to pristine state'

.PHONY: all
all: compile

# ================================================================

REPO ?= ..
ARCH ?= RV64ACDFIMSU

# ================================================================
# Cache implementation options
#     WT_L1:    Write-Through, L1 only
#     WB_L1:    Write-Back, L1 only
#     WB_L1_L2: Write-Back, L1 and L2, cache coherent
#     TCM:      Tightly-coupled memory (no caches)

# NEAR_MEM_VM_DIR=Near_Mem_VM_WT_L1
# NEAR_MEM_VM_DIR=Near_Mem_VM_WB_L1
NEAR_MEM_VM_DIR=Near_Mem_VM_WB_L1_L2
# NEAR_MEM_VM_DIR=Near_Mem_TCM

# ================================================================
# CORE implementation options
#   Use 'Core_v2' with WB_L1_L2 cache, otherwise use 'Core'

# SRC_CORE ?= $(REPO)/src_Core/Core
SRC_CORE ?= $(REPO)/src_Core/Core_v2

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_PRIV_M  -D ISA_PRIV_U  -D ISA_PRIV_S  \
	-D SV39 \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_C  \
	-D ISA_F -D ISA_D -D INCLUDE_FDIV  -D INCLUDE_FSQRT \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH     \
	-D Near_Mem_Caches    \
	-D FABRIC64    \
	-D INCLUDE_GDB_CONTROL \
	-D INCLUDE_TANDEM_VERIF \
	-D BRVF_TRACE \
	-D XILINX_BSCAN  -D XILINX_XCVU9P  -D JTAG_TAP

#================================================================
# For LLCache

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

LLCACHE_DIR   = $(REPO)/src_Core/Near_Mem_VM_WB_L1_L2/src_LLCache
PROCS_LIB_DIR = $(LLCACHE_DIR)/procs/lib
PROCS_OOO_DIR = $(LLCACHE_DIR)/procs/RV64G_OOO
COHERENCE_DIR = $(LLCACHE_DIR)/coherence/src

CUSTOM_DIRS = $(LLCACHE_DIR):$(PROCS_LIB_DIR):$(PROCS_OOO_DIR):$(COHERENCE_DIR)

# ================================================================
# bsc compilation flags

BSC_COMPILATION_FLAGS += \
	-keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
	-suppress-warnings G0020    \
	+RTS -K128M -RTS  -show-range-conflict

# ================================================================
# Search path for bsc for .bsv files

CORE_DIRS = $(REPO)/src_Core/CPU:$(REPO)/src_Core/ISA:$(REPO)/src_Core/RegFiles:$(SRC_CORE):$(REPO)/src_Core/Cache_Config:$(REPO)/src_Core/$(NEAR_MEM_VM_DIR):$(REPO)/src_Core/PLIC:$(REPO)/src_Core/Near_Mem_IO:$(REPO)/src_Core/Debug_Module:$(REPO)/src_Core/BSV_Additional_Libs

TESTBENCH_DIRS  = $(REPO)/src_Testbench/Fabrics/AXI4

# ----------------
# Please use one of the following defs of BSC_PATH (and comment out the other)

# If you are using one of the older bsc 'releases'
BSC_PATH = $(CUSTOM_DIRS):src_BSV:$(CORE_DIRS):$(TESTBENCH_DIRS):+:%/Libraries/TLM3:%/Libraries/Axi:%/Libraries/Axi4

# If you are using a new 'bsc' build from these open-source repos:
#     https://github.com/B-Lang-org/bsc
#     https://github.com/B-Lang-org/bsc-contrib
# CONTRIB_LIBS=$(HOME)/git_clones/bsc-contrib/Libraries
# TLM_LIBS=$(CONTRIB_LIBS)/Bus:$(CONTRIB_LIBS)/AMBA_TLM3/TLM3:$(CONTRIB_LIBS)/AMBA_TLM3/Axi:$(CONTRIB_LIBS)/AMBA_TLM3/Axi4
# BSC_PATH = $(CUSTOM_DIRS):src_BSV:$(CORE_DIRS):$(TESTBENCH_DIRS):+:$(TLM_LIBS)

# ----------------
# Top-level file and module

TOPFILE   = src_BSV/P2_Core.bsv
TOPMODULE = mkP2_Core

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

RTL_GEN_DIRS = -vdir Verilog_RTL  -bdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

.PHONY: compile
compile:  build_dir  Verilog_RTL
	@echo  "INFO: Generating RTL into Verilog_RTL ..."
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Generated RTL into Verilog_RTL"
	cp  Verilog_RTL/*  xilinx_ip/hdl/
	@echo  "INFO: Copied RTL from  Verilog_RTL/  to  xilinx_ip/hdl/"

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  Makefile_*  build_dir

.PHONY: full_clean
full_clean: clean
	rm -r -f  *.log  Verilog_RTL

# ================================================================
