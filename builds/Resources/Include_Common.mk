###  -*-Makefile-*-

# Copyright (c) 2018-2021 Bluespec, Inc. All Rights Reserved

# This file is not a standalone Makefile, but 'include'd by other Makefiles

# It contains common defs used by Makefiles generated for specific
# RISC-V implementations that differ in RISC-V architectural-feature
# choices, hardware implementation choices and simulator choices.

# ================================================================

.PHONY: help
help:
	@echo '    make  compile      Recompile Core (CPU, caches)'
	@echo '                           NOTE: needs Bluespec bsc compiler'
	@echo '                           For Bluesim: generates Bluesim intermediate files'
	@echo '                           For Verilog simulation: generates RTL'
	@echo '    make  simulator    Compiles and links intermediate files/RTL to create simulation executable'
	@echo '                           (Bluesim, verilator or iverilog)'
	@echo '    make  all          = make  compile  simulator'
	@echo ''
	@echo '    make  run_example  Runs simulation executable on ELF given by EXAMPLE'
	@echo ''
	@echo '    make  test         Runs simulation executable on rv32ui-p-add or rv64ui-p-add'
	@echo '    make  isa_tests    Runs simulation executable on all relevant standard RISC-V ISA tests'
	@echo ''
	@echo '    make  clean        Remove intermediate build-files unnecessary for execution'
	@echo '    make  full_clean   Restore to pristine state (pre-building anything)'

.PHONY: all
all: compile  simulator

# ================================================================
# Near-mem (Cache and optional MMU for VM)
# WT = Write-through; WB = write-back
# L1 = L1 only; L1_L2 = L1 + coherent L2
# TCM = No caches, only TCM

CACHES ?= WT_L1

ifeq ($(CACHES),WB_L1)
  NEAR_MEM_VM_DIR=Near_Mem_VM_WB_L1
else ifeq ($(CACHES),WB_L1_L2)
  NEAR_MEM_VM_DIR=Near_Mem_VM_WB_L1_L2
else ifeq ($(CACHES),TCM)
  NEAR_MEM_VM_DIR=Near_Mem/Near_Mem_TCM/src
else
  NEAR_MEM_VM_DIR=Near_Mem_VM_WT_L1
endif

FABRICS ?= AXI4
ifeq ($(FABRICS),AHBL)
   CUSTOM_DIRS = $(REPO)/src_Core/Near_Mem/Near_Mem_TCM/fabrics/AHB_Lite/src
endif

# ================================================================
# CORE

# SRC_CORE ?= $(REPO)/src_Core/Core
# SRC_CORE ?= $(REPO)/src_Core/Core_v2
SRC_CORE ?= $(REPO)/src_Core/Core_v3

# ================================================================
# Debug Module
SRC_DM ?= $(REPO)/Debug_Module/src
# SRC_DM ?= $(REPO)/src_Core/Debug_Module

# ================================================================
# Search path for bsc for .bsv files

CORE_DIRS = $(REPO)/src_Core/CPU:$(REPO)/src_Core/ISA:$(REPO)/src_Core/RegFiles:$(SRC_CORE):$(REPO)/src_Core/Cache_Config:$(REPO)/src_Core/$(NEAR_MEM_VM_DIR):$(REPO)/src_Core/PLIC:$(REPO)/src_Core/Near_Mem_IO:$(SRC_DM):$(REPO)/src_Core/BSV_Additional_Libs

TESTBENCH_DIRS = $(REPO)/src_Testbench/Top:$(REPO)/src_Testbench/SoC:$(REPO)/src_Testbench/Fabrics/AXI4

BSC_PATH = $(CUSTOM_DIRS):$(CORE_DIRS):$(CUSTOM_TB_DIRS):$(TESTBENCH_DIRS):+

# ----------------
# Top-level file and module

TOPFILE   ?= $(REPO)/src_Testbench/Top/Top_HW_Side.bsv
TOPMODULE ?= mkTop_HW_Side

# ================================================================
# bsc compilation flags

BSC_COMPILATION_FLAGS += \
	-keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
	-suppress-warnings G0020    \
	+RTS -K128M -RTS  -show-range-conflict

# ================================================================
# Runs simulation executable on ELF given by EXAMPLE

EXAMPLE ?= PLEASE_DEFINE_EXAMPLE_PATH_TO_ELF

.PHONY: run_example
run_example:
	make -C  $(TESTS_DIR)/elf_to_hex
	$(TESTS_DIR)/elf_to_hex/elf_to_hex  $(EXAMPLE)  Mem.hex
	./exe_HW_sim  $(VERBOSITY)  +exit

# ================================================================
# Test: run the executable on the standard RISCV ISA test specified in TEST

TESTS_DIR ?= $(REPO)/Tests

VERBOSITY ?= +v1

.PHONY: test
test:
	make -C  $(TESTS_DIR)/elf_to_hex
	$(TESTS_DIR)/elf_to_hex/elf_to_hex  $(TESTS_DIR)/isa/$(TEST)  Mem.hex
	./exe_HW_sim  $(VERBOSITY)  +tohost

# ================================================================
# ISA Regression testing

.PHONY: isa_tests
isa_tests:
	@echo "Running regressions on ISA tests; saving logs in Logs/"
	$(REPO)/Tests/Run_regression.py  ./exe_HW_sim  $(REPO)  ./Logs  $(ARCH)  $(PRIVS)
	@echo "Finished running regressions; saved logs in Logs/"

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  Makefile_*  symbol_table.txt  build_dir  obj_dir  directc*

.PHONY: full_clean
full_clean: clean
	rm -r -f  $(SIM_EXE_FILE)*  *.log  *.vcd  *.hex  Logs/  worker_*

# ================================================================
