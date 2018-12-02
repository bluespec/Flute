###  -*-Makefile-*-

# Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

# This file is not a standalone Makefile, but 'include'd by 'Makefile' in the sub-directories

# ================================================================

.PHONY: help
help:
	@echo '    make  gen_RTL        Recompile Core (CPU, caches) into Verilog RTL'
	@echo '                             (NOTE: needs Bluespec bsc compiler)'
	@echo '    make  mkSim          Compiles and links Verilog files into a Verilator executable'
	@echo '    make  all            = make gen_RTL mkSim'
	@echo '    make  test           Run the executable on the standard RISC-V ISA test "$(TEST)"'

.PHONY: all
all: gen_RTL  mkSim

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

# ----------------
# Search path for bsc for .bsv files

REPO ?= ../..

CORE_DIRS = $(REPO)/src_Core/ISA:$(REPO)/src_Core/RegFiles:$(REPO)/src_Core/Core:$(REPO)/src_Core/Near_Mem_VM:$(REPO)/src_Core/BSV_Additional_Libs

TESTBENCH_DIRS  = $(REPO)/src_Testbench/Top:$(REPO)/src_Testbench/SoC:$(REPO)/src_Testbench/Fabrics/AXI4_Lite

BSC_PATH = -p $(CORE_DIRS):$(TESTBENCH_DIRS):+

# ----------------
# Top-level file and module

TOPFILE   = $(REPO)/src_Testbench/Top/Top_HW_Side.bsv
TOPMODULE = mkTop_HW_Side

#----------------
# bsc flags

BSC_FLAGS += -keep-fires -aggressive-conditions -no-warn-action-shadowing \
		-suppress-warnings G0020 \
		-D Near_Mem_Caches \
		-D FABRIC64 \
		+RTS -K64M -RTS  -show-range-conflict

RTL_GEN_DIRS = -vdir Verilog_RTL  -bdir build  -info-dir build

build:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

.PHONY: gen_RTL
gen_RTL:  build  Verilog_RTL
	@echo  "INFO: Verilog RTL generation ..."
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  $(BSC_FLAGS)  $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Verilog RTL generation finished"

# ================================================================
# Compile and link Verilog RTL sources into an verilator executable

SIM_EXE_FILE = exe_HW_sim

# Verilator flags: notes
#    stats              Dump stats on the design, in file {prefix}__stats.txt
#    -O3                Verilator optimization level
#    -CFLAGS -O3        C++ optimization level
#    --x-assign fast    Optimize X value
#    --x-initial fast   Optimize uninitialized value
#    --noassert         Disable all assertions

VERILATOR_FLAGS = --stats -O3 -CFLAGS -O3 -LDFLAGS -static --x-assign fast --x-initial fast --noassert

# Verilator flags: use the following to include code to generate VCDs
# Select trace-depth according to your module hierarchy
# VERILATOR_FLAGS += --trace  --trace-depth 2  -CFLAGS -DVM_TRACE

VTOP                = V$(TOPMODULE)_edited
VERILATOR_RESOURCES = $(REPO)/builds/Resources/Verilator_resources

.PHONY: mkSim
mkSim:
	@echo "INFO: Verilating Verilog files (in newly created obj_dir)"
	sed  -f $(VERILATOR_RESOURCES)/sed_script.txt  Verilog_RTL/$(TOPMODULE).v > tmp1.v
	cat  $(VERILATOR_RESOURCES)/verilator_config.vlt \
	     $(VERILATOR_RESOURCES)/import_DPI_C_decls.v \
	     tmp1.v                                     > Verilog_RTL/$(TOPMODULE)_edited.v
	rm   -f  tmp1.v
	verilator \
		-IVerilog_RTL \
		-I$(REPO)/src_bsc_lib_RTL \
		$(VERILATOR_FLAGS) \
		--cc  $(TOPMODULE)_edited.v \
		--exe  sim_main.cpp \
		$(REPO)/src_Testbench/Top/C_Imported_Functions.c
	@echo "INFO: Linking verilated files"
	cd obj_dir; \
	   ln -s -f ../$(VERILATOR_RESOURCES)/sim_main.cpp; \
	   make -j -f V$(TOPMODULE)_edited.mk  $(VTOP); \
	   cp -p  $(VTOP)  ../$(SIM_EXE_FILE)
	@echo "INFO: Created verilator executable:    $(SIM_EXE_FILE)"

# ================================================================
# Test: run the executable on the standard RISCV ISA test specified in TEST

VERBOSITY ?= +v1

.PHONY: test
test:
	make -C  $(REPO)/Tests/elf_to_hex
	$(REPO)/Tests/elf_to_hex/elf_to_hex  $(REPO)/Tests/isa/$(TEST)  Mem.hex
	./$(SIM_EXE_FILE)  $(VERBOSITY)  +tohost

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  build
	rm -r -f  obj_dir

.PHONY: full_clean
full_clean: clean
	rm -r -f  *~  $(SIM_EXE_FILE)*  *.log  *.vcd  *.hex
	rm -r -f  obj_dir

# ================================================================
