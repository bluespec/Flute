###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# Makefile for standalone Unit Tester for Fabric (Bluesim only)

.PHONY: all
all: bcompile  bsimulator

REPO = ../../../..

# ================================================================
# Search path for bsc for .bsv files

BSV_ADDL_LIBS=../../../../src_Core/BSV_Additional_Libs

BSC_PATH = -p ..:$(BSV_ADDL_LIBS):+

# ----------------
# Top-level file and module

TOPFILE   = Unit_Test_Fabric.bsv
TOPMODULE = mkUnit_Test_Fabric

# ================================================================
# bsc compilation flags

BSC_COMPILATION_FLAGS += \
	-keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
	-suppress-warnings G0020    \
	+RTS -K128M -RTS  -show-range-conflict

# ================================================================
# Compile Bluesim intermediate files from BSV sources (needs Bluespec 'bsc' compiler)

TMP_DIRS  = -bdir build_dir  -simdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

.PHONY: bcompile
bcompile: build_dir
	@echo "INFO: Re-compiling BSV sources"
	bsc -u -elab -sim  $(TMP_DIRS)  $(BSC_COMPILATION_FLAGS)  $(BSC_PATH)  $(TOPFILE)
	@echo "INFO: Re-compiled  BSV sources"

# ================================================================
# Compile and link Bluesim intermediate files into a Bluesim executable

BSIM_EXE_FILE = exe_HW_bsim

BSC_C_FLAGS += \
	-Xc++  -D_GLIBCXX_USE_CXX11_ABI=0 \
	-Xl -v

.PHONY: bsimulator
bsimulator:
	@echo "INFO: linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 \
		$(TMP_DIRS) \
		-e $(TOPMODULE) -o ./$(BSIM_EXE_FILE) \
		$(BSC_C_FLAGS)
	@echo "INFO: linked bsc-compiled objects into Bluesim executable"

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

RTL_GEN_DIRS = -vdir Verilog_RTL  -bdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

.PHONY: vcompile
vcompile:  build_dir  Verilog_RTL
	@echo  "INFO: Verilog RTL generation ..."
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Verilog RTL generation finished"

# ================================================================
# Compile and link Verilog RTL sources into an iverilog executable

VSIM_EXE_FILE = exe_HW_vsim

.PHONY: vsimulator
vsimulator:
	@echo INFO: iVerilog linking start ...
	iverilog  -o ./$(VSIM_EXE_FILE) \
		-y  Verilog_RTL \
		-y  Verilog_lib \
		-DTOP=$(TOPMODULE) \
		Verilog_lib/main.v
	@echo INFO: iVerilog linking finished

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  build_dir

.PHONY: full_clean
full_clean: clean
	rm -r -f  $(BSIM_EXE_FILE)  $(BSIM_EXE_FILE).so  $(VSIM_EXE_FILE)  *.log  *.vcd  Verilog_RTL

# ================================================================
