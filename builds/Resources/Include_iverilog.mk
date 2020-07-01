###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# This file is not a standalone Makefile, but 'include'd by other Makefiles

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

RTL_GEN_DIRS = -vdir Verilog_RTL  -bdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

.PHONY: compile
compile:  build_dir  Verilog_RTL
	@echo  "INFO: Verilog RTL generation ..."
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  -D IVERILOG $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Verilog RTL generation finished"

# ================================================================
# Compile and link Verilog RTL sources into an iverilog executable

VSIM ?= iverilog
SIM_EXE_FILE = exe_HW_sim

.PHONY: simulator
simulator:
	@echo INFO: iVerilog linking start ...
	bsc -verilog -vsim $(VSIM) -keep-fires \
		$(RTL_GEN_DIRS) \
		-p $(BSC_PATH) \
		-e $(TOPMODULE) -o $(SIM_EXE_FILE) \
		$(BSC_C_FLAGS) \
		$(REPO)/src_Testbench/Top/C_Imported_Functions.c
	@echo INFO: iVerilog linking finished

#	iverilog  -o ./$(SIM_EXE_FILE) \
#		-y  Verilog_RTL \
#		-y  $(REPO)/src_bsc_lib_RTL \
#		-DTOP=$(TOPMODULE) \
#		$(REPO)/src_bsc_lib_RTL/main.v

# ================================================================
