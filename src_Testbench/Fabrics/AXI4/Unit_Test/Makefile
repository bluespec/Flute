###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# Makefile for standalone Unit Tester for Deburster (Bluesim only)

.PHONY: all
all: compile  simulator

# ================================================================
# Search path for bsc for .bsv files

BSV_ADDL_LIBS=../../../../src_Core/BSV_Additional_Libs

BSC_PATH = -p ..:$(BSV_ADDL_LIBS):+

# ----------------
# Top-level file and module

TOPFILE   = Unit_Test_Deburster.bsv
TOPMODULE = mkUnit_Test_Deburster

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

.PHONY: compile
compile: build_dir
	@echo "INFO: Re-compiling BSV sources"
	bsc -u -elab -sim  $(TMP_DIRS)  $(BSC_COMPILATION_FLAGS)  $(BSC_PATH)  $(TOPFILE)
	@echo "INFO: Re-compiled  BSV sources"

# ================================================================
# Compile and link Bluesim intermediate files into a Bluesim executable

SIM_EXE_FILE = exe_HW_sim

BSC_C_FLAGS += \
	-Xc++  -D_GLIBCXX_USE_CXX11_ABI=0 \
	-Xl -v

.PHONY: simulator
simulator:
	@echo "INFO: linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 \
		$(TMP_DIRS) \
		-e $(TOPMODULE) -o ./$(SIM_EXE_FILE) \
		$(BSC_C_FLAGS)
	@echo "INFO: linked bsc-compiled objects into Bluesim executable"

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  build_dir

.PHONY: full_clean
full_clean: clean
	rm -r -f  $(SIM_EXE_FILE)*  *.log  *.vcd

# ================================================================
