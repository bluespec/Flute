###  -*-Makefile-*-

# Copyright (c) 2021 Rishiyur S. Nikhil and Bluespec, Inc. All Rights Reserved

# Makefile for standalone Unit Tester for AXI4_to_LDST (Bluesim only)

.PHONY: all
all: compile  simulator

# ================================================================
# Search path for bsc for .bsv files

BSV_ADDL_LIBS=$(FLUTE_REPO)/src_Core/BSV_Additional_Libs

BSC_PATH := ..
BSC_PATH := $(BSC_PATH):$(BSV_ADDL_LIBS)
BSC_PATH := $(BSC_PATH):+

# ----------------
# Top-level file and module

TOPFILE   = Test_AXI4_to_LDST.bsv
TOPMODULE = mkTest_AXI4_to_LDST

# ================================================================
# bsc compilation flags

BSC_COMPILATION_FLAGS =  -keep-fires -aggressive-conditions -check-assert
BSC_COMPILATION_FLAGS += -no-warn-action-shadowing -no-show-timestamps
BSC_COMPILATION_FLAGS += -suppress-warnings G0020  -show-range-conflict
BSC_COMPILATION_FLAGS += +RTS -K128M -RTS

# ================================================================
# Compile Bluesim intermediate files from BSV sources (needs Bluespec 'bsc' compiler)

TMP_DIRS  = -bdir build_dir  -simdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

.PHONY: compile
compile: build_dir
	@echo "INFO: Re-compiling BSV sources"
	bsc -u -elab -sim  $(TMP_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
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

.PHONY: test
test: $(SIM_EXE_FILE)
	./$(SIM_EXE_FILE)

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  build_dir

.PHONY: full_clean
full_clean: clean
	rm -r -f  $(SIM_EXE_FILE)*  *.log  *.vcd

# ================================================================
