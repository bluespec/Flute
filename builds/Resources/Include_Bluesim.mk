###  -*-Makefile-*-

# Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

# This file is not a standalone Makefile, but 'include'd by 'Makefile' in the sub-directories

# ================================================================

.PHONY: help
help:
	@echo '    make  compile        Recompile Core (CPU, caches) into Bluesim intermediate files'
	@echo '                             (NOTE: needs Bluespec bsc compiler)'
	@echo '    make  mkSim          Compiles and links Bluesim intermediate files into a Bluesim executable'
	@echo '    make  all            = make compile mkSim'
	@echo '    make  test           Run the executable on the standard RISC-V ISA test "$(TEST)"'

.PHONY: all
all: compile  mkSim

# ================================================================
# Compile Bluesim intermediate files from BSV sources (needs Bluespec 'bsc' compiler)

# ----------------
# Search path for bsc for .bsv files

REPO = ../..

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

TMP_DIRS  = -bdir build  -simdir build  -info-dir build

build:
	mkdir -p $@

.PHONY: compile
compile: build
	@echo "INFO: Re-compiling Core (CPU, Caches)"
	bsc -u -elab -sim  $(TMP_DIRS)  $(BSC_FLAGS)  $(BSC_PATH)  $(TOPFILE)
	@echo "INFO: Re-compiled  Core (CPU, Caches)"

# ================================================================
# Compile and link Bluesim intermediate files into a Bluesim executable

SIM_EXE_FILE = exe_HW_sim

BSC_C_FLAGS += \
	-Xc++  -D_GLIBCXX_USE_CXX11_ABI=0 \
	-Xl -v \
	-Xc -O3 -Xc++ -O3 \

.PHONY: mkSim
mkSim:
	@echo "INFO: linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 \
		$(TMP_DIRS) \
		-e $(TOPMODULE) -o ./$(SIM_EXE_FILE) \
		$(BSC_C_FLAGS) \
		$(REPO)/src_Testbench/Top/C_ConsoleIO_functions.c
	@echo "INFO: linked bsc-compiled objects into Bluesim executable"

# ================================================================
# Test: run the executable on the standard RISCV ISA test specified in TEST

.PHONY: test
test:
	make -C  $(REPO)/Tests/elf_to_hex
	$(REPO)/Tests/elf_to_hex/elf_to_hex  $(REPO)/Tests/isa/$(TEST)  Mem.hex
	./$(SIM_EXE_FILE)  +v1  +tohost

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  build

.PHONY: full_clean
full_clean: clean
	rm -r -f  *~  $(SIM_EXE_FILE)*  *.log  *.vcd  *.hex

# ================================================================
