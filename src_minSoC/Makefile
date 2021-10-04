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
DEBUG_REPO ?= ../Debug_Module
ARCH ?= RV32ACIMU

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler
#
BSC_COMPILATION_ISA_FLAGS = \
	-D RV32 \
	-D ISA_PRIV_M  -D ISA_PRIV_U  \
	-D ISA_I  -D ISA_M  -D ISA_A -D ISA_C \

BSC_COMPILATION_UARCH_FLAGS = \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH    \
	-D FABRIC_AHBL \
	-D FABRIC_AXI4 \
	-D DUAL_FABRIC \
	-D WATCH_TOHOST \
	-D INCLUDE_TANDEM_VERIF \
	-D INCLUDE_GDB_CONTROL \
	-D SHOW_COMPRESSED \
#	-D MICROSEMI -D TCM_LOADER \
# Comment/Uncomment the above when to include/exclude TCM_LOADER
# The MICROSEMI flag uses BRAM without file-load capability

BSC_COMPILATION_NM_FLAGS = \
	-D FABRIC32    \
	-D Near_Mem_TCM    \
#	-D NM32        \

BSC_COMPILATION_DM_FLAGS = \
	-D JTAG_TAP \
	-D XILINX_BSCAN -D XILINX_XC7K325T \
	-D INCLUDE_GDB_CONTROL \

BSC_COMPILATION_FLAGS = \
			$(BSC_COMPILATION_ISA_FLAGS) \
			$(BSC_COMPILATION_UARCH_FLAGS) \
			$(BSC_COMPILATION_NM_FLAGS) \
# BSC_COMPILATION_FLAGS = \
                          BSC_COMPILATION_ISA_FLAGS \
			  BSC_COMPILATION_UARCH_FLAGS \
			  BSC_COMPILATION_NM_FLAGS \
			  BSC_COMPILATION_DM_FLAGS \


# ================================================================
# Search path for bsc for .bsv files

CORE_DIRS = $(REPO)/src_Core/CPU:$(REPO)/src_Core/ISA:$(REPO)/src_Core/RegFiles:$(REPO)/src_Core/Core_v3:$(REPO)/src_Core/Near_Mem/Near_Mem_TCM/src:$(REPO)/src_Core/Near_Mem/Near_Mem_TCM/fabrics/AHB_Lite/src:$(REPO)/src_Core/PLIC:$(REPO)/src_Core/Near_Mem_IO:$(REPO)/src_Core/BSV_Additional_Libs:$(DEBUG_REPO)/src

TESTBENCH_DIRS  = $(REPO)/src_Testbench/Fabrics/AXI4

BSC_PATH = -p $(CORE_DIRS):src_BSV:$(TESTBENCH_DIRS):+:%/Libraries/TLM3:%/Libraries/Axi:%/Libraries/Axi4

# ----------------
# Top-level file and module

TOPFILE   = src_BSV/BSCore.bsv
TOPMODULE = mkBSCore

# ================================================================
# bsc compilation flags

BSC_COMPILATION_FLAGS += \
	-keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps \
	-suppress-warnings G0020    \
	+RTS -K128M -RTS  -show-range-conflict

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
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Generated RTL into Verilog_RTL"
	mkdir -p xilinx_ip/hdl
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
