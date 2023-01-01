# This file is included by other Makefiles, and defines paths to
# various external resources.

# Please change these as necessary for your environment.

# For building
AMBA_FABRICS        ?= $(HOME)/Git/AMBA_Fabrics
BSV_ADDITIONAL_LIBS ?= $(HOME)/Git/BSV_Additional_Libs
DEBUG_MODULE        ?= $(HOME)/Git/RISCV_Debug_Module

BSC_LIBS            ?= $(HOME)/NoBak/bsc-2022.01-ubuntu-20.04/lib/Verilog

.PHONY: show_Makefile_Params
show_Makefile_Params:
	@echo ""
	@echo "Makefile params:"
	@echo "  AMBA_FABRICS        = $(AMBA_FABRICS)"
	@echo "  BSV_ADDITIONAL_LIBS = $(BSV_ADDITIONAL_LIBS)"
	@echo "  DEBUG_MODULE        = $(DEBUG_MODULE)"
	@echo "  BSC_LIBS            = $(BSC_LIBS)"
