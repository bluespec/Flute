###  -*-Makefile-*-

# Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

# ================================================================
# Regression: run all ISA tests relevant for the chosen simulator

# ----------------
# Choose a prefix for the simulation executable dir

ARCH ?= RV32IMU
# ARCH ?= RV32ACIMSU

# ARCH ?= RV64IMU
# ARCH ?= RV64ACIMSU

# ----------------
# Choose a simulation engine

SIM ?= Bluesim
# SIM ?= iverilog
# SIM ?= verilator

# ----------------
# Optionally choose an architecture explicitly,
# overriding the simulation executable path

OPTARCH ?=

# ----------------
SIM_DIR = ../builds/$(ARCH)_$(SIM)

# ================================================================

.PHONY: test
test:
	@echo "Running regressions; saving logs in Logs/"
	./Run_regression.py \
		$(SIM_DIR)/exe_HW_sim \
		./isa  ./Logs  $(OPTARCH)
	@echo "Finished running regressions; saved logs in Logs/"

# ================================================================

.PHONY: clean
clean:
	rm  *~

.PHONY: full_clean
full_clean:
	rm  -r -f  *~  Logs  *.log
