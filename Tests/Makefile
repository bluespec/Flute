###  -*-Makefile-*-

# Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

# ================================================================
# Regression: run all ISA tests relevant for RV32IMU
# using a build of an RV32IMU simulator.

SIM_32_DIR = ../builds/RV32IMU_Bluesim
# SIM_32_DIR = ../builds/RV32IMU_iverilog
# SIM_32_DIR = ../builds/RV32IMU_verilator


.PHONY: test32
test32:
	@echo "Running RV32IMU regressions; saving logs in Logs/"
	./Run_regression.py \
		$(SIM_32_DIR)/exe_HW_sim \
		RV32IMU  ./isa  ./Logs
	@echo "Finished running RV32IMU regressions; saved logs in Logs/"

# ================================================================
# Regression: run all ISA tests relevant for RV64AIMSU
# using a build of an RV64AIMSU simulator.

# SIM_64_DIR = ../builds/RV64AIMSU_Bluesim
SIM_64_DIR = ../builds/RV64AIMSU_verilator
# SIM_64_DIR = ../builds/RV64AIMSU_iverilog

.PHONY: test64
test64:
	@echo "Running RV64AIMSU regressions; saving logs in Logs/"
	./Run_regression.py \
		$(SIM_64_DIR)/exe_HW_sim \
		RV64AIMSU ./isa  ./Logs
	@echo "Finished running RV64AIMSU regressions; saved logs in Logs/"

# ================================================================

.PHONY: clean
clean:
	rm  *~

.PHONY: full_clean
full_clean:
	rm  -r -f  *~  Logs  *.log
