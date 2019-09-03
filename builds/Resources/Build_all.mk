###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# Build all the "standard" builds and test them
# This Makefile should be invoked in the 'builds' directory

CPU=Flute
REPO ?= $(HOME)/GitHub/$(CPU)

.PHONY: help
help:
	@echo "    Usage:    make build_all"
	@echo "    This Makefile should be invoked in the 'builds' directory"
	@echo ""
	@echo "    Builds and tests all 'standard' builds, i.e., all combinations of:"
	@echo "            RV32/ RV64"
	@echo "         X  ACIMU/ ACDFIMSU"
	@echo "         X  Bluesim/ iverilog/ verilator"
	@echo ""
	@echo "    (needs Bluespec bsc compiler/Bluesim simulator license)"
	@echo ""
	@echo "    Temporary note: iverilog tests are not automated, pending"
	@echo "        fixing the C-import functionality."

.PHONY: build_all
build_all:  build_all_bluesim  build_all_verilator  build_all_iverilog

.PHONY: build_all_bluesim
build_all_bluesim:
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ACIMU     SIM=bluesim      build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ACDFIMSU  SIM=bluesim      build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ACIMU     SIM=bluesim      build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ACDFIMSU  SIM=bluesim      build_and_test

.PHONY: build_all_verilator
build_all_verilator:
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ACIMU     SIM=verilator    build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ACDFIMSU  SIM=verilator    build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ACIMU     SIM=verilator    build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ACDFIMSU  SIM=verilator    build_and_test

.PHONY: build_all_iverilog
build_all_iverilog:
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ACIMU     SIM=iverilog     build_and_test_iverilog
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ACDFIMSU  SIM=iverilog     build_and_test_iverilog
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ACIMU     SIM=iverilog     build_and_test_iverilog
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ACDFIMSU  SIM=iverilog     build_and_test_iverilog

.PHONY: build_and_test
build_and_test:
	$(REPO)/builds/Resources/mkBuild_Dir.py  $(REPO)  $(ARCH)  $(SIM)
	logsave  build_and_test.log  make -C  $(ARCH)_$(CPU)_$(SIM)  all  test  isa_tests
	mv  build_and_test.log  $(ARCH)_$(CPU)_$(SIM)

.PHONY: build_and_test_iverilog
build_and_test_iverilog:
	$(REPO)/builds/Resources/mkBuild_Dir.py  $(REPO)  $(ARCH)  $(SIM)
	logsave  build_and_test.log  make -C  $(ARCH)_$(CPU)_$(SIM)  all
	mv  build_and_test.log  $(ARCH)_$(CPU)_$(SIM)

.phony: full_clean
full_clean:
	make  -C RV32ACIMU_$(CPU)_bluesim     full_clean
	make  -C RV32ACDFIMSU_$(CPU)_bluesim  full_clean
	make  -C RV64ACIMU_$(CPU)_bluesim     full_clean
	make  -C RV64ACDFIMSU_$(CPU)_bluesim  full_clean
#
	make  -C RV32ACIMU_$(CPU)_verilator     full_clean
	make  -C RV32ACDFIMSU_$(CPU)_verilator  full_clean
	make  -C RV64ACIMU_$(CPU)_verilator     full_clean
	make  -C RV64ACDFIMSU_$(CPU)_verilator  full_clean
#
	make  -C RV32ACIMU_$(CPU)_iverilog     full_clean
	make  -C RV32ACDFIMSU_$(CPU)_iverilog  full_clean
	make  -C RV64ACIMU_$(CPU)_iverilog     full_clean
	make  -C RV64ACDFIMSU_$(CPU)_iverilog  full_clean
