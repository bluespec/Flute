
TOP=mkTest

TESTCASE?=1
AWSTERIA_INFRA_DIR?=../../../../../AWSteria_Infra

BSC=bsc
BSCFLAGS+=-verilog
BSCFLAGS+=-u
BSCFLAGS+=-check-assert
BSCFLAGS+=-p +:..:../../../../src_Core/BSV_Additional_Libs:$(AWSTERIA_INFRA_DIR)/Platform_Sim/HW
BSCFLAGS+=-D INCLUDE_DDR_A -D SIM_FOR_VCU118
BSCFLAGS+=-D TESTCASE=$(TESTCASE)
BSCFLAGS+=-bdir build

default: sim

sim:
	mkdir -p build
	$(BSC) $(BSCFLAGS) -g $(TOP) Test_AXI4_Fabric.bsv
	$(BSC) $(BSCFLAGS) -e $(TOP)

clean:
	$(RM) -r build a.out mkTest.v obj_dir_a.out_*

.PHONY: default
