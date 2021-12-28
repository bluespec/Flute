default: compile  link
all:     compile  link  simulate

TOP = Testbench

TOPFILE = $(TOP).bsv
TOPMODULE = mk$(TOP)

BSCFLAGS += -D RV64
BSCFLAGS += -D FABRIC64

BSCFLAGS += -keep-fires
BSCFLAGS += -aggressive-conditions
BSCFLAGS += -suppress-warnings G0020
BSCFLAGS += -no-warn-action-shadowing
BSCFLAGS += -no-inline-rwire
BSCFLAGS += -show-range-conflict
# BSCFLAGS += -show-schedule

# ----------------------------------------------------------------
# FOR BLUESIM

FLUTE_REPO ?= PLEASE_DEFINE_FLUTE_REPO

BSCDIRS_BSIM  = -simdir build_bsim -bdir build -info-dir build

BSCPATH_BSIM := $(BSCPATH_BSIM):..
BSCPATH_BSIM := $(BSCPATH_BSIM):$(FLUTE_REPO)/src_Core/ISA
BSCPATH_BSIM := $(BSCPATH_BSIM):$(FLUTE_REPO)/src_Core/Core_v2
BSCPATH_BSIM := $(BSCPATH_BSIM):$(FLUTE_REPO)/src_Testbench/Fabrics/AXI4
BSCPATH_BSIM := $(BSCPATH_BSIM):$(FLUTE_REPO)/src_Core/BSV_Additional_Libs
BSCPATH_BSIM := $(BSCPATH_BSIM):+

build_bsim:
	mkdir -p $@

build:
	mkdir -p $@

.PHONY: compile
compile: build_bsim build
	@echo Compiling...
	bsc -u -sim $(BSCDIRS_BSIM) $(BSCFLAGS) -p $(BSCPATH_BSIM) $(TOPFILE)
	@echo Compilation finished

.PHONY: link
link:
	@echo Linking...
	bsc -e $(TOPMODULE) $(BSCFLAGS) -parallel-sim-link 8 -sim \
		-o ./$(TOP)_bsim_exe  $(BSCDIRS_BSIM)  -p $(BSCPATH_BSIM)
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	logsave  bsim.log  ./$(TOP)_bsim_exe  -V
	@echo Simulation finished

# ----------------------------------------------------------------
# FOR VERILOG

BSCDIRS_V = -vdir verilog  -bdir build_v  -info-dir build_v
BSCPATH_V = ..:$(ISA_DECLS_DIR):$(ADDL_LIBS_DIR):+

# Set VSIM to desired Verilog simulator
# VSIM = modelsim
# VSIM ?= cvc
VSIM ?= iverilog

build_v:
	mkdir -p $@

verilog:
	mkdir -p $@

.PHONY: rtl
rtl: build_v verilog
	@echo Verilog generation ...
	bsc -u -elab -verilog $(BSCDIRS_V) $(BSCFLAGS) -p $(BSCPATH_V) $(TOPFILE)
	@echo Verilog generation finished

.PHONY: vlink
vlink:
	bsc -v -e $(TOPMODULE) -verilog -o ./out_v -vdir verilog -vsim $(VSIM) -keep-fires \
		verilog/$(TOPMODULE).v

.PHONY: vsim
vsim:
	@echo Simulation...
	./out_v
	@echo Simulation finished

# ----------------------------------------------------------------

.PHONY: clean
clean:
	rm -f  *~  src_*/*~  src_*/*.o  build/*  build_bsim/*  build_v/* *.cxx *.h *.o

.PHONY: full_clean
full_clean: clean
	rm -r -f  *_bsim_exe  *.so  out_v  verilog  build  build_bsim  dump.vcd  bsim.log
