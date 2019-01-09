default: compile  link
all:     compile  link  simulate

TOP = Testbench

TOPFILE = $(TOP).bsv
TOPMODULE = mk$(TOP)

# BSCFLAGS = -keep-fires -aggressive-conditions -no-warn-action-shadowing -no-inline-rwire
# BSCFLAGS = -keep-fires -aggressive-conditions -no-inline-rwire -show-range-conflict -show-schedule
BSCFLAGS = -D RV32 \
	   -keep-fires \
	   -aggressive-conditions \
	   -suppress-warnings G0020 \
	   -show-schedule


# ----------------------------------------------------------------
# FOR BLUESIM

ISA_DECLS_DIR = $(HOME)/Projects/RISCV/Bluespec_RISCV/ISA
TRX_DIR       = $(HOME)/Projects/RISCV/Bluespec_RISCV/Fabrics/TRX
ADDL_LIBS_DIR = $(HOME)/Projects/RISCV/Bluespec_RISCV/BSV_Additional_Libs/BSV

BSCDIRS_BSIM  = -simdir build_bsim -bdir build -info-dir build
BSCPATH_BSIM  = -p .:..:$(ISA_DECLS_DIR):$(TRX_DIR):$(ADDL_LIBS_DIR):%/Prelude:%/Libraries

build_bsim:
	mkdir -p $@

build:
	mkdir -p $@

.PHONY: compile
compile: build_bsim build
	@echo Compiling...
	bsc -u -sim $(BSCDIRS_BSIM) $(BSCFLAGS) $(BSCPATH_BSIM) $(TOPFILE)
	@echo Compilation finished

.PHONY: link
link:
	@echo Linking...
	bsc -e $(TOPMODULE) $(BSCFLAGS) -parallel-sim-link 8 -sim  -o ./$(TOP)_bsim_exe  $(BSCDIRS_BSIM)  $(BSCPATH_BSIM)
	@echo Linking finished

.PHONY: simulate
simulate:
	@echo Simulation...
	logsave  bsim.log  ./$(TOP)_bsim_exe  -V
	@echo Simulation finished

# ----------------------------------------------------------------
# FOR VERILOG

BSCDIRS_V = -vdir verilog  -bdir build_v  -info-dir build_v
BSCPATH_V = -p .:./$(SRC_BSV):%/Prelude:%/Libraries:%/Libraries/TLM3

# Set VSIM to desired Verilog simulator
# VSIM = modelsim
VSIM ?= cvc
# VSIM ?= iverilog

build_v:
	mkdir -p $@

verilog:
	mkdir -p $@

.PHONY: rtl
rtl: build_v verilog
	@echo Verilog generation ...
	bsc -u -elab -verilog $(BSCDIRS_V) $(BSCFLAGS) $(BSCPATH_V) $(TOPFILE)
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
