# Generates fn_read_ROM_RV32.bsvi and fn_read_ROM_RV64.bsvi
# in a temporary directory 'tmpdir/'.
# Manually copy these to .., if ok.

default: tmpdir/fn_read_ROM_RV32.bsvi  tmpdir/fn_read_ROM_RV64.bsvi

tmpdir/fn_read_ROM_RV64.bsvi: tmpdir  gen_bootrom.exe
	./gen_bootrom.exe  RV64  imaus  >  tmpdir/boot_ROM_RV64.memhex
	./Gen_BSV_fn_read_ROM.py  tmpdir/boot_ROM_RV64.memhex  tmpdir/fn_read_ROM_RV64.bsvi

tmpdir/fn_read_ROM_RV32.bsvi: tmpdir  gen_bootrom.exe
	./gen_bootrom.exe  RV32  imaus  >  tmpdir/boot_ROM_RV32.memhex
	./Gen_BSV_fn_read_ROM.py  tmpdir/boot_ROM_RV32.memhex  tmpdir/fn_read_ROM_RV32.bsvi

gen_bootrom.exe: tmpdir  gen_bootrom.cc
	$(CXX)  -o $@  -std=c++11  gen_bootrom.cc

tmpdir:
	mkdir -p tmpdir

.PHONY: full_clean
full_clean:
	rm -r -f  *~  tmpdir  gen_bootrom.exe
