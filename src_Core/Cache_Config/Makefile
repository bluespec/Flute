.PHONY: help
help:
	@echo "Running 'make <target>' will generate one a BSV file"
	@echo "containing cache type, constant and functiion declarations"
	@echo "using info from configs specified in .yaml files"
	@echo "(generated BSV package/file name, cache size, associativity,"
	@echo " line size, phys addr size in bits)."
	@echo ""
	@echo "The generated BSV files are imported by BSV cache codes."
	@echo ""
	@echo "For other configs, create your own yaml files similar to"
	@echo "the yaml files in this directory,"
	@echo "then run the Python program Gen_BSV_Cache_Decls.py on them."

.PHONY: all
all: RV32_4KB_1way RV32_8KB_2way RV32_Sv32_4KB_1way RV32_Sv32_8KB_2way \
	RV64_4KB_1way RV64_8KB_2way RV64_Sv39_4KB_1way RV64_Sv39_8KB_2way

# ================================================================
# For RV32

.PHONY: RV32_4KB_1way
RV32_4KB_1way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV32_4KB_1way_args.yaml

.PHONY: RV32_Sv32_4KB_1way
RV32_Sv32_4KB_1way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV32_Sv32_4KB_1way_args.yaml

.PHONY: RV32_8KB_2way
RV32_8KB_2way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV32_8KB_2way_args.yaml

.PHONY: RV32_Sv32_8KB_2way
RV32_Sv32_8KB_2way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV32_Sv32_8KB_2way_args.yaml

# ================================================================
# For RV64

.PHONY: RV64_4KB_1way
RV64_4KB_1way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV64_4KB_1way_args.yaml

.PHONY: RV64_Sv39_4KB_1way
RV64_Sv39_4KB_1way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV64_Sv39_4KB_1way_args.yaml

.PHONY: RV64_8KB_2way
RV64_8KB_2way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV64_8KB_2way_args.yaml

.PHONY: RV64_Sv39_8KB_2way
RV64_Sv39_8KB_2way:
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV64_Sv39_8KB_2way_args.yaml

# ================================================================

clean:
	rm -r -f  *~  *.bsv
