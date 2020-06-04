.PHONY: help
help:
	@echo "Running 'make <target>' will generate two files"
	@echo "   Cache_Decls_RV32.bsv  (for RV32) and"
	@echo "   Cache_Decls_RV64.bsv  (for RV64)"
	@echo "from configs specified in .yaml files"
	@echo "(cache size, associativity, line size, phys addr size in bits)."
	@echo "The generated BSV files are imported by MMU_Cache.bsv"
	@echo "to configure it appropriately."
	@echo ""
	@echo "Targets:"
	@echo "    make 8KB_2way"
	@echo "    make 4KB_direct_mapped     (i.e., 1 way)"
	@echo ""
	@echo "For other configs, create your own yaml files similar to"
	@echo "the yaml files in this directory,"
	@echo "then run the Python program Gen_BSV_Cache_Decls.py on them."

.PHONY: 8KB_2way
8KB_2way:
	@echo "Generating 8KB, 2-way Cache_Decls_RV32.bsv for RV32"
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV32_Sv32_8KB_2way_args.yaml
	@echo "Generating 8KB, 2-way Cache_Decls_RV64.bsv for RV64"
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV64_Sv39_8KB_2way_args.yaml

.PHONY: 4KB_direct_mapped
4KB_direct_mapped:
	@echo "Generating 4KB, direct-mapped (1-way) Cache_Decls_RV32.bsv for RV32"
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV32_Sv32_args.yaml
	@echo "Generating 4KB, direct-mapped (1-way) Cache_Decls_RV64.bsv for RV64"
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV64_Sv39_args.yaml
