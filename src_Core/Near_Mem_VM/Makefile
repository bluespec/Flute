.PHONY: help
help:
	@echo "Running 'make gen' will generate two files"
	@echo "   Cache_Decls_RV32.bsv  (for RV32) and"
	@echo "   Cache_Decls_RV64.bsv  (for RV64)"
	@echo "from configs specified in .yaml files"
	@echo "(cache size, associativity, line size, phys addr size in bits)."
	@echo "The generated BSV files are imported by MMU_Cache.bsv"
	@echo "to configure it appropriately."
	@echo ""
	@echo "You can run the commands below explicitly with alternative"
	@echo "yaml files if you want different configs."

.PHONY: gen
gen:
	@echo "Generating Cache_Decls_RV32.bsv for RV32"
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV32_Sv32_8KB_2way_args.yaml
	@echo "Generating Cache_Decls_RV64.bsv for RV64"
	./Gen_BSV_Cache_Decls.py  --yaml_file  RV64_Sv39_8KB_2way_args.yaml
