# ================================================================
# Makefile to convert SVG files (could be created in InkScape) to PNG
# using inkscape as a command-line convertor. 

FIGS_PNG = \
    Fig_100_Top_HW_Side.png \
    Fig_200_SoC_Top.png \
    Fig_300_mkCore.png \
    Fig_310_mkCore_v2.png \
    Fig_320_mkCore_Resets.png \
    Fig_500_mkCPU_Flute.png \
    Fig_600_Near_Mem_VM_WB_L1_L2.png \
    Fig_600_Near_Mem_VM_WB_L1.png \


all: $(FIGS_PNG)

$(FIGS_PNG): %.png: %.svg
	inkscape --export-filename=$@  --export-area-drawing  -d 300 -y 0.0 $<

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~

.PHONY: full_clean
full_clean:
	rm -r -f  *~  $(FIGS_PNG)

# ================================================================
