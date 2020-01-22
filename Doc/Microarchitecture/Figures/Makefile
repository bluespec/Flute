.PHONY: all_svg_to_png
all_svg_to_png:
	make  FIG=Fig_100_Top_HW_Side  svg_to_png
	make  FIG=Fig_200_SoC_Top      svg_to_png
	make  FIG=Fig_300_mkCore       svg_to_png
	make  FIG=Fig_500_mkCPU_Flute  svg_to_png

.PHONY: svg_to_png
svg_to_png:
	inkscape --file=$(FIG).svg \
		--export-png=$(FIG).png \
		--export-area-drawing \
		-d 300 -y 0.0
