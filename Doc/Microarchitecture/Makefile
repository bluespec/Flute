TOP_FILE = Microarchitecture

# ================================================================

.PHONY: default
default:
	mkdir -p Aux_files
	pdflatex  -output-directory=Aux_files  $(TOP_FILE).tex
	cp -p Aux_files/$(TOP_FILE).pdf .

.PHONY: figures
figures:
	make -C Figures

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  Aux_files/*    Figures/*~

.PHONY: full_clean
full_clean: clean
	rm -r -f  $(TOP_FILE).pdf
