# Makefile
#
# :Authors: 	KL
# :Maintainers: KL \ WI

# ============================================


.PHONY: all clean

all: \
		output/results_figure.pdf

output/results_figure.pdf: \
		src/MakeFigures.R \
		input/predpol_drug_predictions.csv \
		input/oakland_grid_data.rds \
		input/oakland_outline.rds \
		src/Makefile
	Rscript --vanilla $< \
		--pp_predictions=input/predpol_drug_predictions.csv \
		--oakland_grid=input/oakland_grid_data.rds \
		--oakland_outline=input/oakland_outline.rds \
		--out_figure=$@
