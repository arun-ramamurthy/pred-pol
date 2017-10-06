# Makefile
#
# :Authors: 	KL
# :Maintainers: KL 

# ============================================


.PHONY: all clean

all: \
		output/predpol_drug_predictions.csv

output/predpol_drug_predictions.csv: \
		src/apply_predpol.py \
		src/predpol.py \
		input/drug_crimes_with_bins.csv \
		src/Makefile
	python $< \
		--drug_crimes_with_bins=input/drug_crimes_with_bins.csv \
		--global_start='2010/07/01' \
		--global_end='2011/12/31' \
		--observed=output/predpol_drug_observed \
		--predictions=output/predpol_drug_predictions \
		--predpol_window=180 \
		--begin_predpol=0 \
		--add_crimes_logical=FALSE \
		--percent_increase=0.0