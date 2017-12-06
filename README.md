# Model Analysis: _To Predict and serve?_

This repository contains code, data and tests to analyze the PredPol system presented in "[To Predict and Serve?](http://onlinelibrary.wiley.com/doi/10.1111/j.1740-9713.2016.00960.x/full)," published in volume 13 issue 5 of __Significance__, and authored by [Kristian Lum](https://hrdag.org/people/kristian-lum-phd/) and [William Isaac](https://wsisaac.com) of the Human Rights Data Analysis Group (HRDAG). The original repository which contains the replication materials for the paper can be found in  [HRDAG/predictive-policing](https://github.com/HRDAG/predictive-policing).

Our analysis is entirely contained in the analyses/ folder.

Our model uses past crime data in individual geographical "bins" weighted by an exponential decay kernel to compute bin scores, and we would use these to determine the likelihood of crimes for the next day.

## Data

- drug_crimes_with_bins.csv — Data collected by [OpenOakland](https://www.openoakland.org/oakcrimedata/).
- bin_touching_dictionary.rds — Bin numbers mapped to a list of neighboring bin numbers.
- lum_testing_rates.rds — Capture rates for Kristian Lum's model.
- parameter_tuning.rds — Capture rates for our model with different parameters.

## Code

- auc.R — Generates an ROC curve to compare results.
- model.R — Contains code for our model.
- paramTuning.R — Uses model.R to tune its parameters.
- resultGenerate.R — Plots our accuracy against Kristen's accuracy for sampled dates.



