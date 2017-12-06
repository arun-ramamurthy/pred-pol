library(xtable)

setwd("~/Desktop/School/STAT 157/predictive-policing")
params <- readRDS("analyses/parameter_tuning.rds")
lum <- readRDS("analyses/lum_testing_rates.rds")

colnames(params) <- c("r", "s", paste0("sample", c(1:6)), "mean_capture_rate")
rownames(params) <- NULL
xtable(params[1:10, ], digits = 5,
       caption = "Parameter Grid Search Results.",
       label = "paramTable")


lumTable <- data.frame(t(lum))
lumTable$mean_capture_rate <- mean(as.numeric(lumTable[1,]))
colnames(lumTable) <- c(paste0("sample", c(1:6)), "mean_capture_rate")
xtable(lumTable, digits = 5, caption = "PredPol Accuracy across 6 Samples",
       label = "predPolAcc")


results50 <- readRDS("analyses/results50.rds")