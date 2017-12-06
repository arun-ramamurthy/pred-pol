library(xtable)
library(ggplot2)
library(reshape2)

setwd("~/Desktop/School/STAT 157/predictive-policing")
params <- readRDS("analyses/parameter_tuning.rds")
lum <- readRDS("analyses/lum_testing_rates.rds")


# Parameter Tuning Results #
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


# AUC Curve Generation #
aucDf <- readRDS("analyses/aucData.rds")
names(aucDf) <- c("ourRates", "lumRates")
aucDf <- lapply(aucDf, function(x) 1 - x)
ggDf <- data.frame(k = c(10:30), Model = aucDf[[1]], PredPol = aucDf[[2]])
rownames(ggDf) <- NULL

ggplot(ggDf, aes(x = k)) +
  geom_line(aes(y = Model, color = "Model")) +
  geom_line(aes(y = PredPol, color = "PredPol")) +
  ggtitle("PredPol vs. Simple Model Comparison on % Crimes Missed") +
  labs(x = "Number of Police Officers Deployed", y = "% Crimes Missed", 
       colour = NULL)
