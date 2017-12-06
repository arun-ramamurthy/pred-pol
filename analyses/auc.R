
###############
### auc.R #####
###############
setwd("~/Desktop/School/STAT 157/predictive-policing")
params <- readRDS("analyses/data/parameter_tuning.rds")
lum <- readRDS("analyses/data/lum_testing_rates.rds")

results50 <- readRDS("analyses/results50.rds")

ourRates <- c()
lumRates <- c()

source("analyses/model.R")

# Predpol only has data >= 2010-12-28 dates, and has some dates missing
predpolDates <- gsub("X", "", colnames(predpol_preds))[-1]
oakaggDates <- format(as.Date(unique(oak_agg$date)),  format="%Y.%m.%d")
toUseDates <- oakaggDates[oakaggDates %in% predpolDates]
toUseDates <- as.Date(gsub("\\.", "-", toUseDates))
useDf <- subset(oak_agg, date %in% toUseDates)

for (i in 10:30) {
  ourRates <- c(ourRates,
                get_average_achieved_capture_rate(
                  date_samp = unique(useDf$date), df = oak_agg, 
                  touching_dict = touching_dict, 
                  k = i, n = 365, r = 0, s = 0)
                )
  lumRates <- c(lumRates, 
                get_average_predpol_capture_rate(
                  df = oak_agg, date_samp = unique(useDf$date), 
                  k = i, lum_data = predpol_preds))
  cat("Finished running for k = ", i, "...\n")
}
names(ourRates) <- c(10:30)
names(lumRates) <- c(10:30)

aucData <- list(ourRates, lumRates)
saveRDS(aucData, file = "analyses/data/aucData.rds")

