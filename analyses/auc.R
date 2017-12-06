
###############
### auc.R #####
###############

params <- readRDS("analyses/parameter_tuning.rds")
lum <- readRDS("analyses/lum_testing_rates.rds")

results50 <- readRDS("analyses/results50.rds")
View(results50$results)

ourRates <- c()
lumRates <- c()

source("analyses/model.R")

useDf <- subset(oak_agg, date > min(oak_agg$date) + 365 )
for (k in 10:30) {
  
  get_average_achieved_capture_rate(unique(oak_agg$date))
}
