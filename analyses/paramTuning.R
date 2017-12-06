source("analyses/model.R")

library(plotly)

########################
### paramTuning.R ######
########################

set.seed(157)

params <- list(r = seq(from = 0, to = 0.1, by = 0.02),
               s = seq(from = 0, to = 0.5, by = 0.05)) %>%
  cross_df()
lum_rates <- c()

mod <- function(r, s) {
  get_average_achieved_capture_rate(sampDates, oak_agg, 20, 365, r, s)
}

for(i in 1:6) {
  print(paste("----- TRIAL", i, "-----"))
  sampDates <- base::sample(seq(as.Date("2010-12-28"), as.Date("2011-12-30"), by = 1), size = 50)
  capture_rate <- mapply(mod, params$r, params$s)
  params <- cbind(params, capture_rate)
  lum_rates <- c(lum_rates, get_average_predpol_capture_rate(oak_agg, 20, predpol_preds, sampDates))
}

names(params)[3:ncol(params)] <- letters[1:(ncol(params) - 2)]
mean_capture_rate <- apply(params[,3:ncol(params)], 1, mean)
results <- cbind(params, mean_capture_rate)
results <- results %>%
  arrange(desc(mean_capture_rate))

diff <- results[1, 3:(ncol(results) - 1)] - lum_rates
mean_diff <- apply(diff, 1, mean)

plot_ly(results, type = "heatmap", x = results$r, y = results$s, z = results$mean_capture_rate)