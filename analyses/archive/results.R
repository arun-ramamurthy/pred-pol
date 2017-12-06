source('analyses/model.R')

set.seed(1543)

###############
### TESTING ###
###############

# neighbor_rates <- seq(0, 0.5, by = 0.1)

sampDates <- base::sample(seq(as.Date("2010-12-28"), as.Date("2011-12-30"), by = 1), size = 50)
# sampDates <- seq(as.Date("2010-12-28"), as.Date("2011-12-30"), by = 1)

# our_captures <- c()
# for(s in neighbor_rates) {
#   our_captures <- c(our_captures, get_average_achieved_capture_rate(oak_agg, 20, 365, 0.02, s, sampDates))
# }
# 
# plot(neighbor_rates,our_captures)

our_capture <- get_average_achieved_capture_rate(oak_agg, 20, 365, 0.02, 0.25, sampDates)
predPol_capture <- get_average_predpol_capture_rate(oak_agg, 20, predpol_preds, sampDates)

our_capture
predPol_capture

for(i in 1:5) {
  sampDates <- base::sample(seq(as.Date("2010-12-28"), as.Date("2011-12-30"), by = 1), size = 50)
  our_capture <- get_average_achieved_capture_rate(oak_agg, 20, 365, 0.02, 0.25, sampDates)
  predPol_capture <- get_average_predpol_capture_rate(oak_agg, 20, predpol_preds, sampDates)
  
  print(paste0("Us: ", our_capture, " | PredPol: ", predPol_capture))
}


# variousR <- 
#   sapply(seq(0, 0.1, 0.01), function(i) {
#   return(get_average_achieved_capture_rate(oak_agg, 20, 365, i, 0.25, sampDates))
# })
# 
# variousS <- 
#   sapply(seq(0, 0.5, 0.05), function(i) {
#     return(get_average_achieved_capture_rate(oak_agg, 20, 365, 0.02, i, sampDates))
#   })

# save(rVarious, file = "expRRates.RData")
# 
# plot(seq(0, 0.1, 0.01), variousR, main = "Optimal R")
# plot(seq(0, 0.5, 0.05), variousS, main = "Optimal S")
