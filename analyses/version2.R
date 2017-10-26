library(dplyr)
library(ggplot2)

# Vaibhav path setwd("/Users/vaibhav/Documents/Year4_Senior/Semester 1/stat157/predictive-policing")
# Jong path setwd("~/Desktop/School/STAT 157/predictive-policing")

oak <- read.csv("01_import/input/drug_crimes_with_bins.csv")
oak$OCCURRED <- as.Date(as.character(oak$OCCURRED), format = "%m/%d/%y")
oak_grid <- readRDS("01_import/input/oakland_grid_data.rds")
oak_outline <- readRDS("01_import/input/oakland_outline.rds")

oak_agg <- oak %>%
  group_by(bin, OCCURRED) %>%
  summarize(num_crimes = n(), mean_lag = mean(LAG)) %>%
  arrange(bin) %>%
  rename(date = OCCURRED) %>%
  ungroup()

# Takes in aggregated DF and returns data.frame
# containing total number of crimes over last N days 
# before DATE (not inclusive), per bin (grid),
# discounted according to exponential kernel (e^-r(T-t)).
# Returned table should be arranged by descending value 
# of kernelized number of crimes (bin_score)
# Note: Date needs to be formated as: "YYYY-MM-DD"
get_trailing_table <- function(df, date, n) {
  date <- as.Date(date)
  usedf <- df[(df$date >= (date - n)) & (df$date < date), ]
  output <- usedf %>%
    mutate(bin_score = get_bin_score(bin)) %>%
    arrange(desc(bin_score))
  return(output)
}

# Takes in BIN number and returns vector of neighbor bins
get_neighbors <- function(bin) {
  return(c())
}

# 
get_bin_score <- function(df, bin) {
  
}


