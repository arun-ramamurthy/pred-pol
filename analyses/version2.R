library(dplyr)
library(ggplot2)

# Vaibhav path setwd("/Users/vaibhav/Documents/Year4_Senior/Semester 1/stat157/predictive-policing")
# Jong path 
setwd("~/code/predictive-policing")

oak <- read.csv("01_import/input/drug_crimes_with_bins.csv")
oak$OCCURRED <- as.Date(as.character(oak$OCCURRED), format = "%m/%d/%y")
oak_grid <- readRDS("01_import/input/oakland_grid_data.rds")
oak_outline <- readRDS("01_import/input/oakland_outline.rds")
touching_dict <- readRDS("analyses/bin_touching_dictionary.rds")

oak_agg <- oak %>%
  group_by(bin, OCCURRED) %>%
  summarize(num_crimes = n(), mean_lag = mean(LAG)) %>%
  arrange(bin) %>%
  rename(date = OCCURRED) %>%
  ungroup()

# Takes in aggregated DF (oak_agg) and returns data.frame
# containing total number of crimes over last N days
# before DATE (not inclusive), per bin (grid)
# Note: Date needs to be formated as: "YYYY-MM-DD"
get_trailing_table <- function(df, date, n) {
  date <- as.Date(date)
  daterange <- seq(from = date - n, length.out = n, by = 1)
  usedf <- df %>%
    filter(date %in% daterange)
  return(usedf %>%
           arrange(date, bin))
}

# Takes in BIN number and returns vector of neighbor bins
get_neighbors <- function(bin) {
  touching_dict[[bin]]
}

# Calculates kernelized bin score for BIN_NUM
# using crimes that fall within date range of DF
# where TODAY is today's date using exponential kernel
# (e^-r(T-t)) with r = R.
get_bin_score <- function(bin_num, df, today, r) {
  today <- as.Date(today)
  df <- df %>%
    filter(bin == bin_num) %>%
    mutate(bin_score = num_crimes*exp(-r*as.numeric(today - date)))
  return(sum(df$bin_score))
}

# Takes in TRAILING_DF and returns data.frame
# with bin scores for each bin, with today's date DATE and
# discounted according to exponential kernel with rate R.
# Returned table should be arranged by descending value
# of kernelized number of crimes (bin_score)
# Note: Date needs to be formated as: "YYYY-MM-DD"
get_bin_scores <- function(trailing_df, date, r) {
  bin <- unique(trailing_df$bin)[!is.na(unique(trailing_df$bin))]
  bin_score <- sapply(bin, get_bin_score, trailing_df, date, r)
  output <- data.frame(bin, bin_score)
  return(output %>% arrange(desc(bin_score)))
}

get_predicted_bins_helper <- function(bin, bin_scores) {
  neighbors <- get_neighbors(bin)
  final <- s*sum(bin_scores$bin_score[which(bin_scores$bin %in% neighbors)]) +
    bin_scores$bin_score[which(bin_scores$bin == bin)]
  return(final)
}

# Get predicted bins for deployment of K police on DATE using
# data from N days trailing using R rate of discounting,
# and scaling neighbor bin scores by S
get_predicted_bins <- function(df, date, k, n, r, s) {
  date <- as.Date(date)
  t <- get_trailing_table(df, date, n)
  bin_scores <- get_bin_scores(t, date, r)
  bins <- bin_scores$bin
  final_score <- sapply(bins, get_predicted_bins_helper, bin_scores)
  new_bin_scores <- data.frame(bins, final_score)
  new_bin_scores <- new_bin_scores %>%
    arrange(desc(final_score))
  return(new_bin_scores$bins[1:k])
}

# Gets the best capture rate achievable on TODAY given
# K deployments using DF of crime totals
get_maximal_capture <- function(df, today, k) {
  df <- df %>%
    filter(date == today) %>%
    arrange(desc(num_crimes))
  if (nrow(df) == 0) {
    return(0)
  } else if (nrow(df) < k) {
    return(1)
  } else {
    total_crime <- sum(df$num_crimes)
    captured_crime <- sum(df$num_crimes[1:k])
    return(captured_crime/total_crime)
  }
}

# Gets capture rate of model had we deployed K officers
# on TODAY using data from DF of crime totals
get_achieved_capture_rate <- function(df, today, k, n, r, s) {
  predBins <- get_predicted_bins(df, today, k, n, r, s)
  allDf <- df[df$date == today, ]
  lookDf <- allDf[allDf$bin %in% predBins, ]
  captureRate <- sum(lookDf$num_crimes) / sum(allDf$num_crimes)
  return(captureRate)
}

# Gets average capture rate across all dates for K
# deployments using data from DF of crime totals
sampDates <- base::sample(unique(oak_agg$date), size = 50)
get_average_achieved_capture_rate <- function(df, k, n, r, s, date) {
  all_dates = date
  #all_dates = unique(df$date)
  num_dates = length(all_dates)
  total_capture_rate = 0
  for (i in 1:num_dates) {
    total_capture_rate = total_capture_rate + get_achieved_capture_rate(df, all_dates[i], k, n, r, s)
    if (i %% 10 == 0) {
      cat("Finished", i, "th date for this", r, "iteration..\n")
    }
  }
  average_capture_rate = total_capture_rate / num_dates
  cat("Finished avg capture rate for r = ", r, "..\n")
  return(average_capture_rate)
}

######################
# PREDPOL Processing #
######################

predpol_preds = read.csv("02_run_predictive_policing/output/predpol_drug_predictions.csv", header=TRUE, sep=",")

# Gets capture rate of Kristian's model for K
# deployments using date from DF of crime totals
# on TODAY and predicted bins using LUM_DATA
get_predpol_capture_rate <- function(df, today, k, lum_data) {
  formatted_date = format(as.Date(c(today)), format="%Y.%m.%d")
  bin_indexes = sort.int(lum_data[paste("X", formatted_date, sep="")][[1]], index.return=TRUE, decreasing=TRUE)[[2]]
  df <- filter(df, OCCURRED == today)[bin_indexes,]
  if (length(df$bin) < k) {
    return(1)
  } else {
    total_crime <- sum(df$num_crimes)
    captured_crime <- sum(df$num_crimes[1:k])
    if (total_crime == 0) {
      return(0)
    } else {
      return(captured_crime/total_crime)
    }
  }
}

# Get average capture rate of predpol for K deployments
# using data from DF of crime totalsand predicted bins using LUM_DATA
get_average_predpol_capture_rate <- function(df, k, lum_data) {
  all_dates = unique(df$date)
  num_dates = length(all_dates)
  total_capture_rate = 0
  for (i in 1:num_dates) {
    total_capture_rate = total_capture_rate + get_predpol_capture_rate(df, all_dates[i], k, lum_data)
  }
  average_capture_rate = total_capture_rate / num_dates
}




# Testing
rVarious <- 
  sapply(seq(0, 0.1, 0.01), function(i) {
  return(get_average_achieved_capture_rate(oak_agg, 20, 365, i, 0.25, sampDates))
})

save(rVarious, file = "expRRates.RData")

plot(seq(0, 1, 0.1), rVarious)
