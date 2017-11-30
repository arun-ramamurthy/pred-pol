library(dplyr)
library(ggplot2)

oak <- read.csv("01_import/input/drug_crimes_with_bins.csv")
oak <- oak %>% rename(date=OCCURRED)
oak$date <- as.Date(as.character(oak$date), format = "%m/%d/%y")
oak_grid <- readRDS("01_import/input/oakland_grid_data.rds")
oak_outline <- readRDS("01_import/input/oakland_outline.rds")
touching_dict <- readRDS("analyses/bin_touching_dictionary.rds")
predpol_preds <- read.csv("02_run_predictive_policing/output/predpol_drug_predictions.csv", header=TRUE, sep=",")

oak_agg <- oak %>%
  group_by(bin, date) %>%
  summarize(num_crimes = n(), mean_lag = mean(LAG)) %>%
  ungroup()

get_trailing_table <- function(df, date, n) {
  # Takes in a dataframe containing date/num_crimes data and returns
  # another dataframe containing the total number of crimes over the last
  # n days before date, per bin.
  #
  # Args:
  #   df: A data frame containing the columns date and num_crimes.
  #   date: A date string formatted as "YYYY-MM-DD".
  #   n: The window length (number of days to consider).
  #
  # Returns:
  #   A table containing filtered dates from previous n days
  date <- as.Date(date)
  daterange <- seq(from = date - n, length.out = n, by = 1)
  df <- df %>% filter(date %in% daterange)

  return(df)
}

get_bin_score <- function(bin_num, df, today, r) {
  # Takes in a dataframe containing date/bin_num/num_crimes data, and bin_num
  # and returns the bin score for the specific bin, weighted by
  # exponential decay.
  #
  # Args:
  #   bin_num: Bin number to compute score for.
  #   df: Data frame containing date/bin_num/num_crimes data.
  #   today: Date to compute score for "YYYY-MM-DD".
  #
  # Returns:
  #   A single float denoting the bin score computed.
  today <- as.Date(today)
  df <- df %>%
    filter(bin == bin_num) %>%
    mutate(delta_days = as.numeric(today - date)) %>%
    mutate(bin_score = num_crimes * exp(-r * delta_days))

  return(sum(df$bin_score))
}

# Takes in TRAILING_DF and returns data.frame
# with bin scores for each bin, with today's date DATE and
# discounted according to exponential kernel with rate R.
# Returned table should be arranged by descending value
# of kernelized number of crimes (bin_score)
# Note: Date needs to be formated as: "YYYY-MM-DD"
get_bin_scores <- function(df, date, r) {
  # Applies get_bin_score to every single bin in a dataframe.
  #
  # Args:
  #   df: Data frame containing data points.
  #   date: Date to compute score for "YYYY-MM-DD".
  #   r: Decay rate for exponential kernel.
  #
  # Returns:
  #   A dataframe with a bin_score for each bin.
  
  unique_bins <- unique(df$bin)
  unique_bins <- unique_bins[!is.na(unique_bins)]
  
  bin_score <- sapply(bin, get_bin_score, df, date, r)
  output <- data.frame(bin, bin_score)

  return(output %>% arrange(desc(bin_score)))
}

get_predicted_bins_helper <- function(bin, bin_scores, s) {
  neighbors <- touching_dict[[bin]]
  final <- s*sum(bin_scores$bin_score[which(bin_scores$bin %in% neighbors)]) +
    (1-s)*bin_scores$bin_score[which(bin_scores$bin == bin)]
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
  final_score <- sapply(bins, get_predicted_bins_helper, bin_scores, s)
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
    return(1)
  } else if (nrow(df) < k) {
    return(1)
  } else {
    total_crime <- sum(df$num_crimes)
    captured_crime <- sum(df$num_crimes[1:k])
    if (total_crime == 0) {
      return(1)
    } else {
      return(captured_crime/total_crime)
    }
  }
}

# Gets capture rate of model had we deployed K officers
# on TODAY using data from DF of crime totals
get_achieved_capture_rate <- function(df, today, k, n, r, s) {
  predBins <- get_predicted_bins(df, today, k, n, r, s)
  allDf <- df[df$date == today, ]
  lookDf <- allDf[allDf$bin %in% predBins, ]
  if (nrow(allDf) == 0) {
    return(1)
  } else {
    captureRate <- sum(lookDf$num_crimes) / sum(allDf$num_crimes)
    return(captureRate)  
  }
}

# Gets average capture rate across all dates for K
# deployments using data from DF of crime totals
get_average_achieved_capture_rate <- function(date_samp, df, k, n, r, s) {
  # print(paste0("Getting r = ", r, ", s = ", s))
  start <- Sys.time()
  capture_rates <- sapply(date_samp, function(date) {
    get_achieved_capture_rate(df, date, k, n, r, s)
  })
  end <- Sys.time()
  # print(end - start)
  return(mean(capture_rates))
}

######################
# PREDPOL Processing #
######################

# Gets capture rate of Kristian's model for K
# deployments using date from DF of crime totals
# on TODAY and predicted bins using LUM_DATA
get_predpol_capture_rate <- function(df, today, k, lum_data) {
  formatted_date = format(as.Date(c(today)), format="%Y.%m.%d")
  bin_scores = lum_data[paste("X", formatted_date, sep="")][[1]]
  binPreds <- data.frame(1:length(bin_scores), bin_scores)
  names(binPreds) <- c("bin","lumScore")
  binPreds <- binPreds %>%
    arrange(desc(lumScore))
  bin_final_preds <- binPreds$bin[1:k]
  filtered_df = filter(df, date == today)
  lookDf <- filtered_df[filtered_df$bin %in% bin_final_preds, ]
  if (nrow(filtered_df) == 0) {
    return(1)
  } else {
    total_crime <- sum(filtered_df$num_crimes)
    captured_crime <- sum(lookDf$num_crimes)
    return(captured_crime/total_crime)
  }
}

# Get average capture rate of predpol for K deployments
# using data from DF of crime totals and predicted bins using LUM_DATA
get_average_predpol_capture_rate <- function(df, k, lum_data, date_samp) {
  capture_rates <- sapply(date_samp, function(date) {
    get_predpol_capture_rate(df, date, k, lum_data)
  })
  return(mean(capture_rates))
}