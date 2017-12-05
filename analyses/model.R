library(tidyverse)
library(sf)

set.seed(157)

# Vaibhav path setwd("/Users/vaibhav/Documents/Year4_Senior/Semester 1/stat157/predictive-policing")
# Evan setwd("~/code/predictive-policing")
# Jong path setwd("~/Desktop/School/STAT 157/predictive-policing")

############
### DATA ###
############

# read drug crime data and format field names and types
oak <- read.csv("01_import/input/drug_crimes_with_bins.csv")
oak <- oak %>% rename(date=OCCURRED)
oak$date <- as.Date(as.character(oak$date), format = "%m/%d/%y")
max_bin <- max(oak$bin, na.rm=TRUE)

# read .RDS file of Oakland grid data and extract list of neighbors
oak_grid <- readRDS("01_import/input/oakland_grid_data.rds")
oak_bins <- oak_grid %>% st_as_sf
binIDs = row.names(oak_bins) %>% as.integer()
touching_dict <- sapply(binIDs, 
                        function(i){
                          st_touches(st_geometry(oak_bins) 
                                               %>% `[[`(i), st_geometry(oak_bins))
                          }
                        )

# read table of bin scores from Lum's 
# implementation of ETAS model
predpol_preds <- read.csv("02_run_predictive_policing/output/predpol_drug_predictions.csv", header=TRUE, sep=",")

# aggregate crime data to the bin-date level
oak_agg <- oak %>%
  group_by(bin, date) %>%
  summarize(num_crimes = n(), mean_lag = mean(LAG)) %>%
  ungroup()

get_trailing_table <- function(df, today, n) {
  # Takes in a dataframe containing date/num_crimes data and returns
  # another dataframe containing only crimes that took place within
  # n days before date, by bin.
  #
  # Args:
  #   df: A data frame containing the columns date and num_crimes.
  #   date: A date string formatted as "YYYY-MM-DD".
  #   n: Length of trailing window.
  #
  # Returns:
  #   A table containing filtered dates from previous n days
  today <- as.Date(today)
  df <- df %>% subset((date >= (today - n)) & (date < today))

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
  #   today: A date string formatted as "YYYY-MM-DD".
  #   r: Decay rate for exponential kernel.
  #
  # Returns:
  #   A single float denoting the bin score computed.
  today <- as.Date(today)
  df <- df %>%
    filter(bin == bin_num) %>%
    mutate(delta_days = as.numeric(today - as.Date(date))) %>%
    mutate(bin_score = num_crimes * exp(-r * delta_days))

  return(sum(df$bin_score))
}

get_bin_scores <- function(df, date, r) {
  # Takes in a data frame outputted from get_trailing_table 
  # and returns data frame with bin scores for each bin, 
  # for today's date DATE and discounted according to 
  # exponential decay with rate R.
  #
  # Args:
  #   df: Data frame containing date/bin_num/num_crimes data.
  #   date: A date string formatted as "YYYY-MM-DD".
  #   r: Decay rate for exponential kernel.
  #
  # Returns:
  #   A dataframe with a bin_score for each bin.
  
  unique_bins <- unique(df$bin)
  unique_bins <- unique_bins[!is.na(unique_bins)]
  
  bin_score <- sapply(unique_bins, get_bin_score, df, date, r)
  bin <- 1:max_bin
  temp <- data.frame(unique_bins, bin_score)
  output <- data.frame(bin) %>%
    left_join(temp, by = c("bin"="unique_bins"))
  output$bin_score[is.na(output$bin_score)] <- 0

  return(output)
}

get_neighbor_adjusted_bin_score <- function(bin, touching_dict, bin_scores, s) {
  # Takes in a bin number and table of bin scores
  # as well as a neighbor coefficient s and returns
  # the neighbor-adjusted bin score for bin.
  #
  # Args:
  #   bin: Bin number to compute score for.
  #   bin_scores: Data frame of bin scores, 
  #     unadjusted for neighbors.
  #   s: Neighbor coefficient (0 <= s <= 1).
  #
  # Returns:
  #   A neighbor-adjusted bin score for bin.
  neighbors <- touching_dict[[bin]]
  final <- s*sum(bin_scores$bin_score[neighbors]) +
    (1-s)*bin_scores$bin_score[bin]
  return(final)
}

get_predicted_bins <- function(df, touching_dict, today, k, n, r, s) {
  # Returns list of predicted bins for deployment.
  #
  # Args:
  #   df: Data frame containing date/bin/num_crimes data.
  #   today: A date string formatted as "YYYY-MM-DD".
  #   k: Number of bins to return (number of bins for 
  #     which police will be deployed).
  #   n: Length of trailing window.
  #   r: Decay rate for exponential kernel.
  #   s: Neighbor coefficient.
  #
  # Returns:
  #   Vector of K bins with the highest neigbor-adjusted bin scores.
  today <- as.Date(today)
  t <- get_trailing_table(df, today, n)
  bin_scores <- get_bin_scores(t, today, r)
  bins <- bin_scores$bin
  final_score <- sapply(bins, get_neighbor_adjusted_bin_score, touching_dict, bin_scores, s)
  new_bin_scores <- data.frame(bins, final_score)
  new_bin_scores <- new_bin_scores %>%
    arrange(desc(final_score))
  return(new_bin_scores$bins[1:k])
}

get_maximal_capture <- function(df, today, k) {
  # Gets highest achievable capture rate for today given
  # k deployments.
  #
  # Args:
  #   df: Data frame containing date/bin_num/num_crimes data.
  #   today: A date string formatted as "YYYY-MM-DD".
  #   k: Number of police deployments.
  #
  # Returns:
  #   Float representing percentage of crime captured with
  #     k most optimal deployments.
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

get_achieved_capture_rate <- function(df, touching_dict, today, k, n, r, s) {
  # Gets capture rate achieved by our model.
  #
  # Args:
  #   df: Data frame containing date/bin_num/num_crimes data.
  #   today: A date string formatted as "YYYY-MM-DD".
  #   k: Number of police deployments.
  #   n: Length of trailing window.
  #   r: Decay rate for exponential kernel.
  #   s: Neighbor coefficient.
  #
  # Returns:
  #   Float representing percentage of crime captured by our model for today.
  predBins <- get_predicted_bins(df, touching_dict, today, k, n, r, s)
  allDf <- df[df$date == today, ]
  lookDf <- allDf[allDf$bin %in% predBins, ]
  if (nrow(allDf) == 0) {
    return(1)
  } else {
    captureRate <- sum(lookDf$num_crimes) / sum(allDf$num_crimes)
    return(captureRate)  
  }
}

get_average_achieved_capture_rate <- function(date_samp, df, touching_dict, k, n, r, s) {
  # Gets capture rate achieved by our model, averaged across all dates in date_samp.
  #
  # Args:
  #   date_samp: Vector of dates, in string format "YYYY-MM-DD".
  #   df: Data frame containing date/bin_num/num_crimes data.
  #   k: Number of police deployments.
  #   n: Length of trailing window.
  #   r: Decay rate for exponential kernel.
  #   s: Neighbor coefficient.
  #
  # Returns:
  #   Float representing average percentage of crime captured by our model across
  #     all days in date_samp.
  print(paste0("Getting r = ", r, ", s = ", s))
  start <- Sys.time()
  capture_rates <- sapply(date_samp, function(date) {
    get_achieved_capture_rate(df, touching_dict, date, k, n, r, s)
  })
  end <- Sys.time()
  print(paste0("Time: ", end - start))
  print(paste0("Result: ", round(mean(capture_rates), 4)))
  return(mean(capture_rates))
}

######################
# PREDPOL Processing #
######################

get_predpol_capture_rate <- function(df, today, k, lum_data) {
  # Gets capture rate of Lum's model for K
  # deployments using date from DF of crime totals
  # on TODAY and predicted bins using LUM_DATA.
  #
  # Args:
  #   df: Data frame containing date/bin_num/num_crimes data.
  #   today: A date string formatted as "YYYY-MM-DD".
  #   k: Number of police deployments.
  #   lum_data: Data frame of bin_scores from Lum's ETAS model.
  #
  # Returns:
  #   Float representing percentage of crime captured by ETAS model for today.
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

get_average_predpol_capture_rate <- function(df, k, lum_data, date_samp) {
  # Get average capture rate of predpol for K deployments
  # using data from DF of crime totals and predicted bins using LUM_DATA
  #
  # Args:
  #   df: Data frame containing date/bin_num/num_crimes data.
  #   k: Number of police deployments.
  #   lum_data: Data frame of bin_scores from Lum's ETAS model.
  #   date_samp: Vector of dates, in string format "YYYY-MM-DD".
  #
  # Returns:
  #   Float representing average percentage of crime captured by ETAS model,
  #     across all days in date_samp.
  capture_rates <- sapply(date_samp, function(date) {
    get_predpol_capture_rate(df, date, k, lum_data)
  })
  return(mean(capture_rates))
}

########################
### PARAMETER TUNING ###
########################

parameter_tuning <- function(n_days) {
  params <- list(r = seq(from = 0, to = 0.1, by = 0.02),
                 s = seq(from = 0, to = 0.5, by = 0.05)) %>%
    cross_df()
  
  lum_rates <- c()
  mod <- function(r, s, n) {
    get_average_achieved_capture_rate(sampDates, oak_agg, 20, n, r, s)
  }
  
  for(i in 1:3) {
    print(paste("----- TRIAL", i, "-----"))
    sampDates <- base::sample(seq(as.Date("2010-12-28"), as.Date("2011-12-30"), by = 1), size = 50)
    capture_rate <- mapply(mod, params$r, params$s, n_days)
    params <- cbind(params, capture_rate)
    lum_rates <- c(lum_rates, get_average_predpol_capture_rate(oak_agg, 20, predpol_preds, sampDates))
    saveRDS(list(params = params, lum_rates = lum_rates), "analyses/temp_param_trial.rds")
  }
  
  names(params)[3:ncol(params)] <- letters[1:(ncol(params) - 2)]
  mean_capture_rate <- apply(params[,3:ncol(params)], 1, mean)
  results <- cbind(params, mean_capture_rate)
  results <- results %>%
    arrange(desc(mean_capture_rate))
  
  diff <- results[1, 3:(ncol(results) - 1)] - lum_rates
  mean_diff <- apply(diff, 1, mean)
  
  return(list(results=results, lum_rates=lum_rates))
}

set.seed(157)
results <- parameter_tuning(50)
saveRDS(results, "analyses/results50.rds")
