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

highest_crime_bins <- oak %>%
  group_by(bin) %>%
  summarize(num_crimes = n()) %>%
  arrange(desc(num_crimes))

plotBin <- function(bin_num) {
  d <- oak_agg %>% 
    filter(bin == bin_num)
  if (nrow(d) < 10) {
    stop(paste("Only", nrow(d), "crimes found."))
  }
  date <- seq(from = min(d$date), to = max(d$date), by = 1)
  data <- as.data.frame(date)
  data <- data %>%
    left_join(d %>% select (date, num_crimes))
  for (i in 1:nrow(data)) {
    if (is.na(data$num_crimes[i])) {
      data$num_crimes[i] <- 0
    }
  }
  plot(data$date, data$num_crimes, type = "l", 
       main = paste0("Crimes from Bin #", bin_num), xlab = "Date", ylab = "Number of Crimes")
}

# takes in aggregated TABLE and returns data.frame
# containing total number of crimes over last N days 
# before DATE (not inclusive), per bin (grid)
# sorted by descending number of crimes
# Note: Date needs to be formated as: "YYYY-MM-DD"
get_trailing_table <- function(df, date, n) {
  usedf <- df[(df$date >= (date - n)) & (df$date < date), ]
  output <- 
    usedf %>%
    group_by(., bin) %>%
    summarise(numCrimes_within = sum(num_crimes)) %>%
    arrange(desc(numCrimes_within))
    
  return(output)
}

# takes in TRAILING_TABLE from get_trailing_table and returns 
# vector of K bins with highest crime occurrence
get_bins <- function(trailing_table, k) {
  return(trailing_table$bin[1:k])
}

# takes in a vector of BINS from get_bins and returns two-length vector 
# containing percentage of crimes detected should D days after DATE 
# (inclusive) and percentage of bins with no crimes (~false positive)
get_coverage <- function(table, bins, date, d) {
  daterange <- seq(from = date, length.out = d, by = 1)
  future <- table %>%
    filter(date %in% daterange)
  detect <- 0
  fp <- 0
  for (bin in bins) {
    if (bin %in% future$bin) {
      detect <- detect + 1
    }
  }
  detect_rate <- detect / length(unique(future$bin))
  fp_rate <- (length(bins) - detect) / length(bins)
  return(c(detect_rate, fp_rate, length(unique(future$bin))))
}

# uses everything above to map ROC curve, taking in TABLE of crime data, 
# trailing days N to use, and range of number of bins K_VALS
map_roc <- function(table, n, k_vals, d) {
  fps <- c()
  detections <- c()
  firstdate <- min(table$date) + n
  lastdate <- max(table$date)
  dates <- seq(from = firstdate, to = lastdate, by = 1)
  for (k in k_vals) {
    print(paste("Checking k =", k))
    d_temp <- c()
    f_temp <- c()
    for (date in dates) {
      trailing_table <- get_trailing_table(table, date, n)
      bins <- get_bins(trailing_table, k)
      coverage <- get_coverage(table, bins, date, d)
      d_temp <- c(d_temp, coverage[1])
      f_temp <- c(f_temp, coverage[2])
    }
    detections <- c(detections, mean(d_temp, na.rm = TRUE))
    fps <- c(fps, mean(f_temp, na.rm = TRUE))
  }
  toplot <- data.frame(k_vals, fps, detections)
  return(toplot %>%
    ggplot(aes(x = fps, y = detections)) +
    geom_text(aes(label = k_vals)) +
    ggtitle("ROC") + xlab("False Positive Rate") + ylab("True Positive Rate") +
    geom_abline(slope = 1, intercept = 0))
}

roc365 <- map_roc(oak_agg, 365, 1:30, 30)
roc365_1 <- map_roc(oak_agg, 365, 1:30, 1)

