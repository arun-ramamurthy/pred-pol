library(dplyr)

# setwd("/Users/vaibhav/Documents/Year4_Senior/Semester 1/stat157/predictive-policing")

# Jong path
setwd("~/Desktop/School/STAT 157/predictive-policing")

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
  date <- as.Date(date)
  df$date <- as.Date(df$date)
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
get_coverage <- function(table, bins, d) {
  today <- table %>%
    filter(date == d) %>%
    mutate(crime = num_crimes != 0)
  detect <- 0
  fp <- 0
  for (i in 1:length(bins)) {
    if (bins[i] %in% today$bin) {
      detect <- detect + 1
    }
  }
  detect_rate <- detect / nrow(today)
  fp_rate <- (length(bins) - detect) / length(bins)
  return(c(detect_rate, fp_rate, nrow(today)))
}

# uses everything above to map ROC curve, taking in vector of DATES, 
# trailing days N to use, future days D to evaluate, and number of
# bins K
map_roc <- function(dates, n, d, k) {
  return(NA)
}



