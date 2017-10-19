library(dplyr)

oak <- read.csv("/Users/vaibhav/Documents/Year4_Senior/Semester 1/stat157/predictive-policing/01_import/input/drug_crimes_with_bins.csv")
oak$OCCURRED <- as.Date(as.character(oak$OCCURRED), format = "%m/%d/%y")
oak_grid <- readRDS("/Users/vaibhav/Documents/Year4_Senior/Semester 1/stat157/predictive-policing/01_import/input/oakland_grid_data.rds")
oak_outline <- readRDS("/Users/vaibhav/Documents/Year4_Senior/Semester 1/stat157/predictive-policing/01_import/input/oakland_outline.rds")


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
  plot(data$date, data$num_crimes, type = "l", main = paste0("Crimes from Bin #", bin_num))
}
