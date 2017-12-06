# Jong Exploration

setwd("~/Desktop/School/STAT 157/predictive-policing/01_import/")
list.files()

gridData <- readRDS("input/oakland_grid_data.rds")
drugCrimes <- read.csv('input/drug_crimes_with_bins.csv', stringsAsFactors = F)
outline <- readRDS("input/oakland_outline.rds")


library(dplyr)

drugCrimes$UID <- seq(1:nrow(drugCrimes))

numPerBinDate <-
  drugCrimes %>% 
  group_by(., bin, OCCURRED) %>%
  summarise(., count = n_distinct(UID))

hist(numPerBinDate$count, breaks = 12)
plot(density(numPerBinDate$count))

max(drugCrimes$OCCURRED)
min(drugCrimes$OCCURRED)

length(gridData)
