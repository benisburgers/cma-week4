# Task 1: Write your own functions

## Create a function for our Euclidean distance calculation.


calc_dist <- function(point1, point2) {
  sqrt((point1[1]-point2[1])^2+(point1[2]-point2[2])^2)
}

test_calc_dist <- calc_dist(c(10, 15), c(10, 10))
test_calc_dist

# Task 2: Prepare Analysis

## Load libraries

library(readr)        
library(dplyr)        
library(ggplot2)      
library(lubridate)

## Import Data

wildschwein <- read_delim('wildschwein_BE_2056.txt')
wildschwein

## Filter Data

### Check how many individuals & the date range

unique(wildschwein$TierName)
min(wildschwein$DatetimeUTC)
max(wildschwein$DatetimeUTC)

### Filter by name & date

wildschwein_filtered <- wildschwein %>%
  filter(TierName == "Rosa" | TierName == "Sabi") %>%
  filter(DatetimeUTC >= "2015-04-01" & DatetimeUTC <= "2015-04-15")

unique(wildschwein_filtered$TierName)
min(wildschwein_filtered$DatetimeUTC)
max(wildschwein_filtered$DatetimeUTC)
