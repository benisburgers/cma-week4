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

### Check how many individuals & the date range before filtering

unique(wildschwein$TierName)
min(wildschwein$DatetimeUTC)
max(wildschwein$DatetimeUTC)

### Filter by name & date
### (Define the timezone of the threshold time, otherwise R uses system time)

wildschwein_filtered <- wildschwein %>%
  filter(TierName == "Rosa" | TierName == "Sabi") %>%
  filter(DatetimeUTC >= as.POSIXct("2015-04-01", tz = "UTC") & DatetimeUTC <= as.POSIXct("2015-04-15", tz = "UTC"))

### Check how many individuals & the date range after filtering

unique(wildschwein_filtered$TierName)
min(wildschwein_filtered$DatetimeUTC)
max(wildschwein_filtered$DatetimeUTC)

# Task 3: Create Join Key

### Round DatetimeUTC to the nearest 15 minutes interval (remove the random delay of a few seconds)
wildschwein_filtered <- wildschwein_filtered %>%
  mutate(DatetimeRounded = round_date(DatetimeUTC, "15 mins"))

# Task 4: Measuring distance at concurrent locations

## 1. Split the wildschwein_filter object into one data.frame per animal

ws_rosa <- wildschwein_filtered %>%
  filter(TierName == "Rosa")

ws_sabi <- wildschwein_filtered %>%
  filter(TierName == "Sabi")

## 2. Join* these datasets by the new Datetime column created in the last task. The joined observations are temporally close.

ws_individuals_joined <- ws_rosa %>%
  inner_join(ws_sabi, by = "DatetimeRounded", suffix = c(".rosa", ".sabi"))
ws_individuals_joined

## 3. In the joined dataset, calculate Euclidean distances between concurrent observations and store the values in a new column

ws_individuals_joined <- ws_individuals_joined %>%
  rowwise() %>%
  mutate(
    distance = calc_dist(c(E.rosa, N.rosa), c(E.sabi, N.sabi))
  )
ws_individuals_joined

## 4. Use a reasonable threshold on distance to determine if the animals are also spatially close enough to constitute a meet (we use 100 meters). Store this Boolean information (TRUE/FALSE) in a new column

ws_individuals_joined <- ws_individuals_joined %>%
  mutate(
    meet = ifelse(distance < 100, TRUE, FALSE)
  )

# Task 5: Visualize data

ws_individuals_joined_meet <- ws_individuals_joined %>% filter(
  meet
)

ggplot() +
  geom_point(data = wildschwein_filtered, mapping = aes(x = E, y = N, color = TierName)) +
  geom_point(data = ws_individuals_joined_meet, mapping = aes(x = E.rosa, y = N.rosa, fill = "Rosa"), color = "black", shape = 1) +
  geom_point(data = ws_individuals_joined_meet, mapping = aes(x = E.sabi, y = N.sabi, fill = "Sabi"), color = "black", shape = 1) +
  guides(fill = "legend") +
  ylim(c(1204500, 1205500)) +
  xlim(c(2570000, 2571000))

# Task 6 (optional): Visualize data as timecube with plotly

library(plotly)

plot_ly(wildschwein_filtered, x = ~E, y = ~N, z = ~DatetimeUTC, mode = 'lines',
               opacity = 1, line = list(width = 6, color = ~TierName))
