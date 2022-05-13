# Load libraries & Import Data

library(tidyverse)

# Task 1: Write your own functions

## Create a function for our Euclidean distance calculation.


calc_dist <- function(point1, point2) {
  sqrt((point1[1]-point2[1])^2+(point1[2]-point2[2])^2)
}

test_calc_dist <- calc_dist(c(10, 15), c(10, 10))
test_calc_dist