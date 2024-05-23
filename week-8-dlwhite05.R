library(tidyverse)
library(haven)
library(dplyr)

# Load data set
murders_95df <- read.csv("murders.csv") 

# Filter by year 1995
filtered_murders_1995 <- murders_95df %>%
  filter(year == 1995)
