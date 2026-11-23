
# Import libraries #
library('tidyverse')
library('lubridate')
library('arrow')
library('here')
library('reshape2')
library('dplyr')
library('fs')


dir_create(here::here("output", "wp1"), recurse = TRUE)
source(here::here("analysis", "wp1", "utilities", "other_functions.R"))
source(here::here("analysis", "wp1", "utilities", "cleaning.R"))
source(here::here("analysis", "wp1", "utilities", "checks.R"))

####

wp1_common <- read_csv(here::here("output","dataset_wp1_common.csv.gz"))

# Data file for time series analysis

wp1_2024 <-  merge.fn(2024) %>%
  clean.fn(as.Date("2024-02-01"), 1) %>%
  mutate(year =  2024) 

write.csv(wp1_2024, here::here("output", "wp1", "wp1_ts_2024.csv.gz"), row.names = FALSE)

# run checks of data
run_wp1_checks(here::here("output", "wp1","wp1_ts_2024.csv.gz"), "ts", 2024)
