
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

wp1_common <- read_csv(here::here("output","dataset_wp1_common.csv.gz"))

wp1_main <- merge.fn(2023) %>%
  clean.fn(as.Date("2023-02-01"), 3) %>%
  mutate(year =  2023)

write.csv(wp1_main, here::here("output", "wp1", "wp1_main.csv"), row.names = FALSE)

run_wp1_checks(here::here("output", "wp1","wp1_main.csv"), "main", 2023)

####
