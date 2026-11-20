
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

####

# Data file for time series analysis

wp1_2019 <- read_csv(here::here("output", "dataset_wp1_2019.csv.gz")) %>%
  clean.fn(as.Date("2019-02-01"), 1) %>%
  mutate(year = 2019) 

wp1_2020 <- read_csv(here::here("output", "dataset_wp1_2020.csv.gz")) %>%
  clean.fn(as.Date("2020-02-01"), 1) %>%
  mutate(year = 2020)

wp1_2021 <- read_csv(here::here("output", "dataset_wp1_2021.csv.gz")) %>%
  clean.fn(as.Date("2021-02-01"), 1) %>%
  mutate(year = 2021)

wp1_2022 <- read_csv(here::here("output", "dataset_wp1_2022.csv.gz")) %>%
  clean.fn(as.Date("2022-02-01"), 1) %>%
  mutate(year = 2022)

wp1_2023 <- read_csv(here::here("output", "dataset_wp1_2023.csv.gz")) %>%
  clean.fn(as.Date("2023-02-01"), 1) %>%
  mutate(year = 2023)

wp1_2024 <- read_csv(here::here("output", "dataset_wp1_2024.csv.gz")) %>%
  clean.fn(as.Date("2024-02-01"), 1) %>%
  mutate(year = 2024)

wp1_2025 <- read_csv(here::here("output", "dataset_wp1_2025.csv.gz")) %>%
  clean.fn(as.Date("2025-02-01"), 1) %>%
  mutate(year = 2025)

wp1_ts_all <- rbind(wp1_2019, wp1_2020, wp1_2021, wp1_2022, wp1_2023, wp1_2024, wp1_2025) 

write.csv(wp1_ts_all, here::here("output", "wp1", "wp1_ts_all.csv"), row.names = FALSE)
