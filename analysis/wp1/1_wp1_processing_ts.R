
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

wp1_2019 <-  merge.fn(2019) %>%
  clean.fn(as.Date("2019-02-01"), 1) %>%
  mutate(year =  2019) 

write.csv(wp1_2019, here::here("output", "wp1", "wp1_ts_2019.csv.gz"), row.names = FALSE)


wp1_2020 <-  merge.fn(2020) %>%
  clean.fn(as.Date("2020-02-01"), 1) %>%
  mutate(year =  2020)

write.csv(wp1_2020, here::here("output", "wp1", "wp1_ts_2020.csv.gz"), row.names = FALSE)


wp1_2021 <- merge.fn(2021) %>%
  clean.fn(as.Date("2021-02-01"), 1) %>%
  mutate(year =  2021)

write.csv(wp1_2021, here::here("output", "wp1", "wp1_ts_2021.csv.gz"), row.names = FALSE)


wp1_2022 <- merge.fn(2022) %>%
  clean.fn(as.Date("2022-02-01"), 1) %>%
  mutate(year =  2022)

write.csv(wp1_2022, here::here("output", "wp1", "wp1_ts_2022.csv.gz"), row.names = FALSE)


wp1_2023 <- merge.fn(2023) %>%
  clean.fn(as.Date("2023-02-01"), 1) %>%
  mutate(year =  2023)

write.csv(wp1_2023, here::here("output", "wp1", "wp1_ts_2023.csv.gz"), row.names = FALSE)


wp1_2024 <- merge.fn(2024) %>%
  clean.fn(as.Date("2024-02-01"), 1) %>%
  mutate(year =  2024)

write.csv(wp1_2024, here::here("output", "wp1", "wp1_ts_2024.csv.gz"), row.names = FALSE)


wp1_2025 <- merge.fn(2025) %>%
  clean.fn(as.Date("2025-02-01"), 1) %>%
  mutate(year =  2025)

write.csv(wp1_2025, here::here("output", "wp1", "wp1_ts_2025.csv.gz"), row.names = FALSE)


# run checks of data
run_wp1_checks(here::here("output", "wp1","wp1_ts_2019.csv.gz"), "ts", 2019)
run_wp1_checks(here::here("output", "wp1","wp1_ts_2020.csv.gz"), "ts", 2020)
run_wp1_checks(here::here("output", "wp1","wp1_ts_2021.csv.gz"), "ts", 2021)
run_wp1_checks(here::here("output", "wp1","wp1_ts_2022.csv.gz"), "ts", 2022)
run_wp1_checks(here::here("output", "wp1","wp1_ts_2023.csv.gz"), "ts", 2023)
run_wp1_checks(here::here("output", "wp1","wp1_ts_2024.csv.gz"), "ts", 2024)
run_wp1_checks(here::here("output", "wp1","wp1_ts_2025.csv.gz"), "ts", 2025)

