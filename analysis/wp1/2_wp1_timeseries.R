
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

wp1_all <- read_csv(here::here("output","wp1","wp1_ts_all.csv"))


wp1_ts_all <- summarise_hf(wp1_all) %>%
  mutate(age_group = "All", sex = "All", ethnicity_cat = "All")

wp1_ts_age <- summarise_hf(wp1_all, age_group)%>%
  mutate(sex = "All", ethnicity_cat = "All")

wp1_ts_sex <- summarise_hf(wp1_all, sex)%>%
  mutate(age_group = "All", ethnicity_cat = "All")

wp1_ts_eth <- summarise_hf(wp1_all, ethnicity_cat)%>%
  mutate(age_group = "All", sex = "All")

wp1_ts_age_sex <- summarise_hf(wp1_all, age_group, sex)%>%
  mutate(ethnicity_cat = "All")

wp1_ts_age_sex_eth <- summarise_hf(wp1_all, age_group, sex, ethnicity_cat)

wp1_ts_eth_age <- summarise_hf(wp1_all, age_group, ethnicity_cat)%>%
  mutate(sex = "All")

wp1_ts <- bind_rows(wp1_ts_all, wp1_ts_age, wp1_ts_sex, wp1_ts_eth, wp1_ts_age_sex, wp1_ts_age_sex_eth,
                wp1_ts_eth_age)


write.csv(wp1_ts, here::here("output","wp1","wp1_timeseries.csv"), row.names = FALSE)

