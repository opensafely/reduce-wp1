
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

wp1_all <- read_csv(here::here("output","wp1","wp1_ts_all.csv")) %>%
  mutate(age_5yrs = cut(
    age,
    breaks = c(seq(0, 90, by = 5), Inf),
    labels = c(
      paste0(
        seq(0, 85, by = 5), "-",
        seq(4, 89, by = 5)
      ),
      "90+"
    ),
    right = FALSE
  )
  )

wp1_ts_all <- summarise_hf(wp1_all) %>%
  mutate(subgroup = "All")

wp1_ts_age <- summarise_hf(wp1_all, age_5yrs) %>%
  mutate(subgroup = "By age")

wp1_ts_sex <- summarise_hf(wp1_all, sex)%>%
  mutate(subgroup = "By sex")

wp1_ts_eth <- summarise_hf(wp1_all, ethnicity_cat)%>%
  mutate(subgroup = "By ethnicity")

wp1_ts_eth_stand <- summarise_hf(wp1_all, ethnicity_cat, age_5yrs, sex) %>%
  mutate(subgroup = "By ethnicity - age/sex std")

wp1_ts_imd <- summarise_hf(wp1_all, imd_quintile)%>%
  mutate(subgroup = "By IMD")

wp1_ts_imd_stand <- summarise_hf(wp1_all, imd_quintile, age_5yrs, sex)%>%
  mutate(subgroup = "By IMD - age/sex std")

wp1_ts_age_sex <- summarise_hf(wp1_all, age_5yrs, sex)%>%
  mutate(subgroup = "By age and sex")

wp1_ts_copd <- summarise_hf(wp1_all, copd) %>%
  mutate(subgroup = "By COPD")

wp1_ts_copd_stand <- summarise_hf(wp1_all, copd, age_5yrs, sex) %>%
  mutate(subgroup = "By COPD - age/sex std")


wp1_ts <- bind_rows(mget(ls(pattern = "^wp1_ts_")))


write.csv(wp1_ts, here::here("output","wp1","wp1_timeseries.csv"), row.names = FALSE)

