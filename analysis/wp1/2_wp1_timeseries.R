
# Import libraries #
library('tidyverse')
library('lubridate')
library('arrow')
library('here')
library('reshape2')
library('dplyr')
library('fs')
library('ggpubr')

dir_create(here::here("output", "wp1"), recurse = TRUE)
dir_create(here::here("output", "wp1", "figures"), recurse = TRUE)
source(here::here("analysis", "wp1", "utilities", "other_functions.R"))

years <- 2021:2025


wp1_ts_list <- list()

for (yr in years) {
  
  # Get the dataset for the current year
  df <- read_csv(here::here("output","wp1",paste0("wp1_ts_", yr, ".csv.gz")),
                 show_col_types = FALSE)
  
  # Generate summaries
  ts_all <- summarise_hf(df) %>%
    mutate(subgroup = "All")
  
  ts_age <- summarise_hf(df, age_5yrs) %>%
    mutate(subgroup = "By age")
  
  ts_sex <- summarise_hf(df, sex) %>%
    mutate(subgroup = "By sex")
  
  ts_eth <- summarise_hf(df, ethnicity_cat) %>%
    mutate(subgroup = "By ethnicity")
  
  ts_eth_stand <- summarise_hf(df, ethnicity_cat, age_5yrs, sex) %>%
    mutate(subgroup = "By ethnicity - age/sex std")
  
  ts_imd <- summarise_hf(df, imd_quintile) %>%
    mutate(subgroup = "By IMD")
  
  ts_imd_stand <- summarise_hf(df, imd_quintile, age_5yrs, sex) %>%
    mutate(subgroup = "By IMD - age/sex std")
  
  ts_age_sex <- summarise_hf(df, age_5yrs, sex) %>%
    mutate(subgroup = "By age and sex")
  
  ts_copd <- summarise_hf(df, copd) %>%
    mutate(subgroup = "By COPD")
  
  ts_copd_stand <- summarise_hf(df, copd, age_5yrs, sex) %>%
    mutate(subgroup = "By COPD - age/sex std")
  
  # Combine summaries for this year
  wp1_ts_list[[as.character(yr)]] <- bind_rows(
    ts_all,
    ts_age,
    ts_sex,
    ts_eth,
    ts_eth_stand,
    ts_imd,
    ts_imd_stand,
    ts_age_sex,
    ts_copd,
    ts_copd_stand
  ) %>%
    mutate(year = yr)
  }

# Combine all five years
wp1_timeseries <- bind_rows(wp1_ts_list) 

write.csv(wp1_timeseries, here::here("output", "wp1", "wp1_timeseries.csv"), row.names = FALSE)


# Example plot
a <- ggplot(filter(wp1_timeseries, subgroup == "By age and sex")) +
  geom_line(aes(x = year, y = hf_emerg_rate, colour = sex)) +
  facet_wrap(~age_5yrs, scale = "free_y") +
  scale_colour_viridis_d() +
  #scale_y_continuous(limits = c(0, 35000)) +
  xlab("Year") + ylab("Rate per 100,000") +
  labs(title="Emergency diagnosis") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text= element_text(hjust = 0),
        legend.title = element_blank())


b <- ggplot(filter(wp1_timeseries, subgroup == "By age and sex")) +
  geom_line(aes(x = year, y = hf_primary_rate, colour = sex)) +
  facet_wrap(~age_5yrs, scale = "free_y") +
  scale_colour_viridis_d() +
  #scale_y_continuous(limits = c(0, 35000)) +
  xlab("Year") + ylab("Rate per 100,000") +
  labs(title="Primary care diagnosis") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text= element_text(hjust = 0),
        legend.title = element_blank())

combined <- ggarrange(a , b, nrow = 2)

# Save plot
ggsave(
  here::here("output", "wp1","figures","plots_ts_age_sex.png"),
  combined,
  width = 12,
  height = 10,
  dpi = 300
)