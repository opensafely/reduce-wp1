
# Import libraries #
library('tidyverse')
library('lubridate')
library('arrow')
library('here')
library('reshape2')
library('dplyr')
library('fs')
library('gtsummary')
library('gt')


## Custom functions
source(here::here("analysis", "wp1", "utilities", "other_functions.R"))

# Create directory
dir_create(here::here("output", "wp1"), recurse = TRUE)
dir_create(here::here("output", "wp1", "tables"), recurse = TRUE)

wp1_main <- read_csv(here::here("output","wp1","wp1_main.csv"), show_col_types = FALSE) %>%

  # Convert categorical columns to factors
  mutate(
    mltc = (rowSums(across(c(ckd, t2dm, obese, copd, hypertension, ihd))) >= 2),
    
    across(c(migrant, non_english_speaking, smi, substance_abuse_1yr, homeless_1yr,
                  learndis, housebound_1yr, copd, died_during_study,
                  hypertension, obese,
                  af, ihd, ckd, t2dm, hf_diagnosis_primary,
                  hf_diagnosis_apc, hf_diagnosis_ec, hf_death, 
                  hf_diagnosis_emerg, hf_diagnosis_any, mltc), ~ replace_na(.x, FALSE)),
                
    across(c(imd_quintile, rural, smoking, died_during_study,
                  migrant, non_english_speaking, smi, substance_abuse_1yr, homeless_1yr,
                  learndis, housebound_1yr, sex, hf_diagnosis_primary, hf_diagnosis_emerg,
                  hf_diagnosis_any, ethnicity_cat, copd, hf_diagnosis_type, bmi_cat,
                  hypertension, region, obese, imd_quintile,
                  af, ihd, ckd, t2dm, age_group, bmi_missing, mltc), as.factor),
    
    hf_diag_short = case_when(
      hf_diagnosis_type == "Emergency diagnosis" ~ "emerg",
      hf_diagnosis_type == "Primary care diagnosis" ~ "prim",
      hf_diagnosis_type == "No diagnosis" ~ "none",
      is.na(hf_diagnosis_type) ~ "none")
    
    )  

cat_vars <- c("sex", "imd_quintile", "rural", "smoking",
              #"region", "hypertension",
              "migrant", "non_english_speaking", "smi", "bmi_cat",
              "substance_abuse_1yr", "homeless_1yr", "ethnicity_cat",
              "copd", "af", "ihd", "ckd", "t2dm",
              "age_group", "learndis", "died_during_study",
              "bmi_missing")

cont_vars <- c("age","bmi")


main_table <- create.table(wp1_main)
write.csv(main_table, here::here("output", "wp1", "tables", "wp1_main_cat.csv"), row.names = FALSE)

main_cont <- create.cont(wp1_main) 
write.csv(main_cont, here::here("output", "wp1", "tables", "wp1_main_cont.csv"), row.names = FALSE)

# 
# copd <- wp1_main %>% subset(copd == TRUE)
# 
# create.table(copd) %>%
#   write.csv(here::here("output", "wp1", "tables", "wp1_copd_cat.csv"), row.names = FALSE)
# create.cont(copd) %>%
#   write.csv(here::here("output", "wp1", "tables", "wp1_copd_cont.csv"), row.names = FALSE)
# 
# imd_lowest <- wp1_main %>% subset(imd_quintile == "1 (most deprived)")
# 
# create.table(imd_lowest) %>%
#   write.csv(here::here("output", "wp1", "tables", "wp1_imd_cat.csv"), row.names = FALSE)
# create.cont(imd_lowest) %>%
#   write.csv(here::here("output", "wp1", "tables", "wp1_imd_cont.csv"), row.names = FALSE)
# 
# mltc <- wp1_main %>% subset(mltc == TRUE)
# 
# create.table(mltc) %>%
#   write.csv(here::here("output", "wp1", "tables", "wp1_mltc_cat.csv"), row.names = FALSE)
# create.cont(mltc) %>%
#   write.csv(here::here("output", "wp1", "tables", "wp1_mltc_cont.csv"), row.names = FALSE)
# 
