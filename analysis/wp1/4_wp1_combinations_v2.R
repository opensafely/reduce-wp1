library(ComplexUpset)
library(ggplot2)

library(tidyverse)


factors <- c("imd_lowest", 
             "rural", 
             "non_english_speaking", 
             "female",
             "hypertension", "af", "ihd", "ckd", "t2dm", "copd",
             "age_older", 
             "eth_nonwhite", 
             "bmi_obese",
             "smok_ever")

for_upset <- wp1_main %>%
  
    # some of these columns won't be used - keeping them in case we want to add later
    mutate(age_1 = (age_group == "45-54 years"),
         age_2 = (age_group == "55-64 years"),
         age_3 = (age_group == "65-74 years"),
         age_4 = (age_group == "75-84 years"),
         age_5 = (age_group == ">=85 years"),
         
         age_older = (age>= 65),
         age_younger = (age < 65),
         
         eth_white = (ethnicity_cat == "White"),
         eth_nonwhite = (ethnicity_cat %in% c("Black","Asian","Mixed","Other")),
         eth_black = (ethnicity_cat == "Black"),
         eth_asian = (ethnicity_cat == "Asian"),
         eth_mixed = (ethnicity_cat == "Mixed"),
         eth_other = (ethnicity_cat == "Other"),
         eth_missing = (ethnicity_cat == "Unknown"),
         
         bmi_obese = (bmi_cat == "Obese"),
         bmi_overweight = (bmi_cat == "Overweight"),
         bmi_normal = (bmi_cat == "Normal"),
         bmi_underweight = (bmi_cat == "Underweight"),
         bmi_missing = (bmi_cat == "Missing"),
         
         bmi_nonobese = (!(bmi_cat %in% c("Obese","Overweight","Normal","Underweight","Missing"))),
         
         # region_london = (region == "London"),
         # region_neast = (region == "North East"),
         # region_nwest = (region == "North West"),
         # region_yorkshire = (region == "Yorkshire and the Humber"),
         # region_seast = (region == "South East"),
         # region_swest = (region == "South West"),
         # region_wmidlands = (region == "West Midlands"),
         # region_east = (region == "East"),
         # region_emidlands = (region == "East Midlands"),
          
         male = (sex == "male"), 
         female = (sex == "female"),
         
         smok_never = (smoking == "N"),
         smok_ever = (smoking == "E"),
         smok_curr = (smoking == "C"),
         
         imd_lowest = coalesce((imd_quintile == "1 (most deprivation)"), FALSE)
  ) %>%
  select(all_of(factors))

# set NA to FALSE
for_upset[is.na(for_upset)] <- FALSE

# convert all to logical T/F
for_upset[factors] = for_upset[factors] == TRUE

upset(for_upset, factors, name = 'combination', width_ratio=.1, keep_empty_groups=TRUE)

upset_data <- upset %>%
  count(across(all_of(factors))) %>%
  mutate(prevalence = n / sum(n)) %>%
  arrange(desc(prevalence))
  