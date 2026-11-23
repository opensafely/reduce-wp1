# data cleaning
clean.fn <- function(df, start_date, num_years){

  df %>%
    mutate(
      followup_end_date = 
        pmin(death_date, 
            reg_end_date, 
            start_date + years(num_years),
            as.Date("2025-12-31"), 
            na.rm = TRUE),
      start_date = as.Date(start_date),
      across(ends_with("_date"), as.Date),
      across(all_of(c("t2dm","copd","af","ihd","ckd","hypertension","carehome_at_index",
                      "migrant","non_english_speaking","learndis","smi",
                      "substance_abuse_1yr","homeless_1yr","housebound_1yr",
                      "obesity")), ~ replace_na(.x, FALSE))
    ) %>%
    filter(followup_end_date >= start_date) %>%
    mutate( 
           age_group = case_when(
             45<=age & age<55 ~ "45-54 years",
             55<=age & age<65 ~ "55-64 years",
             65<=age & age<75 ~ "65-74 years",
             75<=age & age<85 ~ "75-84 years",
             85<=age ~ ">=85 years"
           ),
           
           age_5yrs = cut(
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
           ),
           
           ethnicity_primary = as.character(ethnicity_primary),
           
           imd_quintile = case_when(
             imd_decile %in% c("1 (most deprived)", "2") ~ "1 (most deprived)",
             imd_decile %in% c("3","4") ~ "2",
             imd_decile %in% c("5","6") ~ "3",
             imd_decile %in% c("7","8") ~ "4",
             imd_decile %in% c("9","10 (least deprived)") ~ "5 (least deprived)",
             .default = NA_character_
           ),

           person_days = as.numeric(as.Date(followup_end_date) - as.Date(start_date)),

           bmi = as.numeric(ifelse(bmi_value < 60, bmi_value, NA)),
           bmi_missing = ifelse(is.na(bmi), "Yes", "No"),
           bmi_cat =
             case_when(
               !is.na(bmi) & bmi >= 30 & bmi < 60 ~ "Obese",
               !is.na(bmi) & bmi >= 25 & bmi < 30 ~ "Overweight",
               !is.na(bmi) & bmi >= 20 & bmi < 25 ~ "Normal",
               !is.na(bmi) & bmi >= 10 & bmi < 20 ~ "Underweight",
               .default = "Missing"
             ),
           obese = coalesce((bmi_cat == "Obese" | obesity == TRUE), FALSE),
           rural = coalesce(rural_urban >=5 & !is.na(rural_urban), FALSE),

           # outcomes
           died_during_study = coalesce(
             (death_date > start_date) &
               (death_date <= start_date + years(num_years)),
               FALSE
           ),

           hf_diagnosis_primary = coalesce(
             (hf_diagnosis_primary_date >= start_date) & 
             (hf_diagnosis_primary_date <= pmin(followup_end_date, start_date + years(num_years), na.rm=TRUE)),
             FALSE
           ),

           hf_diagnosis_apc = coalesce(
            (hf_diagnosis_apc_date >= start_date) & 
             (hf_diagnosis_apc_date <= pmin(followup_end_date, start_date + years(num_years), na.rm=TRUE)),
             FALSE
           ),

           hf_diagnosis_ec = coalesce(
             (hf_diagnosis_ec_date >= start_date) & 
             (hf_diagnosis_ec_date <= pmin(followup_end_date, start_date + years(num_years), na.rm=TRUE)),
             FALSE
           ),

           hf_death = coalesce(
            (hf_death_date >= start_date) & 
             (hf_death_date <= pmin(followup_end_date, start_date + years(num_years), na.rm=TRUE)),
             FALSE
           ),

           hf_diagnosis_emerg = coalesce(
            (hf_diagnosis_emerg_date >= start_date) & 
             (hf_diagnosis_emerg_date <= pmin(followup_end_date, start_date + years(num_years), na.rm=TRUE)),
             FALSE
           ),

           hf_diagnosis_any = coalesce(
            (hf_diagnosis_emerg | hf_diagnosis_primary),
            FALSE
           ),

           hf_diagnosis_type = case_when(
                (
                  (!is.na(hf_diagnosis_emerg_date) & !is.na(hf_diagnosis_primary_date) &
                  (hf_diagnosis_emerg_date < hf_diagnosis_primary_date)) |
                  (!is.na(hf_diagnosis_emerg_date) & is.na(hf_diagnosis_primary_date)) |
                  (hf_diagnosis_emerg_date == hf_diagnosis_primary_date)
                 ) ~ "Emergency diagnosis",
                ((!is.na(hf_diagnosis_emerg_date) & !is.na(hf_diagnosis_primary_date) &
                    (hf_diagnosis_primary_date < hf_diagnosis_emerg_date)) |
                   is.na(hf_diagnosis_emerg_date) & !is.na(hf_diagnosis_primary_date)) ~ "Primary care diagnosis",
                is.na(hf_diagnosis_emerg_date) & is.na(hf_diagnosis_primary_date) ~ "No diagnosis"
           ),

          patient_end_date = pmin(death_date, 
                                   reg_end_date, 
                                   hf_diagnosis_primary_date,
                                   hf_diagnosis_emerg_date,
                                   start_date + years(num_years),
                                   as.Date("2025-12-31"), 
                                   na.rm= TRUE)
    ) 
}
