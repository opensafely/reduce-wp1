# data cleaning
clean.fn <- function(df, start_date, num_years){

  df %>%
    mutate(
      followup_end_date = 
        pmin(death_date, 
            practice_deregistration_date, 
            as.Date("2025-12-31"), na.rm= TRUE)
           ) %>%
    subset(patient_index_date <= start_date & followup_end_date >= start_date) %>%
    mutate( 
           age =  as.period(interval(birth_date, start_date))$year,

           age_group = case_when(
             45<=age & age<55 ~ "45-54 years",
             55<=age & age<65 ~ "55-64 years",
             65<=age & age<75 ~ "65-74 years",
             75<=age & age<85 ~ "75-84 years",
             85<=age ~ ">=85 years"
           ),

      # create flags for whether people had condition on analysis start date
           copd = coalesce(first_copd_date < start_date, FALSE),

           hypertension = coalesce(
             (hypertension_date_primary < start_date) |
               (hypertension_date_sus < start_date) |
               ((hypertension_date_med < start_date) & 
                  (hypertension_date_med >= start_date - years(1))), 
               FALSE
              ),

           af = coalesce(
             (af_date_primary < start_date) |
               (af_date_sus < start_date),
               FALSE
           ),

           ihd = coalesce(
             (ihd_date_primary < start_date) |
               (ihd_date_sus < start_date),
                FALSE
           ),

           ckd = coalesce(
             (ckd_date_primary < start_date) |
               (ckd_date_sus < start_date),
               FALSE
           ),

           t2dm = coalesce(
             (t2dm_date < start_date) &
               (cat_diabetes == "T2DM"),
               FALSE
           ),

           learndisability = coalesce(
             (learndis < start_date),
             FALSE
           ),

           bmi = as.numeric(
             ifelse((bmi_date < start_date) &
                    (bmi_date >= (start_date - years(2))), 
                    bmi_value, NA) 
           ),
           bmi_missing = ifelse(is.na(bmi), "Yes", "No"),
           bmi_cat =
             case_when(
               !is.na(bmi) & bmi >= 30 & bmi < 60 ~ "Obese",
               !is.na(bmi) & bmi >= 25 & bmi < 30 ~ "Overweight",
               !is.na(bmi) & bmi >= 20 & bmi < 25 ~ "Normal",
               !is.na(bmi) & bmi >= 10 & bmi < 20 ~ "Underweight",
               .default = "Missing"
             ),
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

           hf_diagnosis_any_date = if_else(
              hf_diagnosis_any == FALSE,
              as.Date(NA),
              pmin(
                hf_diagnosis_primary_date,
                hf_diagnosis_emerg_date,
                na.rm = TRUE
              )
            ),

           hf_diagnosis_type = case_when(
            is.na(hf_diagnosis_any_date) ~ "No diagnosis",
            hf_diagnosis_emerg_date == hf_diagnosis_any_date ~ "Emergency diagnosis",
            hf_diagnosis_primary_date == hf_diagnosis_any_date ~ "Primary care diagnosis"
           ),

          patient_end_date = pmin(death_date, 
                                   practice_deregistration_date, 
                                   hf_diagnosis_primary_date,
                                   hf_diagnosis_emerg_date,
                                   start_date + years(num_years),
                                   as.Date("2025-12-31"), 
                                   na.rm= TRUE)
    ) %>%
    select(!c(hf_exclude_date, first_copd_date, hypertension_date_primary,
              hypertension_date_med, hypertension_date_sus, af_date_primary,
              af_date_sus, ihd_date_primary, ihd_date_sus, ckd_date_primary,
              ckd_date_sus, t2dm_date, cat_diabetes, bmi_date, obesity_primary_date,
              obesity_sus_date, learndis, bmi_value)) 
  
}
