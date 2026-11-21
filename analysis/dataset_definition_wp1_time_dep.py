import datetime
from ehrql.tables.tpp import (patients, practice_registrations, addresses)
from ehrql import (create_dataset, years, get_parameter)
from functions.core import(time_dependent, underserved)
from ehrql import table_from_file

dataset = create_dataset()

# dataset.configure_dummy_data(
#     population_size=10000,
#     timeout=500
#     )

cohort_index_date = get_parameter("cohort_index_date", type = str, default = "2019-02-01")
start_date = cohort_index_date
end_date = "2025-12-31"
earliest_date = "2000-01-01"

FULL_COHORT = "output/dataset_wp1_common.csv.gz"

full_cohort = table_from_file(
    FULL_COHORT,
    columns={
        "death_date": datetime.date,
        "hf_exclude_date": datetime.date,
        "sex": str,
        "dob": datetime.date,
        "ethnicity_cat": str,
        "ethnicity_primary": str,
        "patient_index_date": datetime.date,
        "hf_diagnosis_primary_date": datetime.date,
        "hf_diagnosis_apc_date": datetime.date,
        "hf_diagnosis_ec_date": datetime.date,
        "hf_death_date": datetime.date,
        "hf_diagnosis_emerg_date": datetime.date, 
        "hf_mi_diagnosis_apc_date": datetime.date,
        "hf_diagnosis_date": datetime.date,
        "first_t2dm_date": datetime.date,
        "first_copd_date": datetime.date,
        "first_hypertension_date": datetime.date,
        "first_af_date": datetime.date,
        "first_ihd_date": datetime.date,
        "first_ckd_date": datetime.date
    },
)

# add columns to year-specific cohorts
dataset.death_date = full_cohort.death_date
dataset.hf_exclude_date = full_cohort.hf_exclude_date
dataset.sex = full_cohort.sex
dataset.dob = full_cohort.dob
dataset.ethnicity_cat = full_cohort.ethnicity_cat
dataset.ethnicity_primary = full_cohort.ethnicity_primary
dataset.patient_index_date = full_cohort.patient_index_date

dataset.hf_diagnosis_primary_date = full_cohort.hf_diagnosis_primary_date
dataset.hf_diagnosis_apc_date = full_cohort.hf_diagnosis_apc_date
dataset.hf_diagnosis_ec_date = full_cohort.hf_diagnosis_ec_date
dataset.hf_death_date = full_cohort.hf_death_date
dataset.hf_diagnosis_emerg_date = full_cohort.hf_diagnosis_emerg_date
dataset.hf_mi_diagnosis_apc_date = full_cohort.hf_mi_diagnosis_apc_date

dataset.t2dm = (full_cohort.first_t2dm_date.is_before(cohort_index_date))
dataset.copd = (full_cohort.first_copd_date.is_before(cohort_index_date))
dataset.af = (full_cohort.first_af_date.is_before(cohort_index_date))
dataset.ihd = (full_cohort.first_ihd_date.is_before(cohort_index_date))
dataset.ckd = (full_cohort.first_ckd_date.is_before(cohort_index_date))

#hf diagnosis
dataset = time_dependent.fn(dataset, cohort_index_date)
dataset = underserved.fn(dataset, earliest_date, cohort_index_date)

#registered for at least one year prior to index date
registrations = (
    practice_registrations
    .spanning(
        cohort_index_date - years(1), 
        cohort_index_date)
    .sort_by(
        practice_registrations.start_date,
        practice_registrations.end_date,
        practice_registrations.practice_pseudo_id)
    .last_for_patient()
)
dataset.reg_start_date = registrations.start_date
dataset.reg_end_date = registrations.end_date
dataset.age = patients.age_on(cohort_index_date)

patient_address = addresses.for_patient_on(cohort_index_date)
dataset.imd_decile = patient_address.imd_decile
dataset.rural_urban = addresses.for_patient_on(cohort_index_date).rural_urban_classification

dataset.define_population(
    registrations.exists_for_patient()
    & full_cohort.exists_for_patient()
    & (patients.age_on(cohort_index_date) >= 45) #remove pts < 45
    & (patients.age_on(cohort_index_date) < 110) #remove pts age 110+
    & ((cohort_index_date < full_cohort.death_date) | (full_cohort.death_date.is_null())) #remove pts who died before start
    & ((cohort_index_date < full_cohort.hf_exclude_date) | (full_cohort.hf_exclude_date.is_null())) #remove pts with history of HF
)
