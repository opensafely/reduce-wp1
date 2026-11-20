import config

from ehrql.tables.tpp import (
    patients,
    practice_registrations,
    ons_deaths
    )

from ehrql import (
    create_dataset,
    years,
    get_parameter, 
    minimum_of
    )

from functions.core import(
    demog,
    location,
    quality_assurance,
    hf_exclude,
    hf_diagnosis,
    time_dependent,
    underserved,
    comorbidities
    )

dataset = create_dataset()

# dataset.configure_dummy_data(
#     population_size=10000,
#     timeout=500
#     )


cohort_index_date = get_parameter("cohort_index_date", type = str, default = "2019-02-01")

end_date = "2025-12-31"
start_date = cohort_index_date
earliest_date = "2000-01-01"

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

#hf diagnosis
dataset = location.fn(dataset, cohort_index_date)
dataset = time_dependent.fn(dataset, cohort_index_date)
dataset = underserved.fn(dataset, earliest_date, cohort_index_date, end_date)

#demographic variables derived based on start_date
dataset = demog.fn(dataset, start_date, end_date)

#quality assurance
dataset = quality_assurance.fn(dataset, earliest_date, end_date)

#hf diagnosis
dataset = hf_diagnosis.fn(dataset, start_date, end_date)

#hf exclude 
dataset = hf_exclude.fn(dataset, earliest_date, end_date)

dataset = comorbidities.fn(dataset, earliest_date, end_date)

dataset.define_population(
    registrations.exists_for_patient()
    & (patients.sex.is_in(['male','female'])) #known sex proxy for data quality
    & (patients.date_of_birth.is_not_null()) #known dob proxy for data quality
    & (patients.age_on(cohort_index_date) >= 45) #remove pts < 45
    & (patients.age_on(cohort_index_date) < 110) #remove pts age 110+
    #& ~((dataset.sex == 'male') & (dataset.hrtcocp.is_not_null())) #remove males with hrt / cocp codes
    #& ~((dataset.sex == 'male') & (dataset.pregnancy.is_not_null())) #remove males with pregnancy codes
    #& ~((dataset.sex == 'female') & (dataset.prostate_cancer.is_not_null())) #remove females with prostate cancer codes
    & ((cohort_index_date < dataset.death_date) | (dataset.death_date.is_null())) #remove pts who died before start
    & ((cohort_index_date < dataset.hf_exclude_date) | (dataset.hf_exclude_date.is_null())) #remove pts with history of HF
)
