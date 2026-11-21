from functions.lib import *
####################
# Underserved groups
####################

  
def fn(dataset, earliest_date, index_date):
    
    gp_events = filter_gp_events(earliest_date, index_date)

    #Care home status
    location = addresses.for_patient_on(index_date)
    
    #was address at patient index date a care home
    dataset.carehome_at_index = (
        location.care_home_is_potential_match |
        location.care_home_requires_nursing |
        location.care_home_does_not_require_nursing
        )
    
    # Migrant status
    dataset.migrant = last_matching_event_clinical_snomed(
            gp_events,
            migrant,
            where=True
            ).exists_for_patient()

    # Non english speaking
    dataset.non_english_speaking = last_matching_event_clinical_snomed(
            gp_events,
            non_english_speaking,
            where=True
            ).exists_for_patient()

    #learning disability
    dataset.learndis = last_matching_event_clinical_snomed(
            gp_events,
            learndis_primis
            ).exists_for_patient()
    
    #severe mental illness
    smi_code = last_matching_event_clinical_snomed(
            gp_events,
            sev_mental_primis
            ).date

    smi_code_remission = last_matching_event_clinical_snomed(
            gp_events,
            smhres_primis 
            ).date

    dataset.smi = (
        (smi_code.is_not_null() & smi_code_remission.is_before(smi_code)) |
        (smi_code.is_not_null() & smi_code_remission.is_null())
        )

    # Records from one year before index only
    gp_events_1yr = gp_events.where(
        gp_events.date.is_on_or_between(
            index_date - years(1), index_date
            )
        )

    # Substance abuse
    dataset.substance_abuse_1yr = last_matching_event_clinical_snomed(
        gp_events_1yr,
        substance_abuse, 
        where=True
        ).exists_for_patient()

    # Homeless
    dataset.homeless_1yr = last_matching_event_clinical_snomed(
        gp_events_1yr,
        homeless, 
        where=True
        ).exists_for_patient()

    # Housebound
    housebound_date = last_matching_event_clinical_snomed(
        gp_events_1yr,
        housebound, 
        where=True
        ).date

    not_housebound_date = last_matching_event_clinical_snomed(
        gp_events_1yr,
        no_longer_housebound, 
        where=True
        ).date

    dataset.housebound_1yr = (
        housebound_date.is_not_null() & 
        (housebound_date.is_after(
            not_housebound_date) | 
            not_housebound_date.is_null()
            )
        )

    return dataset
