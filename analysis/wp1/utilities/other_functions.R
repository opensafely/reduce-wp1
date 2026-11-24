# Functions

# rounding and redaction
rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}

# merge common variables to year-specific cohorts
merge.fn <- function(year) {
                        
  readr::read_csv(
    paste0("output/dataset_wp1_", year, ".csv.gz"),
    show_col_types = FALSE
    ) %>%
    inner_join(
      wp1_common,
      by = "patient_id",
      relationship = "one-to-one") %>%
    mutate(start_date = as.Date(paste0(year, "-02-01"))) %>%
    filter(
      (is.na(death_date) | (death_date >= start_date)) &
      (is.na(hf_exclude_date) | (hf_exclude_date >= start_date))
       )%>%
    mutate(t2dm = coalesce(first_t2dm_date < start_date, FALSE),
      copd = coalesce(first_copd_date < start_date, FALSE),
      af = coalesce(first_af_date < start_date, FALSE),
      ihd = coalesce(first_ihd_date < start_date, FALSE),
      ckd = coalesce(first_ckd_date < start_date, FALSE),
      hypertension = coalesce(first_hypertension_date < start_date, FALSE)) %>%
    select(
      -any_of(c(
        "prostate_cancer", "pregnancy", "hrtcocp",
        "first_t2dm_date","first_copd_date","first_af_date","first_ihd_date",
        "first_ckd_date","first_hypertension_date"
      ))
      )
}


# min / max/ quantile functions that appropriately deal with missing
safe_min <- function(x) {
  if (all(is.na(x))) return(x[NA_integer_][1])
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(x[NA_integer_][1])
  max(x, na.rm = TRUE)
}

safe_quantile <- function(x, p) {
  if (all(is.na(x))) return(NA_real_)
  unname(quantile(x, p, na.rm = TRUE))
}


# Helper function to summarise HF data by grouping variables
summarise_hf <- function(data, ...) {
  group_vars <- enquos(...)
  
  data %>%
    group_by(year, !!!group_vars) %>%
    summarise(
      hf_emerg   = rounding(sum(hf_diagnosis_emerg    == TRUE, na.rm = TRUE)),
      hf_primary = rounding(sum(hf_diagnosis_primary  == TRUE, na.rm = TRUE)),
      hf_any     = rounding(sum(hf_diagnosis_any      == TRUE, na.rm = TRUE)),
      hf_ae      = rounding(sum(hf_diagnosis_ec       == TRUE, na.rm = TRUE)),
      hf_death   = rounding(sum(hf_death              == TRUE, na.rm = TRUE)),
      pop        = rounding(n()),
      pdays  = rounding(sum(person_days)),
      .groups    = "drop"
    ) %>%
    select(year, !!!group_vars, hf_emerg, hf_primary, hf_any, pop, pdays) %>%
    mutate(
      hf_emerg_rate   = hf_emerg   / pop * 100000,
      hf_primary_rate = hf_primary / pop * 100000,
      hf_any_rate = hf_any / pop * 100000
    )
}

create.table <- function(df) {

  # Total population by diagnosis group
  totals <- df %>%
    group_by(hf_diag_short) %>%
    summarise(
      tot = rounding(n()),
      .groups = "drop"
    )

  # Person days by characteristic
  pdays <- df %>%
    pivot_longer(
      all_of(cat_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(variable, value) %>%
    summarise(
      pdays = rounding(sum(person_days, na.rm = TRUE)),
      .groups = "drop"
    )

  # Categorical breakdowns
  table_cat <- df %>%
    pivot_longer(
      all_of(cat_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    group_by(hf_diag_short, variable, value) %>%
    summarise(
      n = rounding(n()),
      .groups = "drop"
    ) %>%
    left_join(
      totals,
      by = "hf_diag_short"
    ) %>%
    pivot_wider(
      names_from = hf_diag_short,
      values_from = c(n, tot),
      names_glue = "{hf_diag_short}_{.value}"
    ) %>%
    left_join(
      pdays,
      by = c("variable", "value")
    )

  return(table_cat)
}


create.cont <- function(df){

  table_cont <- df %>%
    select(hf_diag_short, all_of(cont_vars)) %>%
    group_by(hf_diag_short) %>%
    summarise(
      across(
        all_of(cont_vars),
        list(
          p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
          p50 = ~ quantile(.x, 0.50, na.rm = TRUE),
          p75 = ~ quantile(.x, 0.75, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -hf_diag_short,
      names_to = c("variable", "stat"),
      names_sep = "__",
      values_to = "value"
    ) %>%
    pivot_wider(
      id_cols = variable,
      names_from = c(hf_diag_short, stat),
      values_from = value,
      names_glue = "{hf_diag_short}_{stat}"
    ) %>%
    arrange(variable)

  return(table_cont)
}

