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
      relationship = "one-to-one",
      suffix = c("", "_common")
    ) %>%
    select(
      -any_of(c(
        "prostate_cancer", "pregnancy", "hrtcocp",
        "tmp_copd_date_primary", "tmp_copd_date_sus",
        "otherdm_date", "gestationaldm_date",
        "t1dm_date", "practice_id", "practice_stp",
        "msoa", "sysbp_date", "sysbp_value",
        "diasbp_date", "diasbp_value",
        "last_hdl_cholesterol_date",
        "last_hdl_cholesterol_value",
        "last_cholesterol_date",
        "last_cholesterol_value",
        "weight", "weight_date", "height",
        "carehome_at_index", "death_date_common",
        "household_size"
      ))
      )
}


# min / max/ quantile funcations that appropriately deal with missing
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
      .groups    = "drop"
    ) %>%
    select(year, !!!group_vars, hf_emerg, hf_primary, hf_any, pop) %>%
    mutate(
      hf_emerg_rate   = hf_emerg   / pop * 100000,
      hf_primary_rate = hf_primary / pop * 100000,
      hf_any_rate = hf_any / pop * 100000
    )
}

create.table <- function(df){

  pdays <- df %>%
    pivot_longer(all_of(cat_vars), names_to = "variable", values_to = "value") %>%
    group_by(variable, value) %>%
    summarise(pdays = as.numeric(sum(person_days)), .groups = "drop")

  table_cat <- df %>%
    pivot_longer(all_of(cat_vars), names_to = "variable", values_to = "value") %>%
    group_by(hf_diag_short, variable, value) %>%
    summarise(n = rounding(n()), .groups = "drop") %>%
    group_by(hf_diag_short) %>%
    mutate(tot = rounding(sum(n))) %>%
    ungroup() %>%
    pivot_wider(
      names_from  = hf_diag_short,
      values_from = c(n, tot),
      names_glue  = "{hf_diag_short}_{.value}"
    ) %>%
    left_join(pdays, by = c("variable", "value"))

  return(table_cat)

}


create.cont <- function(df){

  table_cont <- df %>%
    select(hf_diag_short, cont_vars) %>%
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

