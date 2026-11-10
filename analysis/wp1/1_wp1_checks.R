################################################
# REDUCE-HF project
# Author: Andrea Schaffer
# Bennett Institute for Applied Data Science
# University of Oxford
################################################

# Import libraries #
library(tidyverse)
library(lubridate)
library(fs)
library(here)
library(skimr)

source(here::here("analysis", "wp1", "utilities", "other_functions.R"))
source(here::here("analysis", "wp1", "utilities", "checks.R"))

# Same checks on the main and yearly/time-series datasets.
run_wp1_checks(here::here("output", "wp1", "wp1_main.csv"), "main")
run_wp1_checks(here::here("output", "wp1", "wp1_ts_all.csv"), "ts")
