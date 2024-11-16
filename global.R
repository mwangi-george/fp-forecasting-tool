pacman::p_load(
    tidyverse, bslib, shiny, shinyjs, janitor, apexcharter, shinyWidgets, glue, plotly, timetk, shinycssloaders,
    shinyalert, bsicons, DT, prophet, fs, httr, memoise, dhis2r, tibble
)


historical_fp_data <- read_rds("data/comparison_data.rds")
forecast_results <- read_rds("data/final_forecasts.rds")

distinct_analytics <- historical_fp_data %>% arrange(analytic_name) %>% distinct(analytic_name) %>% pull(analytic_name)
distinct_organisations <- historical_fp_data %>% arrange(org_unit) %>%  distinct(org_unit) %>% pull(org_unit)

forecasting_approaches <- historical_fp_data %>% distinct(method) %>% pull(method)


fp_consumption_747A_ids <- read_rds("data/fp_consumption_data_element_ids.rds")
fp_service_711_ids <- read_rds("data/fp_service_data_element_ids.rds")
counties_and_country_ids <- read_rds("data/level_1_2_ids.rds")
