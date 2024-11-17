pacman::p_load(
  tidyr, dplyr, purrr, stringr, bslib, shiny, shinyjs, janitor, apexcharter, shinyWidgets, glue, plotly, timetk, shinycssloaders,
  shinyalert, bsicons, DT, prophet, fs, httr, memoise, dhis2r, tibble, lubridate, thematic
)

# Import required datasets

historical_fp_data <- readRDS("data/comparison_data.rds")
forecast_results <- readRDS("data/final_forecasts_drive.rds")

fp_consumption_747A_ids <- readRDS("data/fp_consumption_data_element_ids.rds")
fp_service_711_ids <- readRDS("data/fp_service_data_element_ids.rds")
counties_and_country_ids <- readRDS("data/level_1_2_ids.rds")

distinct_analytics <- historical_fp_data |>
  arrange(analytic_name) |>
  distinct(analytic_name) |>
  pull(analytic_name)

distinct_organisations <- historical_fp_data |>
  arrange(org_unit) |>
  distinct(org_unit) |>
  pull(org_unit)

forecasting_approaches <- historical_fp_data |>
  distinct(method) |>
  pull(method)

forecast_results_end_date <- forecast_results |>
  summarize(latest_date = max(.index) |> as.Date()) %>%
  pull(latest_date)



