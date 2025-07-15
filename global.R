pacman::p_load(
  tidyr, dplyr, purrr, stringr, bslib, shiny, shinyjs, janitor, apexcharter, shinyWidgets, glue, plotly, timetk, shinycssloaders,
  shinyalert, bsicons, DT, prophet, fs, httr, memoise, tibble, lubridate, thematic, reactable, echarts4r, readxl, here, cli
)

# Import required datasets

historical_fp_data <- readRDS("data/pinned_df.rds") |>
  rename(analytic = analytic_name) |>
  # deselect unnecessary columns
  select(-contains(c("outlier", "year", "median_value")))

historical_fp_data |>
  saveRDS("data/historical_fp_data.rds")


forecast_results <- readRDS("data/final_forecasts_drive.rds") |>
  rename(analytic = analytic_name) |>
  mutate(period = .index)

fp_consumption_747A_ids <- readRDS("data/fp_consumption_data_element_ids.rds") |> sort()
fp_service_711_ids <- readRDS("data/fp_service_data_element_ids.rds") |> sort()
counties_and_country_ids <- readRDS("data/level_1_2_ids.rds")

distinct_analytics <- historical_fp_data |>
  arrange(analytic) |>
  distinct(analytic) |>
  pull(analytic)

distinct_organisations <- historical_fp_data |>
  arrange(org_unit) |>
  distinct(org_unit) |>
  pull(org_unit)

forecasting_approaches <- historical_fp_data |>
  distinct(method) |>
  pull(method)

forecast_results_max_date <- forecast_results |>
  summarize(latest_date = max(.index) |> as.Date()) %>%
  pull(latest_date)

forecast_results_end_date <- floor_date(today(), unit = "month") + 360


# Forecast vs Actual Data
forecast_actual_df <- readRDS("data/forecast_vs_actual_data.rds")


# TODO
# Create a tab documenting the platform's usage, navigation, value add etc
# Embed reason for the changed service data CYP
# Supply planning cycle

