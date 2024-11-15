pacman::p_load(
    tidyverse, bslib, shiny, shinyjs, janitor, apexcharter, shinyWidgets, glue, plotly, timetk, shinycssloaders,
    shinyalert, bsicons, DT, prophet
)


historical_fp_data <- read_rds("data/comparison_data.rds")
forecast_results <- read_rds("data/final_forecasts.rds")

distinct_analytics <- historical_fp_data %>% arrange(analytic_name) %>% distinct(analytic_name) %>% pull(analytic_name)
distinct_organisations <- historical_fp_data %>% arrange(org_unit) %>%  distinct(org_unit) %>% pull(org_unit)

forecasting_approaches <- historical_fp_data %>% distinct(method) %>% pull(method)
