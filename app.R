source("global.R")

# source R scripts in sub directories
dir_ls("utils/") |> map(~source(.x))
dir_ls("R/") |> map(~source(.x))

# Enable thematic
thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))


ui <- page_navbar(
  useShinyjs(), # initialize shinyjs
  title = "FP Forecasting Tool",
  theme = bs_theme(
    version = 5,
    bootswatch = "journal",
    base_font = font_google("Inter"),
    navbar_bg = "red"
  ),
  sidebar = sidebar(open = "closed"),
  nav_item(tags$a("Home", href = "/")),
  nav_spacer(),
  nav_panel("Trend Analytics", comparison_service_consumption_page_ui("service_consumption_comparison")),
  nav_panel("Anomaly Detection", anomaly_detection_and_handling_page_ui("anomaly_detection_and_handling")),
  nav_panel("Forecast Results", forecast_results_consumption_page_ui("forecasting_results_comparison")),
  nav_panel("Live Model", live_prophet_forecasting_model_page_ui("prophet_forecasting_model")),
  nav_panel("Acess DHIS2", extract_from_khis_page_ui("extraction_from_dhis2")),
  tags$head(tags$style(disconnection_notification_style)) # styles.R
)

server <- function(input, output) {
  comparison_service_consumption_page_server("service_consumption_comparison", data_to_plot = historical_fp_data)
  anomaly_detection_and_handling_page_server("anomaly_detection_and_handling", data_to_plot = historical_fp_data)
  forecast_results_consumption_page_server("forecasting_results_comparison", data_to_plot = forecast_results)
  live_prophet_forecasting_model_page_server("prophet_forecasting_model", data_to_forecast = historical_fp_data)
  extract_from_khis_page_server("extraction_from_dhis2")
}

shinyApp(ui, server, options = options(shiny.launch.browser = TRUE))
