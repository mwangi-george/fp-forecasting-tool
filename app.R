source("global.R")
source("utils/custom_functions.R")
source("R/comparison_service_consumption.R")


# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))


ui <- page_navbar(
  title = "FP Forecasting Tool",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    navbar_bg = "red"
  ),
  sidebar = sidebar(open = "closed"),
  nav_spacer(),
  nav_panel("Trend Analytics", comparison_service_consumption_page_ui("service_consumption_comparison")),
  nav_panel("Anomaly Detection", anomaly_detection_and_handling_ui("anomaly_detection_and_handling")),
  nav_panel("Forecast Results", forecast_results_consumption_page_ui("forecasting_results_comparison")),
  nav_panel("Live Model", live_prophet_forecasting_model_ui("prophet_forecasting_model")),
  nav_item(tags$a("Posit", href = "/"))
)

server <- function(input, output) {
  comparison_service_consumption_page_server("service_consumption_comparison", data_to_plot = historical_fp_data)
  anomaly_detection_and_handling_server("anomaly_detection_and_handling", data_to_plot = historical_fp_data)
  forecast_results_consumption_page_server("forecasting_results_comparison", data_to_plot = forecast_results)
  live_prophet_forecasting_model_server("prophet_forecasting_model", data_to_forecast = historical_fp_data)
}

shinyApp(ui, server, options = options(shiny.launch.browser = TRUE))
