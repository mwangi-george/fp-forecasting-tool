source("global.R")

# source R scripts in sub directories
dir_ls("utils/") |> map(~ source(.x))
dir_ls("R/") |> map(~ source(.x))

# Enable thematic
thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))


ui <- page_navbar(
  useShinyjs(), # initialize shinyjs
  title = "FP Forecasting Tool",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),
  sidebar = sidebar(
    open = "closed",
    actionButton(
      "use_preloaded_data",
      label = "Use Preloaded Data",
      class = "btn-primary",
      style = "width: 100%;"
    )
  ),
  footer = tagList(
    hidden(
      actionButton(
        "update_data",
        label = "New data detected! Click here to update your visuals",
        class = "btn-primary",
        style = "width: 100%;"
      )
    )
  ),
  nav_item(tags$a("Home", href = "/")),
  nav_spacer(),
  nav_panel("Trend Analytics", comparison_service_consumption_page_ui("service_consumption_comparison")),
  nav_panel("Anomaly Detection", anomaly_detection_and_handling_page_ui("anomaly_detection_and_handling")),
  nav_panel("Forecast Results", pre_processed_forecasts_page_ui("forecasting_results_comparison")),
  nav_panel("Live Model", live_prophet_forecasting_model_page_ui("prophet_forecasting_model")),
  nav_panel("Acess DHIS2", extract_from_khis_page_ui("extraction_from_dhis2")),
  tags$head(tags$style(disconnection_notification_style)) # styles.R
)


server <- function(input, output, session) {

  # File path to monitor
  file_path <- "data/historical_fp_data.rds" # Replace with your actual file path

  # Reactive file reader
  file_data <- reactiveFileReader(
    intervalMillis = 60000,  # Check for file changes every 50 seconds
    session = session,      # Provide the session object
    filePath = file_path,   # File to monitor
    readFunc = readRDS    # Function to read the file
  )

  pre_processed_forecasts_page_server("forecasting_results_comparison", data_to_plot = forecast_results)
  khis_output <- extract_from_khis_page_server("extraction_from_dhis2")


  # Observe changes in KHIS output and handle accordingly
  observe({
    comparison_service_consumption_page_server("service_consumption_comparison", data_to_plot = file_data())
    anomaly_detection_and_handling_page_server("anomaly_detection_and_handling", data_to_plot = file_data())
    live_prophet_forecasting_model_page_server("prophet_forecasting_model", data_to_forecast = file_data())
  })

  observe({
    khis_data <- khis_output()

    if (!is.null(khis_data)) {
      if (nrow(khis_output()) > 0) {
        showElement(id = "update_data", anim = TRUE)
      }
    }
  })

  observeEvent(input$update_data, {
    use_khis_output_notification() # see definition in custom_functions.R
    hide(id = "update_data", anim = TRUE) # hide the update data button
  })

  # Handle user's decision to use KHIS output
  observeEvent(input$use_khis_output, {
    removeModal() # Remove the notification modal
    khis_output() |>
      update_service_data_with_cyp() |>
      saveRDS("data/historical_fp_data.rds")

    runjs("location.reload();")  # reload the app
  })

  # Handle user's decision to disregard KHIS output
  observeEvent(input$disregard_khis_output, {
    removeModal() # Remove the notification modal
  })

  observeEvent(input$use_preloaded_data, {
    historical_fp_data |>
      saveRDS("data/historical_fp_data.rds")

    runjs("location.reload();") # reload app
  })
}

shinyApp(ui, server, options = options(shiny.launch.browser = TRUE))
