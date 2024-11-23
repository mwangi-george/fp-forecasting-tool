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
    base_font = font_google("Inter"),
    navbar_bg = "red"
  ),
  sidebar = sidebar(open = "closed"),
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

server <- function(input, output) {
  # Initialize reactive value with historical data
  data_to_use <- reactiveVal(value = historical_fp_data)

  # Call initial modules
  pre_processed_forecasts_page_server("forecasting_results_comparison", data_to_plot = forecast_results)
  khis_output <- extract_from_khis_page_server("extraction_from_dhis2")

  # Function to load data into modules
  load_data_into_modules <- function(data, trigger = NULL) {
    comparison_service_consumption_page_server("service_consumption_comparison", data_to_plot = data, listen_to = trigger)
    anomaly_detection_and_handling_page_server("anomaly_detection_and_handling", data_to_plot = data, listen_to = trigger)
    live_prophet_forecasting_model_page_server("prophet_forecasting_model", data_to_forecast = data, listen_to = trigger)
  }

  # Observe changes in KHIS output and handle accordingly
  observe({
    khis_data <- khis_output()

    if (is.null(khis_data)) {
      # Load modules with pre-loaded data
      load_data_into_modules(data_to_use())
    } else {
      if (khis_data |> nrow() > 0) {
        # Prompt user to decide whether to overwrite existing data
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
    data_to_use(khis_output() |> update_service_data_with_cyp()) # Overwrite reactive value with KHIS data
    load_data_into_modules(data_to_use(), trigger = input$use_khis_output) # Load modules with new data
  })

  # Handle user's decision to disregard KHIS output
  observeEvent(input$disregard_khis_output, {
    removeModal() # Remove the notification modal
  })
}

shinyApp(ui, server, options = options(shiny.launch.browser = TRUE))
