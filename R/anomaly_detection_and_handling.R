anomaly_detection_and_handling_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        make_ui_inputs(ns, show_both_approaches = FALSE),
        actionButton(ns("run_anomaly_detection"), "Run Anomaly Detection", class = "btn-primary", style = "width: 100%;")
      ),
      navset_card_underline(
        title = "Anomaly Plots",
        full_screen = TRUE,
        nav_panel("Anomaly Detection", plotlyOutput(ns("anomalies_plot"), height = "100%") |> withSpinner(type = 4, size = 0.5)),
        nav_panel("Anomaly Handling", plotlyOutput(ns("cleaned_anomaly_plot"), height = "100%") |> withSpinner(type = 4, size = 0.5))
      )
    )
  )
}

anomaly_detection_and_handling_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- filter_historical_data(data_to_plot, input)

    observe({
      data_to_plot
      update_ui_elements(session, data_to_plot)
    })

    observeEvent(input$run_anomaly_detection, {
      req(input$run_anomaly_detection)

      anomalization_results <- run_anomaly_detection(data_to_anomalize = filtered_data())

      plot_title <- glue("Showing data for {input$analytic_for_service_consumption_comparison} -- {input$org_unit_for_service_consumption_comparison}")

      if (anomalization_results$success) {
        output$anomalies_plot <- renderPlotly({
          anomalization_results$res |>
            plot_anomalies(period, .title = plot_title, .x_lab = "Date", .y_lab = "Value")
        })
        output$cleaned_anomaly_plot <- renderPlotly({
          anomalization_results$res |>
            plot_anomalies_cleaned(period, .title = plot_title, .x_lab = "Date", .y_lab = "Value")
        })
      } else {
        output$anomalies_plot <- renderPlotly({
          NULL
        })
        output$cleaned_anomaly_plot <- renderPlotly({
          NULL
        })
        notify_client("Oops!", anomalization_results$message)
      }
    })
  })
}
