anomaly_detection_and_handling_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        make_ui_inputs(ns, show_both_consumption_and_service = FALSE)
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
      anomalization_results <- run_anomaly_detection(data_to_anomalize = filtered_data())
      plot_title <- glue("Showing data for {input$analytic_for_service_consumption_comparison} -- {input$org_unit_for_service_consumption_comparison}")

      if ("data.frame" %in% c(class(anomalization_results))) {
        output$anomalies_plot <- renderPlotly({
          anomalization_results |>
            plot_anomalies(period, .title = plot_title, .x_lab = "Date", .y_lab = "Value")
        })
        output$cleaned_anomaly_plot <- renderPlotly({
          anomalization_results |>
            plot_anomalies_cleaned(period, .title = plot_title, .x_lab = "Date", .y_lab = "Value")
        })
      } else {
        output$anomalies_plot <- renderPlotly({
          NULL
        })
        output$cleaned_anomaly_plot <- renderPlotly({
          NULL
        })
        shinyalert("Oops!", anomalization_results, type = "error", closeOnClickOutside = TRUE)
      }
    })
  })
}
