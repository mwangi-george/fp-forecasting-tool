anomaly_detection_and_handling_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_underline(
      title = "Anomaly Plots",
      full_screen = TRUE,
      div(
        style = "display: flex; gap: 10px; padding: 5px 20px 5px 20px;",
        div(style = "flex: 1;", pickerInput(
          ns("org_unit_for_service_consumption_comparison"),
          label = "Organization Unit",
          choices = "",
          multiple = FALSE,
          width = "100%",
          options = list(`live-search` = TRUE)
        )),
        div(style = "flex: 1;", pickerInput(ns("analytic_for_service_consumption_comparison"),
          label = "FP Product",
          choices = "",
          multiple = FALSE,
          width = "100%",
          options = list(`live-search` = TRUE)
        )),
        div(style = "flex: 1;", pickerInput(ns("forecasting_approach_for_service_consumption_comparison"),
          label = "Method",
          choices = "",
          multiple = FALSE,
          width = "100%",
          options = list(`live-search` = TRUE)
        ))
      ),
      nav_panel("Anomaly Detection", plotlyOutput(ns("anomalies_plot"), height = "550px") |> withSpinner(type = 4, size = 0.5)),
      nav_panel("Anomaly Handling", plotlyOutput(ns("cleaned_anomaly_plot"), height = "550px") |> withSpinner(type = 4, size = 0.5))
    )
  )
}

anomaly_detection_and_handling_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
    data_to_plot |>
      filter(
        org_unit == input$org_unit_for_service_consumption_comparison,
        analytic == input$analytic_for_service_consumption_comparison,
        method %in% c(input$forecasting_approach_for_service_consumption_comparison)
      ) |>
      # arrange data frame in ascending order of date
      arrange(period)
  })

    observe({
      data_to_plot
      update_ui_elements(session, data_to_plot)
    })

    observeEvent(
      list(
        input$analytic_for_service_consumption_comparison,
        input$org_unit_for_service_consumption_comparison,
        input$forecasting_approach_for_service_consumption_comparison
      ),
      {
        req(input$analytic_for_service_consumption_comparison)

        anomalization_results <- run_anomaly_detection(data_to_anomalize = filtered_data())

        # plot_title <- glue("{input$analytic_for_service_consumption_comparison} -- {input$org_unit_for_service_consumption_comparison}")
        plot_title <- glue("Spotting the Exceptions: Outlier Analysis of {input$analytic_for_service_consumption_comparison} {input$forecasting_approach_for_service_consumption_comparison} Data in {input$org_unit_for_service_consumption_comparison} ")

        if (anomalization_results$success) {
          output$anomalies_plot <- renderPlotly({
            anomalization_results$res |>
              plot_anomalies(period, .title = plot_title, .x_lab = "Date", .y_lab = "Value", .legend_show = FALSE)
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
      }
    )
  })
}
