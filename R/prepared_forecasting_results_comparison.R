forecast_results_consumption_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        make_ui_inputs(ns, date_range_end_date = forecast_results_end_date),
        p(strong("Models Used")),
        textOutput(ns("model_used_for_consumption")),
        textOutput(ns("model_used_for_service"))
      ),
      card(
        full_screen = TRUE,
        card_header("Forecasts Results"),
        apexchartOutput(ns("forecast_plot"))
      )
    )
  )
}

forecast_results_consumption_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {

    filtered_data <- reactive({
      data_to_plot |>
        filter(
          org_unit == input$org_unit_for_service_consumption_comparison,
          analytic_name == input$analytic_for_service_consumption_comparison,
          .index |> between(input$date_range_for_service_consumption_comparison[1], input$date_range_for_service_consumption_comparison[2]),
          method %in% c(input$forecasting_approach_for_service_consumption_comparison)
        )
    })


    output$forecast_plot <- renderApexchart({
        suppressWarnings(
          filtered_data() |>
            unite(col = ".method_key", method, .key, sep = " - ") |>
            apex(
              type = "spline",
              mapping = aes(x = as.Date(.index), y = as.numeric(.value), group = .method_key)
            ) |>
            ax_labs(
              title = glue("Forecast Plot"),
              # subtitle = glue("Showing  data"),
              x = "Date",
              y = "Value"
            ) |>
            ax_markers(size = 6) |>
            ax_tooltip(shared = FALSE, fillSeriesColor = TRUE, y = add_comma_sep_to_y_values())
        )
    })

    output$model_used_for_consumption <- renderText({
      glue("Consumption: {filtered_data() |> retrieve_model_info('Consumption')}")
    })
    output$model_used_for_service <- renderText({
      glue("Service: {filtered_data() |> retrieve_model_info('Service')}")
    })

  })
}
