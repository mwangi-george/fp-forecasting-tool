pre_processed_forecasts_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        make_ui_inputs(ns, show_both_approaches = TRUE),
        p(strong("Models Used")),
        textOutput(ns("model_used_for_consumption")),
        textOutput(ns("model_used_for_service")),
        textOutput(ns("text_for_converted_service_products"))
      ),
      card(
        full_screen = TRUE,
        card_header("Forecasts Results"),
        card_body(
          max_height = "700px",
          height = "650px",
          apexchartOutput(ns("forecast_plot"), width = "95%")
        )
      )
    )
  )
}

pre_processed_forecasts_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {

    observe({
      data_to_plot
      update_ui_elements(session, data_to_plot, use_both_approaches = TRUE)
    })

    filtered_data <- reactive({
      data_to_plot |>
        filter(
          org_unit == input$org_unit_for_service_consumption_comparison,
          analytic == input$analytic_for_service_consumption_comparison,
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
            ax_tooltip(shared = FALSE, fillSeriesColor = TRUE, y = add_comma_sep_to_y_values()) %>%
            ax_legend(position = "bottom")
        )
    })

    output$model_used_for_consumption <- renderText({
      glue("Consumption: {filtered_data() |> retrieve_model_info('Consumption')}")
    })
    output$model_used_for_service <- renderText({
      glue("Service: {filtered_data() |> retrieve_model_info('Service')}")
    })

    observe({
      input$analytic_for_service_consumption_comparison

      generate_text_for_converted_service_products(input, output)
    })

  })
}
