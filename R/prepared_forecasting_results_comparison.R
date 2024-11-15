forecast_results_consumption_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = TRUE,
        card_header("Filters"),
        make_ui_inputs(ns)
      ),
      card(
        full_screen = TRUE,
        card_header("Forecasts Results"),
        apexchartOutput(ns("forecast_plot"), height = "auto")
      )
    )
  )
}

forecast_results_consumption_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {

    filtered_data <- reactive({
      data_to_plot %>%
        filter(
          org_unit == input$org_unit_for_service_consumption_comparison,
          analytic_name == input$analytic_for_service_consumption_comparison,
          .index %>% between(input$date_range_for_service_consumption_comparison[1], input$date_range_for_service_consumption_comparison[2]),
          method %in% c(input$forecasting_approach_for_service_consumption_comparison)
        ) %>%
        unite(col = ".method_key", method, .key, sep = " - ")
    })


    output$forecast_plot <- renderApexchart({
        analytic <- filtered_data() %>% distinct(analytic_name) %>% pull(analytic_name)
      filtered_data() %>%
        apex(
          type = "spline",
          mapping = aes(x = as.Date(.index), y = as.numeric(.value), group = .method_key)
        ) %>%
        ax_labs(
          title = glue("Forecast Plot"),
          # subtitle = glue("Showing  data"),
          x = "Date",
          y = "Value"
        ) %>%
        ax_markers(size = 6) %>%
        ax_tooltip(shared = FALSE, fillSeriesColor = TRUE, y = add_comma_sep_to_y_values())
    })
  })
}
