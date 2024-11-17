comparison_service_consumption_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        make_ui_inputs(ns)
      ),
      card(
        full_screen = TRUE,
        card_header("Trend Analysis"),
        apexchartOutput(ns("comparison_chart"))
      )
    )
  )
}


comparison_service_consumption_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- filter_historical_data(data_to_plot, input)

    output$comparison_chart <- renderApexchart({
      filtered_data() |>
        apex(
          type = "spline",
          mapping = aes(x = as.Date(period), y = as.numeric(value), group = method)
        ) |>
        ax_labs(
          title = glue("Comparison between consumption and service data for over time"),
          # subtitle = glue("Showing  data for {input$org_unit_for_service_consumption_comparison}"),
          x = "Date",
          y = "Value"
        ) |>
        ax_markers(size = 6) |>
        ax_tooltip(shared = FALSE, fillSeriesColor = TRUE, y = add_comma_sep_to_y_values())
    })
  })
}
