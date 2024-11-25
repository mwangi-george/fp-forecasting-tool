comparison_service_consumption_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        make_ui_inputs(ns),
        textOutput(ns("text_for_converted_service_products"))
      ),
      card(
        full_screen = TRUE,
        card_header("Trend Analysis"),
        apexchartOutput(ns("comparison_chart"), height = "250px")
      )
    )
  )
}


comparison_service_consumption_page_server <- function(id, data_to_plot, listen_to) {
  moduleServer(id, function(input, output, session) {
    observeEvent(listen_to, {
      update_ui_elements(session, data_to_plot)
    }, ignoreNULL = TRUE)

    filtered_data <- filter_historical_data(data_to_plot, input)

    output$comparison_chart <- renderApexchart({
      filtered_data() |>
        apex(
          type = "spline",
          mapping = aes(x = as.Date(period), y = as.numeric(value), group = method)
        ) |>
        ax_labs(
          title = glue("Comparison between consumption and service data for over time"),
          x = "Date",
          y = "Value"
        ) |>
        ax_markers(size = 6) |>
        ax_tooltip(shared = FALSE, fillSeriesColor = TRUE, y = add_comma_sep_to_y_values())
    })

    observe({
      input$analytic_for_service_consumption_comparison

      generate_text_for_converted_service_products(input, output)
    })
  })
}
