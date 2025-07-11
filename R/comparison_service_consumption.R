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
        card_header("Trend Analysis: Comparison between consumption and service data for over time", class = "fs-6 fw-bold"),
        card_body(
          # max_height = "700px",
          # height = "650px",
          echarts4rOutput(ns("comparison_chart"))
        )
      )
    )
  )
}


comparison_service_consumption_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {
    observe({
      data_to_plot
      update_ui_elements(session, data_to_plot, use_both_approaches = TRUE)
    })

    filtered_data <- filter_historical_data(data_to_plot, input)

    output$comparison_chart <- renderEcharts4r({
      data <- filtered_data()

      # Convert period to Date format if necessary
      data$period <- as.Date(data$period)



      data %>%
        dplyr::group_by(method) %>%
        e_charts_("period") %>%
        e_line_("value", smooth=TRUE, draw = FALSE) %>%
        e_axis_labels(x = "Date", y = "Value") %>%
        e_theme("roma") %>%
        e_legend(right = 100) %>% # move legend to the right
        e_tooltip(trigger = "axis") %>%
        e_toolbox() %>%
        e_toolbox_feature(
          feature = "dataZoom"
        ) %>%
        e_toolbox_feature(
          feature = "saveAsImage"
        )
    })

    observe({
      input$analytic_for_service_consumption_comparison

      generate_text_for_converted_service_products(input, output)
    })
  })
}
