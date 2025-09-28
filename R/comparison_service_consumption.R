comparison_service_consumption_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_underline(
      title = "Trend Analysis",
      full_screen = TRUE,
      div(
        style = "display: flex; gap: 10px; padding: 2px 20px 5px 20px;",
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
          multiple = TRUE,
          width = "100%",
          options = list(`live-search` = TRUE)
        )),
        div(style = "flex: 1;", dateRangeInput(
          ns("date_range_for_service_consumption_comparison"),
          "Date range:",
          start = NULL,
          end = NULL,
          min = NULL,
          max = NULL,
          format = "mm/dd/yy",
          separator = " - ",
          width = "100%"
        ))
      ),
      nav_panel("", echarts4rOutput(ns("comparison_chart"), height = "550px"), textOutput(ns("text_for_converted_service_products"))),
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
        e_line_("value", smooth = TRUE, draw = FALSE) %>%
        e_axis_labels(x = "Date", y = "Value") %>%
        e_title(glue("Is Service Provision Keeping Pace with Consumption? A Time Trend for {input$analytic_for_service_consumption_comparison} in {input$org_unit_for_service_consumption_comparison}")) %>%
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
