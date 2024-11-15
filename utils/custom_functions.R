make_ui_inputs <- function(ns, show_both_consumption_and_service = TRUE) {

  ui_inputs <- tagList(
    pickerInput(
      ns("org_unit_for_service_consumption_comparison"),
      label = "Choose Org Unit",
      choices = distinct_organisations, selected = "Kenya", multiple = FALSE, width = "100%"
    ),
    pickerInput(
      ns("analytic_for_service_consumption_comparison"),
      label = "Choose Product",
      choices = distinct_analytics, selected = "COCs", multiple = FALSE, width = "100%"
    ),
    pickerInput(
      ns("forecasting_approach_for_service_consumption_comparison"),
      label = "Choose Method",
      choices = forecasting_approaches,
      selected = forecasting_approaches, multiple = show_both_consumption_and_service,
      options = list(`live-search` = TRUE, style = "btn-danger")
    ),
    dateRangeInput(
      ns("date_range_for_service_consumption_comparison"), "Date range:",
      start = "2020-01-01",
      end = today(),
      min = "2020-01-01",
      max = today(),
      format = "mm/dd/yy",
      separator = " - "
    )
  )
  return(ui_inputs)
}


add_comma_sep_to_y_values <- function() {
  y_values = list(
    formatter = htmlwidgets::JS("
        function(value) {
          return value.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
        }
      ")
  )
  return(y_values)
}


filter_historical_data <- function(historical_data, input) {
  filtered_data <- reactive({
    historical_data %>%
      filter(
        org_unit == input$org_unit_for_service_consumption_comparison,
        analytic_name == input$analytic_for_service_consumption_comparison,
        period %>% between(input$date_range_for_service_consumption_comparison[1], input$date_range_for_service_consumption_comparison[2]),
        method %in% c(input$forecasting_approach_for_service_consumption_comparison)
      ) %>%
      # arrange data frame in ascending order of date
      arrange(period) %>%
      # deselect unnecessary columns
      select(-c(org_unit, outlier_size, year)) %>%
      # round median value to nearest whole number
      mutate(median_value = round(median_value))
  })
  return(filtered_data)
}
