# UI function
forecast_accuracy_tracker_ui <- function(id) {
  ns <- NS(id)

  # UI elements
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Filters"),
        card_body(
          pickerInput(
            ns("analytics_to_analyze"),
            label = "Choose Product",
            choices = distinct_analytics,
            selected = "EC Pills",
            multiple = FALSE,
            width = "100%",
            options = list(`live-search` = TRUE),
          ),
          layout_column_wrap(
            max_height = "400px",
            value_box(
              title = "MAPE",
              value = textOutput(ns("mape_value")),
              theme = "success",
              max_height = "90px",
            ),
            value_box(
              title = "RMSE",
              value = textOutput(ns("rmse_value")),
              theme = "success",
              max_height = "90px"
            ),
            value_box(
              title = "MAE",
              value = textOutput(ns("mae_value")),
              theme = "success",
              max_height = "90px"
            )
          ),
          p(tags$em("Note: Actual consumption data retrieved from KHIS")),
          actionButton(ns("update_actual_data"), "Refresh Actual Data", class = "btn-primary", style = "width: 100%;"),
        )
      ),
      card(
        card_header("Forecasting Accuracy Results"),
        card_body(
          echarts4rOutput(ns("forecast_accuracy_plot")),
          textOutput(ns("adopted_method_text")),
        )
      )
    )
  )
}

forecast_accuracy_tracker_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$update_actual_data, {
      withProgress(
          expr = {
            update_forecast_accuracy_dataset()
          }, min = 0, max = 10, value = 7, message = "Extracting..."
        )
      runjs("location.reload();") # reload the app
    })

    observe({
      input$analytics_to_analyze
    })

    filtered_df <- reactive({
      forecast_actual_df %>%
        filter(analytic == input$analytics_to_analyze) %>%
        arrange(period)
    })

    output$forecast_accuracy_plot <- renderEcharts4r({
      # Calculate min and max values for dynamic Y-axis scaling
      # y_min <- min(filtered_df()$value, na.rm = TRUE)
      # y_max <- max(filtered_df()$value, na.rm = TRUE)

      filtered_df() %>%
        dplyr::group_by(.type) %>%
        e_charts_("period") %>%
        e_line_("value", smooth = TRUE, draw = FALSE) %>%
        e_axis_labels(x = "Date", y = "Value") %>%
        e_theme("roma") %>%
        e_legend(right = 100) %>% # move legend to the right
        e_tooltip(trigger = "axis") %>%
        e_toolbox() %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_title(text = glue("Comparison between Actual & Forecast Data for {input$analytics_to_analyze}"))
        # e_y_axis(min = y_min * 0.9, max = y_max * 1.1)
    })

    output$adopted_method_text <- renderText({
      method <- filtered_df() %>%
        select(method) %>%
        filter(method != "Actual Consumption") %>%
        distinct() %>%
        pull(method)

      glue("{input$analytics_to_analyze} forecast was generated using the {method} approach")
    })

    observe({
      get_metrics_list <- memoise(
        function(dataset) {
        # define a default list to return incase of error
        default_output <- list(
          MAE = "N/A", MAPE = "N/A",
          MASE = "N/A", SMAPE = "N/A",
          RMSE = "N/A", Rsquared = "N/A"
        )
        tryCatch(
          expr = {
            df <- dataset %>%
              select(-method) %>%
              pivot_wider(names_from = .type, values_from = value)

            if (!"forecast" %in% colnames(df)) {
              cli_alert("Invalid data")
              notify_client("Forecasted data not available!")
              return(default_output)
            }

            if (!"actual" %in% colnames(df)) {
              cli_alert("Invalid data")
              notify_client("Actula data not available!")
              return(default_output)
            }

            metrics <- df %>%
              filter(!is.na(forecast), !is.na(actual)) %>%
              calculate_accuracy_metrics()

            cli_alert(metrics)
            return(metrics)
          },
          error = function(e) {
            cli_alert_danger("Error calculating accuracy metrics")
            return(default_output)
          }
        )
      })

      metrics_list <- get_metrics_list(filtered_df())

      output$mape_value <- renderText({
        if (metrics_list$MAPE == "N/A") {
          "N/A"
        } else {
          paste0(metrics_list$MAPE, "%")
        }
      })

      output$rmse_value <- renderText({
        metrics_list$RMSE
      })

      output$mae_value <- renderText({
        metrics_list$MAE
      })
    })
  })
}
