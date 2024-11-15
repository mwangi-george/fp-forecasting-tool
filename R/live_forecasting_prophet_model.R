# Module User Interface
live_prophet_forecasting_model_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = TRUE,
        card_header("Filter"),
        make_ui_inputs(ns, show_both_consumption_and_service = FALSE),
        radioButtons(ns("seasonality"), "Show Seasonality", choices = c(Yes = T, No = F), selected = F),
        radioButtons(ns("growth"), "Growth Type", choices = c(Linear = "linear", Flat = "flat"), selected = "linear"),
        numericInput(ns("horizon"), "Forecast Horizon:", min = 1, max = 100, value = 12),
        actionButton(
          ns("run_forecast"), "Forecast",
          icon = icon("line-chart", lib = "font-awesome"),
          style = "color: #fff; background-color: steelblue; border-color: steelblue;width: 150px; height: 35px;"
        )
      ),
      navset_card_underline(
        title = "Forecasting Outputs",
        nav_panel("Model Plot", plotlyOutput(ns("forecast_plot")) %>% withSpinner(type = 4, size = 0.5)),
        nav_panel(
          "Model Data",
          value_box(title = "Summary Value", value = textOutput(ns("forecast_summary_value")), showcase = bs_icon("clock")),
          DTOutput(ns("monthly_forecast")) %>% withSpinner(type = 4, size = 0.5)
        )
      )
    )
  )
}

# Module Server
live_prophet_forecasting_model_server <- function(id, data_to_forecast) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- filter_historical_data(data_to_forecast, input)

    data <- eventReactive(input$run_forecast, {

      filtered_data() %>%
        transmute(ds = period, y = round(value)) %>%
        arrange(ds)
    })

    forecast_data <- eventReactive(input$run_forecast, {
      if (nrow(data()) >= 2) {
        fit <- prophet(
          data(),
          growth = input$growth, seasonality.mode = "additive",
          yearly.seasonality = input$seasonality, interval.width = 0.95
        )
        future <- make_future_dataframe(fit, periods = input$horizon, freq = "1 month", include_history = T)
        last_date <- tail(data()$ds, n = 1)
        future <- future %>% filter(ds > last_date)
        forecast <- predict(fit, future)
        return(forecast)
      } else {
        NULL
      }
    })

    observeEvent(input$run_forecast, {
      # This function renders a plotly chart of the actual data and the forecast.
      output$forecast_plot <- renderPlotly({
        if (!is.null(forecast_data())) {
          plot_ly() %>%
            add_trace(
              data = data(), x = ~ds, y = ~y, type = "scatter",
              mode = "lines+markers", name = "Actual Data",
              line = list(color = "#E73846"), marker = list(color = "#E73846", size = 5)
            ) %>%
            add_trace(
              data = forecast_data(), x = ~ds, y = ~yhat,
              type = "scatter", mode = "lines+markers", line = list(color = "#1C3557"),
              marker = list(color = "#1C3557", size = 5), name = "Forecast"
            ) %>%
            add_ribbons(
              data = forecast_data(), x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper,
              fillcolor = "gray90", line = list(color = "transparent"), name = "Forecast Interval"
            ) %>%
            layout(
              title = str_c(input$org_unit_for_service_consumption_comparison, input$analytic_for_service_consumption_comparison, "Forecast Plot", sep = " "),
              xaxis = list(title = "Date"), yaxis = list(title = "Value"), showlegend = FALSE
            )
        }
      })

      forecast_table <- reactive({
        forecast_data() %>%
          transmute(
            analytic_name = input$analytic_for_service_consumption_comparison,
            org_unit = input$org_unit_for_service_consumption_comparison,
            Date = as.Date(ds), Forecast = round(yhat), Lower = round(yhat_lower), Upper = round(yhat_upper)
          )
      })

      # This function renders a gt table of the forecast summary.
      output$monthly_forecast <- renderDT({
        if (!is.null(forecast_data())) {
          forecast_table() %>%
            datatable(
              rownames = F, extensions = "Buttons", editable = T,
              fillContainer = T,
              options = list(
                dom = "Bfrt", buttons = c("excel", "pdf"), pageLength = 40,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': 'red', 'color': 'white'});",
                  "}"
                )
              )
            )
        }
      })

      output$forecast_summary_value <- renderText({
        var <- forecast_table() %>%
          summarize(total = sum(Forecast, na.rm = T)) %>%
          pull(total)

        format(var, big.mark = ", ")
      })

      # end of observer
    })
  })
}
