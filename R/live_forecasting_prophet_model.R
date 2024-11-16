# Module User Interface
live_prophet_forecasting_model_page_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        pickerInput(
          ns("org_unit_for_service_consumption_comparison"),
          label = "Choose Org Unit",
          choices = distinct_organisations, selected = "Kenya", multiple = FALSE, width = "100%",
          options = list(`live-search` = TRUE)
        ),
        pickerInput(
          ns("analytic_for_service_consumption_comparison"),
          label = "Choose Product",
          choices = distinct_analytics, selected = "COCs", multiple = FALSE, width = "100%",
          options = list(`live-search` = TRUE)
        ),
        pickerInput(
          ns("forecasting_approach_for_service_consumption_comparison"),
          label = "Choose Method",
          choices = forecasting_approaches, multiple = FALSE, width = "100%",
          options = list(`live-search` = TRUE)
        ),
        actionButton(ns("run_forecast"), "Click to Forecast", icon = icon("line-chart"), class = "btn-primary", style = "width: 100%;"),
        numericInput(ns("horizon"), "Change Forecast Horizon:", min = 1, max = 100, value = 12),
        radioButtons(ns("seasonality"), "Show Seasonality", choices = c(Yes = T, No = F), selected = F),
        radioButtons(ns("growth"), "Choose Growth Type", choices = c(Linear = "linear", Flat = "flat"), selected = "linear"),
      ),
      navset_card_underline(
        title = "Forecasting Outputs",
        full_screen = TRUE,
        nav_panel(
          "Model Data",
          layout_column_wrap(
            value_box(title = "Total Lower", value = textOutput(ns("yhat_lower")), height = "100px", theme = "red"),
            value_box(title = "Total Estimate", value = textOutput(ns("yhat")), height = "100px", theme = "success"),
            value_box(title = "Total Upper", value = textOutput(ns("yhat_upper")), height = "100px", theme = "warning")
          ),
          DTOutput(ns("monthly_forecast"), height = 400)
        ),
        nav_panel("Model Plot", plotlyOutput(ns("forecast_plot"), height = "auto"))
      )
    )
  )
}

# Module Server
live_prophet_forecasting_model_page_server <- function(id, data_to_forecast) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- filtered_data <- reactive({
      data_to_forecast %>%
        filter(
          org_unit == input$org_unit_for_service_consumption_comparison,
          analytic_name == input$analytic_for_service_consumption_comparison,
          method %in% c(input$forecasting_approach_for_service_consumption_comparison)
        ) %>%
        # arrange data frame in ascending order of date
        arrange(period) %>%
        # deselect unnecessary columns
        select(-c(org_unit, outlier_size, year)) %>%
        # round median value to nearest whole number
        mutate(median_value = round(median_value))
    })

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
              marker = list(color = "#1C3557", size = 5), name = "Estimate"
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

      forecast_table_data <- reactive({
        forecast_data() %>%
          transmute(
            analytic_name = input$analytic_for_service_consumption_comparison,
            org_unit = input$org_unit_for_service_consumption_comparison,
            Date = as.Date(ds), Lower = round(yhat_lower), Estimate = round(yhat), Upper = round(yhat_upper)
          )
      })

      output$monthly_forecast <- renderDT({
        if (!is.null(forecast_data())) {
          forecast_table_data() %>%
            datatable(
              rownames = F, extensions = "Buttons", editable = TRUE, fillContainer = T,
              options = list(dom = "Brt", buttons = c("excel", "pdf", "copy"), pageLength = 40)
            ) %>%
            DT::formatCurrency(4:6, currency = "", digits = 0)
        }
      })

      observe({
        forecast_table_data()
        summaries <- forecast_table_data() %>% summarize(across(4:6, ~ sum(.x, na.rm = TRUE)))

        output$yhat <- renderText({
          summaries %>%
            pull(Estimate) %>%
            format(., big.mark = ", ")
        })

        output$yhat_lower <- renderText({
          summaries %>%
            pull(Lower) %>%
            format(., big.mark = ", ")
        })

        output$yhat_upper <- renderText({
          summaries %>%
            pull(Upper) %>%
            format(., big.mark = ", ")
        })
      })

      # end of observer
    })
  })
}
