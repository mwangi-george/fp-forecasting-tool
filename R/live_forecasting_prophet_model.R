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
        materialSwitch(ns("run_anomalization"), label = "Check anomalies first", value = FALSE, width = "100%", status = "danger")
      ),
      navset_card_underline(
        title = "Forecasting Outputs",
        full_screen = TRUE,
        nav_panel(
          "Model Plot",
          plotlyOutput(ns("forecast_plot"), height = "auto"),
          downloadButton(ns("download_model_data"), "Download", class = "btn-primary", style = "width: 20%;")
        ),
        nav_panel(
          "Model Data",
          layout_column_wrap(
            value_box(
              title = "Lower AMC",
              value = textOutput(ns("yhat_lower")),
              min_height = "100px",
              max_height = "150px",
              theme = "danger",
              showcase = bs_icon("arrow-down-right-circle-fill")
            ),
            value_box(
              title = "Forecast AMC",
              value = textOutput(ns("yhat")),
              min_height = "100px",
              max_height = "150px",
              theme = "success",
              showcase = bs_icon("arrow-right-circle-fill")
            ),
            value_box(
              title = "Upper AMC",
              value = textOutput(ns("yhat_upper")),
              min_height = "100px",
              max_height = "150px",
              theme = "primary",
              showcase = bs_icon("arrow-up-right-circle-fill")
            )
          ),
          reactableOutput(ns("monthly_forecast"), height = "400px"),
          tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('monthly_forecast_download_csv')", class = "btn-primary", style = "width: 20%;")
        )
      )
    )
  )
}

# Module Server
live_prophet_forecasting_model_page_server <- function(id, data_to_forecast) {
  moduleServer(id, function(input, output, session) {
    prophet_input_data <- eventReactive(input$run_forecast, {

      # filter the data using user inputs
      data_to_forecast <- data_to_forecast |>
        filter(
          org_unit == input$org_unit_for_service_consumption_comparison,
          analytic_name == input$analytic_for_service_consumption_comparison,
          method %in% c(input$forecasting_approach_for_service_consumption_comparison)
        )

      # Check for empty data frame
      if (data_to_forecast |> nrow() == 0) {
        notify_client("Oops! No data...", "There's no data for the selected inputs")
        return(NULL)
      }

      if (input$run_anomalization) {

        # run anomaly detection and prepare data for modeling if user wants it
        data_to_forecast <- data_to_forecast |>
          run_anomaly_detection() |>
          mutate(y = round(observed_clean))

      } else {

        data_to_forecast <- data_to_forecast |>
          mutate(y = round(value))
      }

      # Standardize column names and sort
      data_to_forecast |>
        transmute(ds = period, y) |>
        arrange(ds)
    })

    #

    prophet_output_data <- eventReactive(input$run_forecast, {
      req(prophet_input_data())

      model_results <- suppressMessages(
        withProgress(
          expr = {
            expr <- {
              forecast_with_prophet(
                data_to_forecast = prophet_input_data(),
                horizon = input$horizon,
                growth_type = input$growth,
                show_seasonality = input$seasonality
              )
            }
          }, min = 0, max = 10, value = 9, message = "Processing..."
        )
      )
      return(model_results)
    })

    observeEvent(input$run_forecast, {
      req(prophet_input_data())

      if ("character" %in% class(prophet_output_data())) {
        notify_client("Processing Error...", prophet_output_data())

        output$forecast_plot <- renderPlotly({
          NULL
        })
        output$monthly_forecast <- renderReactable({
          NULL
        })
        output$yhat <- renderText({
          NULL
        })
        output$yhat_lower <- renderText({
          NULL
        })
        output$yhat_upper <- renderText({
          NULL
        })
      } else {
        withProgress(
          expr = {
            output$forecast_plot <- renderPlotly({
              build_prophet_model_results_chart(prophet_input_data(), prophet_output_data(), input)
            })

            forecast_table_data <- prophet_output_data() |>
              transmute(
                Product = input$analytic_for_service_consumption_comparison,
                `Org Unit` = input$org_unit_for_service_consumption_comparison,
                Date = as.Date(ds),
                Lower = round(yhat_lower),
                Forecast = round(yhat),
                Upper = round(yhat_upper),
                seasonality = input$seasonality,
                growth_type = input$growth,
                anomalies_checked = input$run_anomalization,
                method = input$forecasting_approach_for_service_consumption_comparison
              )

            output$download_model_data <- downloadHandler(
              filename = function() {
                glue("{input$analytic_for_service_consumption_comparison}- {input$forecasting_approach_for_service_consumption_comparison}.csv")
              },
              content = function(file) {
                write.csv(forecast_table_data, file, row.names = FALSE)
              }
            )

            # summaries <- forecast_table_data |> summarize(across(4:6, ~ sum(.x, na.rm = TRUE)))
            averages <- forecast_table_data |> summarize(across(4:6, ~ round(mean(.x, na.rm = TRUE))))

            output$yhat <- renderText({
              averages |>
                pull(Forecast) |>
                format(big.mark = ", ")
            })

            output$yhat_lower <- renderText({
              averages |>
                pull(Lower) |>
                format(big.mark = ", ")
            })

            output$yhat_upper <- renderText({
              averages |>
                pull(Upper) |>
                format(big.mark = ", ")
            })
            output$monthly_forecast <- renderReactable({
              df_to_render <- forecast_table_data |>
                select(-c(seasonality, growth_type, anomalies_checked, method))

              df_to_render |>
                 render_data_with_reactable(
                  dataset_id = "monthly_forecast_download_csv",
                  columns_to_format = generate_reactable_columns(df_to_render, c("Lower", "Forecast", "Upper"))
                )
            })

          }, min = 0, max = 10, value = 9, message = "Building visuals..."
        )
      }
    })
  })
}
