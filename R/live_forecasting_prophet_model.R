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
        materialSwitch(ns("run_anomalization"), label = "Check anomalies first", value = TRUE, width = "100%", status = "danger")
      ),
      card(
        full_screen = FALSE,
        card_header("Forecasting Outputs"),
        layout_column_wrap(
          height = "100px",
          value_box(
            title = "Lower Bound AMC",
            value = textOutput(ns("yhat_lower")),
            min_height = "90px",
            max_height = "120px",
            theme = "danger",
            showcase = bs_icon("arrow-down-right-circle-fill")
          ),
          value_box(
            title = "Forecast Estimate AMC",
            value = textOutput(ns("yhat")),
            min_height = "90px",
            max_height = "120px",
            theme = "success",
            showcase = bs_icon("arrow-right-circle-fill")
          ),
          value_box(
            title = "Upper Bound AMC",
            value = textOutput(ns("yhat_upper")),
            min_height = "90px",
            max_height = "120px",
            theme = "primary",
            showcase = bs_icon("arrow-up-right-circle-fill")
          )
        ),
        card(
          full_screen = TRUE,
          plotlyOutput(ns("forecast_plot"), height = "400px")
        ),
        tagList(
          div(
            actionButton(ns("show_forecast_data_table"), "Display Data Table", class = "btn-primary", style = "width: 25%;"),
            downloadButton(ns("download_model_data"), "Download Forecast", class = "btn-primary", style = "width: 25%;"),
            style = "display: flex; gap: 10px;"
          )
        )
      )
    )
  )
}

# Module Server
live_prophet_forecasting_model_page_server <- function(id, data_to_forecast) {

  model_results_table <- reactiveVal(
    value = tibble(
      product = "",
      org_unit = "",
      date = "",
      lower = "",
      forecast = "",
      upper = "",
      # seasonality = "",
      # growth_type = "",
      # anomalies_checked = "",
      # method = ""
    )
  )

  moduleServer(id, function(input, output, session) {

    observe({
      data_to_forecast

      new_inputs <- get_data_dimensions(data_to_forecast)

      updatePickerInput(
        session,
        inputId = "org_unit_for_service_consumption_comparison",
        choices = new_inputs$org_units,
        selected = new_inputs$org_units[1]
      )

      updatePickerInput(
        session,
        inputId = "analytic_for_service_consumption_comparison",
        choices = new_inputs$data_elements,
        selected = new_inputs$data_elements[1]
      )

      updatePickerInput(
        session,
        inputId = "forecasting_approach_for_service_consumption_comparison",
        choices = new_inputs$fp_approaches,
        selected = new_inputs$fp_approaches[1]
      )
    })


    prophet_input_data <- function() {
      req(input$run_forecast)

      # filter the data using user inputs
      data_to_forecast <- data_to_forecast |>
        filter(
          org_unit == input$org_unit_for_service_consumption_comparison,
          analytic == input$analytic_for_service_consumption_comparison,
          method %in% c(input$forecasting_approach_for_service_consumption_comparison)
        )

      if (input$run_anomalization) {
        # run anomaly detection and prepare data for modeling if user wants it
        anomalization_results <- data_to_forecast |>
          run_anomaly_detection()

        if (anomalization_results$success) {
          # process was successful
          data_to_forecast <- anomalization_results$res |>
            mutate(y = round(observed_clean))
        } else {
          # process failed
          data_to_forecast <- anomalization_results$res |>
            mutate(y = round(value))
        }
      } else {
        data_to_forecast <- data_to_forecast |>
          mutate(y = round(value))
      }

      # Standardize column names and sort
      data_to_forecast |>
        transmute(ds = period, y) |>
        arrange(ds)
    }

    prophet_output_data <- function() {

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
          }, min = 0, max = 10, value = 7, message = "Processing..."
        )
      )
      return(model_results)
    }

    observeEvent(input$run_forecast, {
      model_input <- prophet_input_data()

      model_results <- prophet_output_data()

      if (model_results$success) {
        withProgress(
          expr = {
            output$forecast_plot <- renderPlotly({
              build_prophet_model_results_chart(model_input, model_results$res, input)
            })

            forecast_table_data <- model_results$res |>
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

            model_results_table(forecast_table_data %>% select(-c(method, anomalies_checked, growth_type, seasonality)))

            file_name <- glue("{input$analytic_for_service_consumption_comparison}- {input$forecasting_approach_for_service_consumption_comparison}")

            output$download_model_data <- forecast_table_data |> download_data_as_csv(file_name)
            output$download_model_data_via_data_display <- forecast_table_data |> download_data_as_csv(file_name)

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
          }, min = 0, max = 10, value = 9, message = "Building visuals..."
        )
      } else {
        print("Condition for null rendering passed................")
        notify_client("Processing Error...", model_results$message)
        render_empty_forecast_visuals(output)
      }
    })

    # observe trigger button
    observeEvent(input$show_forecast_data_table, {
      ns <- session$ns
      showModal(modalDialog(
        title = div(tags$h3("Forecasts Results", style = heading_style)),
        downloadButton(ns("download_model_data_via_data_display"), "Download Forecast", class = "btn-primary", style = "width: 25%;"),
        model_results_table() |>
          render_data_with_reactable(
            columns_to_format = generate_reactable_columns(model_results_table())
          ),
        easyClose = TRUE, size = "xl", footer = NULL
      ))
    })
  })
}
