make_ui_inputs <- function(ns, show_both_consumption_and_service = TRUE, date_range_end_date = today()) {
  ui_inputs <- tagList(
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
      choices = forecasting_approaches,
      selected = forecasting_approaches,
      multiple = show_both_consumption_and_service,
      width = "100%",
      options = list(`live-search` = TRUE)
    ),
    dateRangeInput(
      ns("date_range_for_service_consumption_comparison"), "Date range:",
      start = "2020-01-01",
      end = date_range_end_date,
      min = "2020-01-01",
      max = date_range_end_date,
      format = "mm/dd/yy",
      separator = " - ",
      width = "100%"
    )
  )
  return(ui_inputs)
}


add_comma_sep_to_y_values <- function() {
  y_values <- list(
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
    historical_data |>
      filter(
        org_unit == input$org_unit_for_service_consumption_comparison,
        analytic_name == input$analytic_for_service_consumption_comparison,
        period |> between(input$date_range_for_service_consumption_comparison[1], input$date_range_for_service_consumption_comparison[2]),
        method %in% c(input$forecasting_approach_for_service_consumption_comparison)
      ) |>
      # arrange data frame in ascending order of date
      arrange(period) |>
      # deselect unnecessary columns
      select(-c(org_unit, outlier_size, year)) |>
      # round median value to nearest whole number
      mutate(median_value = round(median_value))
  })
  return(filtered_data)
}

memoised_login <- memoise(
  function(url, username, password) {
    login <- httr::GET(url, authenticate(username, password))

    print(login |> status_code())
    return(login)
  },
  # result automatically time out after 15 minutes
  cache = cachem::cache_mem(max_age = 60 * 15)
)

login_to_dhis2_within_shiny <- function(base_url, username, password) {
  login_status <- FALSE

  tryCatch(
    expr = {
      # Check if there's an internet connection and required inputs are not null
      if (curl::has_internet() && !is.null(base_url) && !is.null(username) && !is.null(password)) {
        # Construct the URL for the API endpoint
        url <- str_c(base_url, "/api/me")

        # Perform the login request & cache the response
        login <- memoised_login(url, username, password)

        if (status_code(login) == 200L) {
          shinyalert(
            title = "Success",
            text = glue("Welcome {username}! Select the data your want to extract on the sidebar and click the 'Extract' button."), "success",
            closeOnClickOutside = TRUE, closeOnEsc = TRUE
          )
          login_status <- TRUE
          return(login_status)
        } else {
          shinyalert("Failed", "Invalid username/password. Please try again!", "error", closeOnClickOutside = TRUE, closeOnEsc = TRUE)
          return(login_status)
        }
      } else {
        shinyalert("Network Error", "Please check your internet connection or ensure all fields are filled.", "error", closeOnClickOutside = TRUE, closeOnEsc = TRUE)
        return(login_status)
      }
    },
    error = function(e) {
      shinyalert("Processing Error!", e$message, "error", closeOnClickOutside = TRUE, closeOnEsc = TRUE)
      return(login_status)
    }
  )
}


run_anomaly_detection <- memoise(
  function(data_to_anomalize, date_col, value_col) {
    tryCatch(
      expr = {
        print("Running anomaly detection...")
        anomalized_data <- suppressMessages(
          expr = {
            data_to_anomalize |> anomalize(period, value)
          }
        )
        return(anomalized_data)
      },
      error = function(e) {
        print(e$message)
        return("The selected series is not periodic or has less than two periods. Please review it's trend in the Trend Analysis tab.")
      }
    )
  }
)

forecast_with_prophet <- memoise(
  function(data_to_forecast, horizon, growth_type, show_seasonality) {
    if ((data_to_forecast |> nrow()) > 2) {
      tryCatch(
        expr = {
          print("Building your forecast with Prophet...")
          model_results <- suppressWarnings(
            expr = {
              # Fit the Prophet model
              fit <- prophet(
                df = data_to_forecast,
                growth = growth_type,
                seasonality.mode = "additive",
                yearly.seasonality = show_seasonality,
                interval.width = 0.80
              )

              # Create future dates for forecasting
              future <- make_future_dataframe(fit, periods = horizon, freq = "1 month", include_history = TRUE)

              # Ensure the future dates are beyond the last date in the input data
              last_date <- tail(data_to_forecast$ds, n = 1)
              future <- future |> filter(ds > last_date)

              # Generate the forecast
              forecast <- predict(fit, future)

              # Apply a lower bound to ensure no negative forecasts or intervals
              forecast <- forecast %>%
                mutate(
                  yhat = pmax(yhat, 0),
                  yhat_lower = pmax(yhat_lower, 0),
                  yhat_upper = pmax(yhat_upper, 0)
                )

              # Return the adjusted forecast
              forecast
            }
          )

          return(model_results)
        },
        error = function(e) {
          print("Error occurred while building forecast...")
          return(e$message)
        }
      )
    } else {
      return("Series has insufficient data to build a forecast...")
    }
  }
)


render_data_with_dt <- function(dt_object) {
  dt_object |>
    datatable(
      rownames = F,
      extensions = "Buttons",
      editable = TRUE,
      fillContainer = T,
      options = list(dom = "Brt", buttons = c("excel", "pdf", "copy"), pageLength = 40, ajax=NULL)
    )
}


render_data_with_reactable <- function(dataset, dataset_id) {
  reactable(
    dataset,
    elementId = dataset_id,
    searchable = TRUE,
    pagination = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    bordered = TRUE,
    striped = TRUE,
    defaultPageSize = 40,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 20, 40, 80, 100),
    theme = reactableTheme(
      color = "#333",
      borderColor = "#ccc",
      stripedColor = "#f9f9f9",
      highlightColor = "#ccc",
      cellPadding = "8px 12px",
      style = list(fontFamily = "Arial")
    )
  )
}

read_forecasts_data_from_drive <- function() {
  path <- "https://docs.google.com/spreadsheets/d/14h3_V3UZS8HrS5jjmN_SzjBjXwAvEtyKR7IQmTIioj8/"
  tryCatch(
    expr = {
      print("Importing forecast from drive...")
      ss <- googledrive::drive_get(path)
      forecasts <- googlesheets4::read_sheet(ss, sheet = "master")

      print("Saving data to disk...")
      saveRDS(forecasts, here::here("data/final_forecasts_drive.rds"))
    },
    error = function(e) {
      print(e$message)
    }
  )
}


retrieve_model_info <- function(dataset, fp_method) {
  tryCatch(
    expr = {
      model_data <- dataset |>
        filter(.key == "prediction", method == fp_method)

      if (model_data |> nrow() > 0) {
        model_desc <- model_data |>
          distinct(.model_desc) |>
          pull(.model_desc)
        return(model_desc)
      } else {
        return("No data")
      }
    },
    error = function(e) {
      print(e$message)
    }
  )
}



build_prophet_model_results_chart <- function(actual_df, predicted_df, input) {
  plot_ly() |>
    add_trace(
      data = actual_df, x = ~ds, y = ~y, type = "scatter",
      mode = "lines+markers", name = "Actual Data",
      line = list(color = "#E73846"), marker = list(color = "#E73846", size = 5)
    ) |>
    add_trace(
      data = predicted_df, x = ~ds, y = ~yhat,
      type = "scatter", mode = "lines+markers", line = list(color = "#1C3557"),
      marker = list(color = "#1C3557", size = 5), name = "Estimate"
    ) |>
    add_ribbons(
      data = predicted_df, x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper,
      fillcolor = "gray90", line = list(color = "transparent"), name = "Forecast Interval"
    ) |>
    layout(
      title = str_c(input$org_unit_for_service_consumption_comparison, input$analytic_for_service_consumption_comparison, "Forecast Plot", sep = " "),
      xaxis = list(title = "Date"), yaxis = list(title = "Value"), showlegend = FALSE
    )
}


extraction_data_from_dhis2 <- memoise(
  function(connection, consumption_ids, service_ids, org_ids, period_range, output_scheme) {
    tryCatch(
      expr = {
        sample_df_if_query_fails <- tibble::tibble(
          analytic = character(),
          org_unit = character(),
          period = character(),
          value = numeric()
        )

        print("Extracting requested data from khis aggregate web server...")
        response <- connection$get_analytics(
          analytic = c(consumption_ids, service_ids),
          org_unit = c(org_ids),
          period = c(period_range),
          output_scheme = output_scheme
        )

        if (nrow(response) == 0) {
          return(sample_df_if_query_fails)
        }
        return(response)
      },
      error = function(e) {
        shinyalert("Error during extraction", e$message, "error", closeOnClickOutside = TRUE)
        return(sample_df_if_query_fails)
      }
    )
  }
)

