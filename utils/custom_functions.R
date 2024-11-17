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
  function(data_to_anomalize) {
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
          print("Building your forecast with prophet...")

          model_results <- suppressWarnings(
            expr = {
              fit <- prophet(
                df = data_to_forecast,
                growth = growth_type,
                seasonality.mode = "additive",
                yearly.seasonality = show_seasonality,
                interval.width = 0.95
              )

              future <- make_future_dataframe(fit, periods = horizon, freq = "1 month", include_history = T)

              last_date <- tail(data_to_forecast$ds, n = 1)

              future <- future |> filter(ds > last_date)
              forecast <- predict(fit, future)
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
      options = list(dom = "Brt", buttons = c("excel", "pdf", "copy"), pageLength = 40)
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
        model_desc <- model_data |> distinct(.model_desc) |>  pull(.model_desc)
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




















