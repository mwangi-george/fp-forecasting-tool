make_ui_inputs <- function(
    ns,
    start_date = NULL,
    end_date = NULL,
    min_date = NULL,
    max_date = NULL,
    show_both_approaches = TRUE) {
  ui_inputs <- tagList(
    pickerInput(
      ns("org_unit_for_service_consumption_comparison"),
      label = "Choose Org Unit",
      choices = "",
      multiple = FALSE,
      width = "100%",
      options = list(`live-search` = TRUE)
    ),
    pickerInput(
      ns("analytic_for_service_consumption_comparison"),
      label = "Choose Product",
      choices = "",
      multiple = FALSE,
      width = "100%",
      options = list(`live-search` = TRUE)
    ),
    pickerInput(
      ns("forecasting_approach_for_service_consumption_comparison"),
      label = "Choose Method",
      choices = "",
      multiple = show_both_approaches,
      width = "100%",
      options = list(`live-search` = TRUE)
    ),
    dateRangeInput(
      ns("date_range_for_service_consumption_comparison"),
      "Date range:",
      start = start_date,
      end = end_date,
      min = min_date,
      max = max_date,
      format = "mm/dd/yy",
      separator = " - ",
      width = "100%"
    )
  )
  return(ui_inputs)
}

get_data_dimensions <- function(data_to_use) {
  data_elements <- data_to_use |>
    distinct(analytic) |>
    pull(analytic)

  org_units <- data_to_use |>
    distinct(org_unit) |>
    pull(org_unit)

  fp_approaches <- data_to_use |>
    distinct(method) |>
    pull(method)

  dates <- data_to_use |>
    summarise(
      start_date = min(period, na.rm = TRUE),
      end_date = max(period, na.rm = TRUE)
    )

  start_date <- dates |> pull(start_date)
  end_date <- dates |> pull(end_date)

  dims <- list(
    data_elements = data_elements,
    org_units = org_units,
    fp_approaches = fp_approaches,
    start_date = start_date,
    end_date = end_date
  )

  return(dims)
}


update_ui_elements <- function(
    session,
    data_to_use,
    use_both_approaches = FALSE) {
  new_inputs <- get_data_dimensions(data_to_use)

  if (use_both_approaches) {
    fp_approaches <- new_inputs$fp_approaches
  } else {
    fp_approaches <- new_inputs$fp_approaches[1]
  }

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
    selected = fp_approaches
  )

  updateDateRangeInput(
    session,
    inputId = "date_range_for_service_consumption_comparison",
    start = new_inputs$start_date,
    min = new_inputs$start_date,
    end = new_inputs$end_date,
    max = new_inputs$end_date
  )
}

add_comma_sep_to_y_values <- function() {
  y_values <- list(
    formatter = htmlwidgets::JS(
      "
        function(value) {
          return value.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
        }
      "
    )
  )
  return(y_values)
}


filter_historical_data <- function(historical_data, input) {
  filtered_data <- reactive({
    historical_data |>
      filter(
        org_unit == input$org_unit_for_service_consumption_comparison,
        analytic == input$analytic_for_service_consumption_comparison,
        period |>
          between(
            input$date_range_for_service_consumption_comparison[1],
            input$date_range_for_service_consumption_comparison[2]
          ),
        method %in%
          c(input$forecasting_approach_for_service_consumption_comparison)
      ) |>
      # arrange data frame in ascending order of date
      arrange(period)
  })
  return(filtered_data)
}

memoised_login <- memoise(
  function(url, username, password) {
    login <- httr::GET(url, authenticate(username, password))

    cli_alert_info(login |> status_code())
    return(login)
  },
  # result automatically time out after 15 minutes
  cache = cachem::cache_mem(max_age = 60 * 15)
)

notify_client <- function(notification_title, notification_text) {
  showModal(
    modalDialog(
      title = div(tags$h3(notification_title, style = heading_style)),
      notification_text,
      easyClose = TRUE,
      size = "m"
    )
  )
}

use_khis_output_notification <- function() {
  showModal(
    modalDialog(
      title = div(tags$h3("Confirm Action", style = heading_style)),
      "Do you want to overwrite this app's preloaded data with the data you have just extracted?
      This will affect existing outputs in other tabs",
      easyClose = FALSE,
      size = "m",
      footer = tagList(
        actionButton(
          inputId = "use_khis_output",
          label = "Yes",
          icon = icon("thumbs-up"),
          width = NULL,
          class = "btn-primary",
          style = "width: 30%;"
        ),
        actionButton(
          inputId = "disregard_khis_output",
          label = "No",
          icon = icon("thumbs-down"),
          width = NULL,
          class = "btn-primary",
          style = "width: 30%;"
        )
      )
    )
  )
}


login_to_dhis2_within_shiny <- function(base_url, username, password) {
  login_status <- FALSE

  tryCatch(
    expr = {
      # Check if there's an internet connection and required inputs are not null
      if (
        curl::has_internet() &&
          !is.null(base_url) &&
          !is.null(username) &&
          !is.null(password)
      ) {
        # Construct the URL for the API endpoint
        url <- str_c(base_url, "/api/me")

        # Perform the login request & cache the response
        login <- memoised_login(url, username, password)

        if (status_code(login) == 200L) {
          notify_client(
            "Login Successful...",
            glue(
              "Welcome {username}! Select the data your want to extract on the sidebar and click the 'Extract' button."
            )
          )

          login_status <- TRUE
          return(login_status)
        } else {
          notify_client(
            "Login Failed...",
            "Invalid username/password. Please try again!"
          )
          return(login_status)
        }
      } else {
        notify_client(
          "Network Error",
          "Please check your internet connection or ensure all fields are filled."
        )
        return(login_status)
      }
    },
    error = function(e) {
      notify_client("Processing Error!", e$message)
      return(login_status)
    }
  )
}


run_anomaly_detection <- memoise(
  function(data_to_anomalize) {
    success <- FALSE

    tryCatch(
      expr = {
        cli_alert_info("Running outlier detection")
        anomalized_data <- suppressMessages(
          expr = {
            data_to_anomalize |>
              arrange(period) |>
              anomalize(period, value, .max_anomalies = 0.3, .iqr_alpha = 0.10)
          }
        )

        return(list(success = TRUE, res = anomalized_data))
      },
      error = function(e) {
        cli_alert_danger("Anomaly detection failed...")
        return(list(
          success = FALSE,
          res = data_to_anomalize,
          message = e$message
        ))
      }
    )
  }
)

forecast_with_prophet <- memoise(
  function(data_to_forecast, horizon, growth_type, show_seasonality) {
    success <- FALSE

    tryCatch(
      expr = {
        cli_alert_info("Building your forecast with Prophet...")
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
            future <- make_future_dataframe(
              fit,
              periods = horizon,
              freq = "1 month",
              include_history = TRUE
            )

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

        return(list(success = TRUE, res = model_results))
      },
      error = function(e) {
          cli_alert_danger("Error occurred while building forecast...")
        return(list(
          success = success,
          res = data_to_forecast,
          message = "Series has insufficient data to build a forecast"
        ))
      }
    )
  }
)


render_data_with_dt <- function(dt_object) {
  dt_object |>
    datatable(
      rownames = F,
      extensions = "Buttons",
      editable = TRUE,
      fillContainer = T,
      options = list(
        dom = "Brt",
        buttons = c("excel", "pdf", "copy"),
        pageLength = 40,
        ajax = NULL
      )
    )
}


render_data_with_reactable <- function(dataset, columns_to_format) {
  reactable(
    dataset,
    searchable = FALSE,
    pagination = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    bordered = TRUE,
    striped = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 20, 40, 80, 100),
    columns = columns_to_format,
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

# Function to create column definitions dynamically
generate_reactable_columns <- function(data, columns_to_format = NULL) {
  # If no specific columns are provided, select all numeric columns by default
  columns_to_format <- columns_to_format %||%
    names(select(data, where(is.numeric)))

  # Generate the list for the reactable columns argument using purrr::map
  column_definitions <- map(
    names(data),
    ~ if (.x %in% columns_to_format) {
      colDef(format = colFormat(separators = TRUE))
    } else {
      colDef()
    }
  ) %>%
    set_names(names(data)) # Convert the output to a named list

  return(column_definitions)
}

read_forecasts_data_from_drive <- function() {
  path <- "https://docs.google.com/spreadsheets/d/14h3_V3UZS8HrS5jjmN_SzjBjXwAvEtyKR7IQmTIioj8/"
  tryCatch(
    expr = {
        cli_alert_info("Importing forecast from drive...")
      ss <- googledrive::drive_get(path)
      forecasts <- googlesheets4::read_sheet(ss, sheet = "master")

      cli_alert_info("Saving data to disk...")
      saveRDS(forecasts, here::here("data/final_forecasts_drive.rds"))
    },
    error = function(e) {
        cli_alert_danger(e$message)
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
        cli_alert_danger(e$message)
    }
  )
}


build_prophet_model_results_chart <- function(actual_df, predicted_df, input) {
  plot_ly() |>
    add_trace(
      data = actual_df,
      x = ~ds,
      y = ~y,
      type = "scatter",
      mode = "lines+markers",
      name = "Actual Data",
      line = list(color = "#E73846"),
      marker = list(color = "#E73846", size = 5)
    ) |>
    add_trace(
      data = predicted_df,
      x = ~ds,
      y = ~yhat,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1C3557"),
      marker = list(color = "#1C3557", size = 5),
      name = "Estimate"
    ) |>
    add_ribbons(
      data = predicted_df,
      x = ~ds,
      ymin = ~yhat_lower,
      ymax = ~yhat_upper,
      fillcolor = "gray90",
      line = list(color = "transparent"),
      name = "Forecast Interval"
    ) |>
    layout(
      title = str_c(
        input$org_unit_for_service_consumption_comparison,
        input$analytic_for_service_consumption_comparison,
        "Forecast Plot",
        sep = " "
      ),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      showlegend = FALSE
    )
}


extraction_data_from_dhis2 <- memoise(
  function(
      connection,
      consumption_ids,
      service_ids,
      org_ids,
      period_range,
      output_scheme) {
    tryCatch(
      expr = {
        sample_df_if_query_fails <- tibble::tibble(
          org_unit = character(),
          analytic = character(),
          period = character(),
          value = numeric()
        ) |>
          mutate(period = ymd(period))

        cli_alert_info("Extracting requested data from khis aggregate web server...")
        response <- connection$get_analytics(
          analytic = c(consumption_ids, service_ids),
          org_unit = c(org_ids),
          period = c(period_range),
          output_scheme = output_scheme
        )

        if (nrow(response) == 0) {
          return(sample_df_if_query_fails)
        } else {
          khis_output <- response |>
            select(org_unit, analytic, period, value) |>
            mutate(period = period |> my())

          return(khis_output)
        }
      },
      error = function(e) {
        notify_client("Error during extraction", e$message)
        return(sample_df_if_query_fails)
      }
    )
  }
)

generate_api_url <- function(
    data_elements,
    org_units,
    start_month,
    end_month,
    outputIdScheme = "UID") {
  # Define api parts separately
  base_url <- "https://hiskenya.org/api/analytics.csv?"
  data_elements_spec <- paste0(
    "dimension=dx%3A",
    paste0(data_elements, sep = "", collapse = "%3B"),
    "&"
  )
  org_units_spec <- paste0(
    "dimension=ou%3AUSER_ORGUNIT%3B",
    paste0(org_units, collapse = "%3B"),
    "&"
  )
  periods_vector <- seq(
    from = as.Date(start_month),
    to = as.Date(end_month),
    by = "month"
  )
  periods_vector_formatted <- format(periods_vector, "%Y%m")
  periods_spec <- paste0(
    "dimension=pe%3A",
    paste0(periods_vector_formatted, collapse = "%3B"),
    "&"
  )
  other_params <- glue(
    "showHierarchy=false&hierarchyMeta=false&includeMetadataDetails=true&includeNumDen=false&skipRounding=false&completedOnly=false&outputIdScheme={outputIdScheme}"
  )

  api_url <- glue(
    base_url,
    data_elements_spec,
    org_units_spec,
    periods_spec,
    other_params
  )
  cli_alert_info(api_url)
  return(api_url)
}


extraction_data_from_dhis2_with_httr <- function(
    dhis_username,
    dhis_password,
    data_elements,
    org_units,
    start_month,
    end_month,
    outputIdScheme = "UID"
    ) {
  tryCatch(
    expr = {
      sample_df_if_query_fails <- tibble::tibble(
        org_unit = character(),
        analytic = character(),
        period = character(),
        value = numeric()
      ) |>
        mutate(period = ymd(period))

      cli_alert_info("Extracting requested data from khis aggregate web server...")

      response <- generate_api_url(
        data_elements,
        org_units,
        start_month,
        end_month,
        outputIdScheme
      ) %>%
        GET(url = ., authenticate(dhis_username, dhis_password))

      if (status_code(response) != 200) {
        return(sample_df_if_query_fails)
      }

      response_data <- response %>%
        content() %>%
        rawToChar(.) %>%
        read.csv(text = .) %>%
        clean_names() %>%
        rename(analytic = data, org_unit = organisation_unit)

      if (nrow(response_data) == 0) {
        return(sample_df_if_query_fails)
      } else {
        khis_output <- response_data |>
          select(org_unit, analytic, period, value) |>
          mutate(period = period |> my())

        return(khis_output)
      }
    },
    error = function(e) {
      notify_client("Error during extraction", e$message)
      return(sample_df_if_query_fails)
    }
  )
}


generate_text_for_converted_service_products <- function(input, output) {
  product <- input$analytic_for_service_consumption_comparison

  if (product == "COCs") {
    output$text_for_converted_service_products <- renderText({
      glue("A factor of 2.2 has been applied to to {product}'s Service data")
    })
  } else if (product %in% c("Female Condoms", "Male Condoms")) {
    output$text_for_converted_service_products <- renderText({
      glue("A factor of 10 has been applied to to {product}'s Service data")
    })
  } else if (product == "POPs") {
    output$text_for_converted_service_products <- renderText({
      glue("A factor of 1.5 has been applied to to {product}'s Service data")
    })
  } else {
    output$text_for_converted_service_products <- renderText({
      NULL
    })
  }
}

create_method_column <- function(df) {
  tryCatch(
    expr = {
      # Create a column for method
      df <- df |>
        mutate(
          method = case_when(
            analytic |> str_detect("711|UpS2bTVcClZ") ~ "Service",
            analytic |> str_detect("747|g3RQRuh8ikd") ~ "Consumption",
            .default = "Check this!!!!!!!!!!!!!!!"
          )
        )
      return(df)
    },
    error = function(e) {
        cli_alert_danger("There is chaos here--------------")
        cli_alert_danger(e$message)
    }
  )
}


standardize_dhis_dx_names <- function(df) {
  if (!"analytic" %in% colnames(df)) {
    return(df)
  }

  tryCatch(
    expr = {
      df <- df |>
        mutate(
          # remove dispensed tag
          analytic = analytic |> str_remove_all(".Dispensed"),

          # standardize names
          analytic = case_when(
            analytic %in%
              c(
                "MOH 711 Pills progestin only",
                "MOH 747A_Progestin only pills"
              ) ~
              "POPs",
            analytic %in%
              c(
                "MOH 711 Rev 2020_IUCD Insertion Non Hormonal",
                "MOH 747A_Non-Hormonal IUCD"
              ) ~
              "Non-Hormonal IUCD",
            analytic %in%
              c(
                "MOH 711 Client receiving Male condoms",
                "MOH 747A_Male Condoms"
              ) ~
              "Male Condoms",
            analytic %in% c("MOH 747A_Implants (2-Rod) - LNG 75mg (3 years)") ~
              "Levoplant",
            analytic %in%
              c(
                "MOH 711 Rev 2020_Implants insertion 1 Rod",
                "MOH 747A_Implants (1-Rod) – ENG 68mg"
              ) ~
              "Implanon",
            analytic %in% c("MOH 747A_Implant (2-Rod) – LNG 75mg (5 years)") ~
              "Jadelle",
            analytic %in%
              c(
                "MOH 711 Rev 2020_IUCD Insertion Hormonal",
                "MOH 747A_Hormonal IUCD"
              ) ~
              "Hormonal IUCD",
            analytic %in%
              c(
                "MOH 711 Clients receiving Female Condoms",
                "MOH 747A_Female Condoms"
              ) ~
              "Female Condoms",
            analytic %in%
              c(
                "MOH 711 Emergency contraceptive pill",
                "MOH 747A_Emergency Contraceptive pills"
              ) ~
              "EC Pills",
            analytic %in%
              c("MOH 711 Rev 2020_FP Injections DMPA- SC", "MOH 747A_DMPA-SC") ~
              "DMPA-SC",
            analytic %in%
              c("MOH 711 Rev 2020_FP Injections DMPA- IM", "MOH 747A_DMPA-IM") ~
              "DMPA-IM",
            analytic %in%
              c(
                "MOH 711 Rev 2020_Clients given cycle beads",
                "MOH 747A_Cycle Beads"
              ) ~
              "Cycle Beads",
            analytic %in%
              c(
                "MOH 711 Pills Combined oral contraceptive",
                "MOH 747A_Combined Oral contraceptive Pills"
              ) ~
              "COCs",
            analytic %in% c("MOH 711 Rev 2020_Implants insertion 2 Rod") ~
              "2 Rod",
            analytic == "g3RQRuh8ikd.REPORTING_RATE" ~
              "Consumption Reporting Rate",
            analytic == "UpS2bTVcClZ.REPORTING_RATE" ~ "Service Reporting Rate",
            .default = analytic
          )
        )

      return(df)
    },
    error = function(e) {
        cli_alert_danger(e$message)
      return(df)
    }
  )
}


show_in_excel <- function(df, format) {
  tmp <- paste0(tempfile(), ".xlsx")

  openxlsx::write.xlsx(df, tmp)
  fs::file_show(path = tmp)
}

download_data_as_csv <- function(x, name) {
  downloadHandler(
    filename = function() {
      glue("{name}.csv")
    },
    content = function(file) {
      write.csv(x, file, row.names = FALSE)
    }
  )
}


update_service_data_with_cyp <- function(data) {
  tryCatch(
    expr = {
      updated_data <- data |>
        mutate(
          value = case_when(
            method == "Service" & analytic == "COCs" ~ value * 2.2,
            method == "Service" & analytic == "POPs" ~ value * 1.5,
            method == "Service" & analytic == "Female Condoms" ~ value * 10,
            method == "Service" & analytic == "Male Condoms" ~ value * 10,
            .default = value
          )
        )

      return(updated_data)
    },
    error = function(e) {
        cli_alert_danger(e$message)
      notify_client("CYP Adjustment Error", e$message)
    }
  )
}


render_empty_forecast_visuals <- function(output) {
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
}


read_ai_forecast_df <- function() {
  wb_path <- here("data/final_forecasts_ai_approach.xlsx")

  forecast_df <- readxl::excel_sheets(wb_path) %>%
    map(., ~ read_excel(wb_path, sheet = .x)) %>%
    list_rbind() %>%
    rename(analytic = analytic_method) %>%
    mutate(.type = "forecast") %>%
    filter(period >= "2024-10-01")

  return(forecast_df)
}


calculate_accuracy_metrics <- function(data) {
  tryCatch(
    expr = {
        # Extract forecast and actual values from the dataframe
        forecast <- data$forecast
        actual <- data$actual

        # Mean Absolute Error (MAE)
        mae <- mean(abs(actual - forecast))

        # Mean Absolute Percentage Error (MAPE)
        mape <- mean(abs((actual - forecast) / actual)) * 100

        # Mean Absolute Scaled Error (MASE)
        mase <- mean(abs(actual - forecast)) / mean(abs(diff(actual)))

        # Symmetric Mean Absolute Percentage Error (SMAPE)
        smape <- 2 * mean(abs(actual - forecast) / (abs(actual) + abs(forecast))) * 100

        # Root Mean Squared Error (RMSE)
        rmse <- sqrt(mean((actual - forecast)^2))

        # R-squared (Coefficient of determination)
        rsq <- 1 - sum((actual - forecast)^2) / sum((actual - mean(actual))^2)

        # Return a named list of metrics
        metrics <- list(
            MAE = round(mae, 1),
            MAPE = round(mape, 1),
            MASE = round(mase, 1),
            SMAPE = round(smape, 1),
            RMSE = round(rmse, 1),
            Rsquared = round(rsq, 1)
        )

        return(metrics)
    },
    error = function(e) {
      cli_alert_danger(e$message)
    }
  )
}


get_and_organize_forecasts <- function() {

  # Source (https://docs.google.com/spreadsheets/d/1zmBECQ2T4y9SUng7XiYpCKnWi86YQoUdwYWSTICGuaw/edit?gid=0#gid=0)
  amcs <- tibble(
    analytic = c('COCs', 'Cycle Beads', 'DMPA-IM', 'DMPA-SC', 'EC Pills', 'Female Condoms', 'Hormonal IUCD', 'Implanon', 'Jadelle',
                 'Levoplant', 'Male Condoms', 'Non-Hormonal IUCD', 'POPs'),
    value = c(126506, 2141, 208949, 66827, 6547, 32378, 1597, 35833, 38497, 13981, 3094611, 6596, 31566),
    method = c("Demographic", "Demographic", "Consumption AI", "Demographic", "Consumption AI", "Consumption AI", "Service AI",
                       "Service AI", "Consumption AI", "Consumption AI", "Demographic", "Consumption Excel", "Consumption AI"),
    .type = rep("forecast", 13)
  )

  # Generate the sequence of periods from 2024-10-01 to 2025-10-01
  periods <- seq.Date(from = as.Date("2024-10-01"), to = as.Date("2025-10-01"), by = "month")

  # Expand to all periods in the future
  df <- amcs %>%
    tidyr::expand(analytic, period = periods) %>%
    left_join(amcs, by = "analytic")

  return(df)
}



update_forecast_accuracy_dataset <- function() {
  tryCatch(
    expr = {
      actual_consumption_data <- extraction_data_from_dhis2_with_httr(
        dhis_username = Sys.getenv("DHIS2_USERNAME"),
        dhis_password = Sys.getenv("DHIS2_PASSWORD"),
        data_elements = fp_consumption_747A_ids,
        org_units = "HfVjCurKxh2",
        start_month = "2024-10-01",
        end_month = today() - 30,
        outputIdScheme = "NAME"
      )
      cli_alert_success("Data extracted successfully...")

      forecast_vs_actual_df <- actual_consumption_data %>%
        select(-org_unit) %>%
        standardize_dhis_dx_names() %>%
        mutate(.type = "actual", method = "Actual Consumption") %>%
        bind_rows(get_and_organize_forecasts()) # merge with existing forecast data

      forecast_vs_actual_df %>%
        saveRDS("data/forecast_vs_actual_data.rds")

      cli_alert_success("Cleaned and saved successfully...")

      return(forecast_vs_actual_df)
    },
    error = function(e) {
      cli::cli_alert_danger(e$message)
      notify_client("Extraction error", e$message)
    }
  )
}
