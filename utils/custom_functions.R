make_ui_inputs <- function(ns, show_both_consumption_and_service = TRUE) {
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
      end = today(),
      min = "2020-01-01",
      max = today(),
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


make_dhis2r_connection <- memoise(
  function(his_base_url, user, pass) {
    # custom dhis2 connection using dhis2r package
    connection_to_his <- Dhis2r$new(base_url = his_base_url, username = user, password = pass)
    return(connection_to_his)
  },
  # result automatically time out after 15 minutes
  cache = cachem::cache_mem(max_age = 60 * 15)
)


# Customized Data extraction function
extract_data_from_his <- memoise(
  function(con, analytic, org_unit, date_range, output_format = "NAME") {
    response <- con$get_analytics(
      analytic = c(analytic), org_unit = c(org_unit),
      period = date_range, output_scheme = output_format
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
      options = list(dom = "Brt", buttons = c("excel", "pdf", "copy"), pageLength = 40)
    )
}


