extract_from_khis_page_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        full_screen = FALSE,
        card_header("Filters"),
        actionButton(
          ns("click_here_to_show_khis_login_modal"),
          label = "Login here",
          icon = icon("location-arrow"),
          class = "btn-primary",
          style = "width: 100%;"
        ),
        pickerInput(
            ns("fp_consumption_data_ids"),
            label = "Choose Consumption Data",
            choices = fp_consumption_747A_ids,
            selected = "hH9gmEmEhH4.to0Pssxkq4S", # COC dispensed ID
            width = "100%",
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE, `live-search` = TRUE)
        ),
        pickerInput(
            ns("fp_service_data_ids"),
            label = "Choose Service Data",
            choices = fp_service_711_ids,
            selected = "BQmcVE8fex4", # COC dispensed ID
            multiple = TRUE,
            width = "100%",
            options = pickerOptions(actionsBox = TRUE, `live-search` = TRUE)
        ),
        pickerInput(
            ns("his_org_unit"),
            label = "Choose County or Country",
            choices = counties_and_country_ids,
            selected = "HfVjCurKxh2",
            multiple = TRUE,
            width = "100%",
            options = pickerOptions(actionsBox = TRUE, `live-search` = TRUE)
        ),
        dateRangeInput(
            ns("his_date_range"),
            label = "Period",
            start = as.Date("2024-01-01"),
            end = today(),
            min = as.Date("2021-01-01"),
            max = today(),
            width = "100%"
        ),
        disabled(
            actionButton(
                ns("click_here_to_extract_selected_data_from_khis"),
                label = "Extract",
                icon = icon("cloud-arrow-down"),
                class = "btn-primary",
                style = "width: 100%;"
            )
        ),
        pickerInput(
            ns("his_output_scheme"),
            label = "Select Your Output Scheme",
            choices = c("NAME", "UID"),
            selected = "NAME",
            multiple = FALSE,
            width = "100%",
            options = pickerOptions(`live-search` = TRUE)
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Extraction Results"),
        reactableOutput(ns("extraction_results_table"), height = "500"),
        downloadButton(ns("extraction_results_export"), "Download", class = "btn-primary", style = "width: 20%;")
      )
    )
  )
}


extract_from_khis_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # get namespace
    ns <- session$ns
    dhis_connection <- reactiveVal(NULL)
    base_url <- "https://hiskenya.org"
    dhis_output <- reactiveVal(NULL)

    # observe trigger button
    observeEvent(input$click_here_to_show_khis_login_modal, {
      showModal(modalDialog(
        title = div(tags$h3("Access DHIS2 directly", style = heading_style)),
        textInput(ns("his_user"), label = "Enter your DHIS2 username", value = "", placeholder = "your_username", width = "100%"),
        passwordInput(ns("his_pass"), "Enter your DHIS2 password", value = "", width = "100%", placeholder = "your_password"),
        actionButton(ns("click_here_to_login_to_dhis2"), "Click here to login", class = "btn-primary", style = "width: 100%;"),
        easyClose = TRUE, size = "m", footer = NULL
      ))
    })

    # login a user to DHIS
    observeEvent(input$click_here_to_login_to_dhis2, {
      username <- input$his_user
      password <- input$his_pass
      req(username)
      req(password)

      login_successful <- login_to_dhis2_within_shiny(base_url, username, password)
      removeModal()

      if (login_successful) {
        updateActionButton(session, "click_here_to_show_khis_login_modal", label = "Already logged in! 🚀🚀")
        disable("click_here_to_show_khis_login_modal")
        enable("click_here_to_extract_selected_data_from_khis")

        con <- Dhis2r$new(base_url, username, password)
        dhis_connection(con)
      }
    })


    observeEvent(input$click_here_to_extract_selected_data_from_khis, {
      # format date range from user interface
      start_date <- input$his_date_range[1]
      end_date <- input$his_date_range[2]

      periods_vector <- seq(from = as.Date(start_date), to = as.Date(end_date), by = "month")
      period_formatted <- format(periods_vector, "%Y%m")

      withProgress(
        expr = {
          extraction_results <- extraction_data_from_dhis2(
            connection = dhis_connection(),
            consumption_ids = input$fp_consumption_data_ids,
            service_ids = input$fp_service_data_ids,
            org_ids = input$his_org_unit,
            period_range = period_formatted,
            output_scheme = input$his_output_scheme
          )

          if (!is.null(extraction_results)) {
            extraction_results <- extraction_results |>
              create_method_column()

            # standardize data elements names
            if (input$his_output_scheme == "NAME") {
              extraction_results <- extraction_results |> standardize_dhis_dx_names()
            }

            dhis_output(extraction_results)

            file_name <- glue("extraction_results-{today()}")
            output$extraction_results_export <- download_data_as_csv(extraction_results, file_name)

            output$extraction_results_table <- renderReactable({
              print("Rendering data.......")
              extraction_results |>
                render_data_with_reactable(
                  columns_to_format = generate_reactable_columns(extraction_results, "value")
                )
            })
          } else {
            output$extraction_results_table <- renderReactable({
              print("Rendering NULL.......")
              NULL
            })
          }
        }, min = 0, max = 10, value = 7, message = "Extracting...", detail = "Please wait"
      )
    })

    return(dhis_output)
  })
}
