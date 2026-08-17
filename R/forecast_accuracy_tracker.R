# UI function

forecast_accuracy_tracker_ui <- function(id) {
  ns <- NS(id)

  # UI elements
  tagList(
    div(
      style = "display: flex; gap: 10px; padding: 5px 20px 5px 20px;",
      div(style = "flex: 1;", pickerInput(
        ns("analytics_to_analyze"),
        label = "Choose Product",
        choices = distinct_analytics,
        selected = "Jadelle",
        multiple = FALSE,
        width = "100%",
        options = list(`live-search` = TRUE),
      )),
      div(style = "flex: 1;", pickerInput(
        ns("forecast_series_type"),
        label = "Choose Forecast Type",
        choices = c("Main", "Review"),
        selected = "Main",
        multiple = FALSE,
        width = "100%",
        options = list(`live-search` = TRUE),
      )),
      div(style = "flex: 1;", pickerInput(
        ns("fy_year"),
        label = "Choose Year",
        choices = financial_years,
        selected = financial_years[1],
        multiple = TRUE,
        width = "100%",
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format`= "count"
          )
      )),
      div(style = "flex: 1;", pickerInput(
        ns("month"),
        label = "Choose Month",
        choices = month.name,
        selected = month.name,
        multiple = TRUE,
        width = "100%",
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format`= "count"
          )
      ))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(textOutput(ns("card_header_text"))),
        card_body(
          apexchartOutput(ns("forecast_accuracy_plot")),
        )
      ),
      card(
        card_header("Metrics"),
        card_body(
          gt_output(ns("forecast_accuracy_tbl"))
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

      output$card_header_text <- renderText({
        glue("Product - {input$analytics_to_analyze}")
      })
    })

    filtered_df <- reactive({
      filtered_dates <- get_fy_month_dates(c(input$fy_year))

      fa_df_clean %>%
        filter(
          product == input$analytics_to_analyze,
          forecast_type == input$forecast_series_type,
          period %in% filtered_dates,
          month(period, label = TRUE, abbr = FALSE) %in% c(input$month)
        ) %>%
        arrange(period)
    })

    output$forecast_accuracy_plot <- renderApexchart({
      tryCatch(
        expr = {
          df <- filtered_df()
          print(unique(df$method))
          print(levels(df$method))
          generate_forecast_accuracy_chart(df)
        },
        error = function(e) {
          NULL
        }
      )
    })

    output$forecast_accuracy_tbl <- render_gt({
      tryCatch(
        expr = {
          metrics <- calculate_forecast_accuracy_all_methods(filtered_df())
          make_forecast_accuracy_gt_table(metrics)
        },
        error = function(e) {
          NULL
        }
      )
    })

  })
}
