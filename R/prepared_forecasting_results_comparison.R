pre_processed_forecasts_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_underline(
      title = "Foreacsting",
      full_screen = TRUE,
      div(
        style = "display: flex; gap: 10px; padding: 2px 20px 5px 20px;",
        div(style = "flex: 1;", pickerInput(
          ns("org_unit_for_service_consumption_comparison"),
          label = "Organization Unit",
          choices = "",
          multiple = FALSE,
          width = "100%",
          options = list(`live-search` = TRUE)
        )),
        div(style = "flex: 1;", pickerInput(ns("analytic_for_service_consumption_comparison"),
          label = "FP Product",
          choices = "",
          multiple = FALSE,
          width = "100%",
          options = list(`live-search` = TRUE)
        )),
        div(style = "flex: 1;", pickerInput(ns("forecasting_approach_for_service_consumption_comparison"),
          label = "Method",
          choices = "",
          multiple = TRUE,
          width = "100%",
          options = list(`live-search` = TRUE)
        )),
        div(style = "flex: 1;", dateRangeInput(
          ns("date_range_for_service_consumption_comparison"),
          "Date range:",
          start = NULL,
          end = NULL,
          min = NULL,
          max = NULL,
          format = "mm/dd/yy",
          separator = " - ",
          width = "100%"
        )),
        div(style = "flex: 1; margin-top: 30px", downloadButton(ns("download_forecasted_data"), "Download Forecast", class = "btn-primary", style = "width: 100%;")),
      ),
      nav_panel(
        "",
        echarts4rOutput(ns("forecast_plot"), height = "550px"),
        div(
          style = "display: flex; gap: 10px; align-items: center;",
          div(p(strong("Models Used"))),
          div(textOutput(ns("model_used_for_consumption"))),
          div(textOutput(ns("model_used_for_service"))),
          div(textOutput(ns("text_for_converted_service_products")))
        )
      ),
    )
  )
}

pre_processed_forecasts_page_server <- function(id, data_to_plot) {
  moduleServer(id, function(input, output, session) {
    observe({
      data_to_plot
      update_ui_elements(session, data_to_plot, use_both_approaches = TRUE)
    })

    filtered_data <- reactive({
      data_to_plot |>
        filter(
          org_unit == input$org_unit_for_service_consumption_comparison,
          analytic == input$analytic_for_service_consumption_comparison,
          .index |> between(input$date_range_for_service_consumption_comparison[1], input$date_range_for_service_consumption_comparison[2]),
          method %in% c(input$forecasting_approach_for_service_consumption_comparison)
        )
    })

    output$forecast_plot <- renderEcharts4r({
      plot_data <- filtered_data() |>
        unite(col = ".method_key", method, .key, sep = " - ") %>%
        group_by(.method_key)


      plot_data %>%
        e_charts_(".index") %>%
        e_line_(".value", smooth = TRUE, draw = FALSE) %>%
        e_color(c("#FF0000", "#8B0000", "#003153", "#87CEEB")) %>%
        e_axis_labels(x = "Date", y = "Value") %>%
        e_title(text = "Looking Ahead: Forecasting Future Trend Based on Historical Trends") %>%
        e_theme("roma") %>%
        e_legend(right = 100) %>% # move legend to the right
        e_tooltip(trigger = "axis") %>%
        e_toolbox() %>%
        e_toolbox_feature(
          feature = "dataZoom"
        ) %>%
        e_toolbox_feature(
          feature = "saveAsImage"
        )
    })

    observe({
        file_name <- glue("{input$analytic_for_service_consumption_comparison}_forecast_for_{input$org_unit_for_service_consumption_comparison}")

        output$download_forecasted_data <- filtered_data() %>%
          filter(.key == "prediction") %>%
          select(-c(".model_id",	".model_desc",	".key", ".index")) %>%
          rename(forecasted_value = .value) %>%
          relocate(period, .after = org_unit) %>%
          download_data_as_csv(file_name)
    })

    output$model_used_for_consumption <- renderText({
      glue("Consumption: {filtered_data() |> retrieve_model_info('Consumption')}")
    })
    output$model_used_for_service <- renderText({
      glue("Service: {filtered_data() |> retrieve_model_info('Service')}")
    })

    observe({
      input$analytic_for_service_consumption_comparison

      generate_text_for_converted_service_products(input, output)
    })
  })
}
