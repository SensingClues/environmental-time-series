
source("src/scenario_analysis.R")

server <- function(input, output, session) {
  # error logging
  message("=========== Starting Environmental Time Series Analysis App =============")
  
  # js code to get the browser language
  runjs(js_lang)
  
  language <- get_sys_language(Sys.info()['sysname']) # gives a long and a short version, e.g. "English" and "en"
  lang_short <- language[["lang_short"]] # language according to system (short="en" instead of "English")
  lang_long <- language[["lang_long"]] # language according to system (long="English" instead of "en")

  observeEvent(input$browser_language, {
    # let user choose language, pre-filled = browser language, unless this language is
    # not (yet) supported, then default is "en" (English)
    session$userData$inp_lang <- substr(input$browser_language,1,2)
    
    session$userData$sel_lang <- ifelse(session$userData$inp_lang %in% languages,
                                        session$userData$inp_lang, "en")
    
    # set language to browser language (or "en") and get the appropriate json translation file
    # path <- paste0(session$userData$url_translation, sel_lang)
    # i18n <- Translator$new(translation_json_path = path)
    i18n$set_translation_language(session$userData$sel_lang)
    
    message('browserlanguage is: ',session$userData$inp_lang)
    message('chosen language is: ',session$userData$sel_lang)
  })
  
  #####################################################################################################
  #####################################################################################################
  # ---------------------------------------------------------------------------------------------------
  # GENERAL & SIDEBAR
  # ---------------------------------------------------------------------------------------------------
  
  # Create selector choice sets based on the selected tab (NDVI or BA Explorer)
  countrychoices_rv <- reactiveValues(
    choice_set =   list(NDVIexplorerTab = c("Mponda, Zambia" = "Zambia_Mponda", "West Lunga, Zambia" = "Zambia_WL",
                                            "Ancares Courel, Spain" = "Spain",
                                            "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya"),
                        BAexplorerTab = c("West Lunga, Zambia" = "Zambia_WL", "Mponda, Zambia" = "Zambia_Mponda"),
                        ScenarioExplorerTab = c("Mponda, Zambia" = "Zambia_Mponda", "West Lunga, Zambia" = "Zambia_WL",
                                                "Ancares Courel, Spain" = "Spain",
                                                "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya")),
    selected_set = list(NDVIexplorerTab = "Zambia_Mponda", 
                        BAexplorerTab = "Zambia_WL",
                        ScenarioExplorerTab = "Zambia_Mponda")
  )
  
  resolutionchoices_rv <- reactiveValues(
    choice_set =   list(NDVIexplorerTab = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                            "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100"),
                        BAexplorerTab = c("500 (Terra MODIS)" = "500"),
                        ScenarioExplorerTab = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                                "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100")),
    selected_set = list(NDVIexplorerTab = "Sentinel_1000", 
                        BAexplorerTab = "500",
                        ScenarioExplorerTab = "Sentinel_1000")
  )

  selected_available_year <- function(years, current_value = NULL) {
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    if (length(years) == 0L) {
      return(character(0))
    }

    current_year <- suppressWarnings(as.integer(current_value))
    if (length(current_year) == 1L && !is.na(current_year) && current_year %in% years) {
      return(as.character(current_year))
    }

    as.character(max(years))
  }

  sidebar_data_type <- reactive({
    if (identical(input$tabs, "BAexplorerTab")) "BurnedArea" else "NDVI"
  })

  available_year_end <- function(data_type, country_name, resolution, year) {
    dates_df <- get_available_dates(data_dir, data_type, country_name, resolution)
    year <- suppressWarnings(as.integer(year))
    months <- dates_df$month[dates_df$year == year]
    if (length(months) == 0L) {
      stop("No ", data_type, " data found for ", country_name, " in ", year, ".")
    }

    list(end_year = year, end_month = max(months))
  }

  selected_country_label <- function(country_value = input$country, tab_value = input$tabs) {
    choices <- countrychoices_rv$choice_set[[tab_value]]
    label <- names(choices)[match(country_value, unname(choices))]
    if (length(label) == 0L || is.na(label)) country_value else label
  }

  selected_resolution_label <- function(resolution_value = input$resolution, tab_value = input$tabs) {
    choices <- resolutionchoices_rv$choice_set[[tab_value]]
    label <- names(choices)[match(resolution_value, unname(choices))]
    if (length(label) == 0L || is.na(label)) resolution_value else label
  }

  ndvi_source_label <- function(resolution_value = input$resolution) {
    if (grepl("MODIS|250|500", resolution_value, ignore.case = TRUE)) {
      "MODIS"
    } else if (grepl("Sentinel|100", resolution_value, ignore.case = TRUE)) {
      "Sentinel-2"
    } else {
      "selected data source"
    }
  }

  ndvi_year_range_label <- function(country_name = input$country, resolution = input$resolution) {
    years <- get_available_years(data_dir, "NDVI", country_name, resolution)
    if (length(years) == 0L) {
      return("")
    }
    if (min(years) == max(years)) as.character(min(years)) else paste0(min(years), "-", max(years))
  }

  friendly_ndvi_no_data_message <- function(year = input$year, resolution = input$resolution,
                                            country = input$country) {
    paste0(
      "No data available for ", year, " at ",
      selected_resolution_label(resolution, "NDVIexplorerTab"),
      " for ", selected_country_label(country, "NDVIexplorerTab"),
      ". Please try selecting a different year or resolution."
    )
  }

  ndvi_error_ui <- function(message) {
    if (is.null(message) || !nzchar(message)) return(NULL)
    div(class = "ndvi-error-message", message)
  }
  
  # Observe selected tab and update choices according to the one selected
  observeEvent(input$tabs, { 
    updateSelectInput(session, "country",
                      choices = countrychoices_rv$choice_set[[input$tabs]], 
                      selected = countrychoices_rv$selected_set[[input$tabs]])
    updateSelectInput(session, "resolution",
                      choices = resolutionchoices_rv$choice_set[[input$tabs]], 
                      selected = resolutionchoices_rv$selected_set[[input$tabs]])
  })

  observeEvent(list(input$tabs, input$country, input$resolution), {
    req(input$tabs, input$country, input$resolution)

    years <- get_available_years(
      data_dir      = data_dir,
      data_type     = sidebar_data_type(),
      country_name  = input$country,
      resolution    = input$resolution
    )

    updateSelectInput(
      session,
      "year",
      choices  = as.character(years),
      selected = selected_available_year(years, input$year)
    )
  }, ignoreInit = FALSE)

  observeEvent(list(input$tabs, input$country, input$resolution), {
    req(input$tabs, input$country, input$resolution)

    years <- get_available_years(
      data_dir      = data_dir,
      data_type     = "NDVI",
      country_name  = input$country,
      resolution    = input$resolution
    )
    if (length(years) == 0L) {
      return()
    }

    year_choices <- stats::setNames(as.character(years), as.character(years))
    anomaly_year <- selected_available_year(years, input$scenario_anomaly_year)

    compare_year <- input$scenario_productivity_compare_year
    if (is.null(compare_year) || !compare_year %in% as.character(years)) {
      compare_year <- ""
    }

    updateSelectInput(session, "scenario_productivity_compare_year",
                      choices = c("None" = "", year_choices),
                      selected = compare_year)
    updateSelectInput(session, "scenario_anomaly_year",
                      choices = year_choices,
                      selected = anomaly_year)
  }, ignoreInit = FALSE)
  
  # Enable/disable "Month" selector based on the tab
  observeEvent(c(input$tabs, input$basubtabs, input$ndvisubtabs), {
    if (input$tabs == "NDVIexplorerTab" & input$ndvisubtabs == "NDVIdeltaTab") {
      shinyjs::enable("month")
    } else if (input$tabs == "BAexplorerTab" & input$basubtabs == "BAmapexplorer") {
      shinyjs::enable("month")
    } else {
      shinyjs::disable("month") 
    }
  })
  
  # ---------------------------------------------------------------------------------------------------
  # SCENARIO EXPLORER: shared data reactive
  # Loads all NDVI-per-class data once per (country, resolution) combination.
  # Invalidated automatically when input$country or input$resolution changes.
  # Each generate handler reads this reactive instead of re-running raster I/O.
  # ---------------------------------------------------------------------------------------------------
  scenario_ndvi_data <- reactive({
    req(input$country, input$resolution)
    country_name <- input$country
    resolution   <- input$resolution

    # HOT PATH: the /ndvi/by-landcover endpoint is single-year, but the scenario
    # plots need every available year. Fetch one year at a time and bind. Only
    # use the API result if EVERY expected year came back (same coverage guard as
    # the warm path below); otherwise fall through silently. The first call fails
    # fast when the API is down (connection refused), so fallback stays quick.
    sr <- get_sensor_resolution(resolution)
    api_years <- tryCatch(
      .scenario_avail_years(data_dir, country_name, resolution),
      error = function(e) integer(0)
    )
    if (!is.null(sr) && length(api_years) > 0) {
      # Stop at the first failed year so a down API costs one timeout, not one
      # per year. Need every year for the scenario plots, so any miss = fall back.
      api_parts <- vector("list", length(api_years))
      api_ok    <- TRUE
      for (i in seq_along(api_years)) {
        d <- try_api_call(
          endpoint = "/api/v1/ndvi/by-landcover",
          params   = list(aoi = country_name, sensor = sr$sensor,
                          resolution = sr$resolution, year = api_years[i])
        )
        if (is.null(d) || !all(c("year", "month", "land_cover", "mean_ndvi") %in% names(d))) {
          api_ok <- FALSE
          break
        }
        api_parts[[i]] <- d[, c("year", "month", "land_cover", "mean_ndvi")]
      }
      if (api_ok) {
        df <- do.call(rbind, api_parts)
        df$year  <- as.integer(df$year)
        df$month <- as.integer(df$month)
        df <- .drop_empty_lc_classes(df)
        if (nrow(df) > 0) {
          message("=== scenario_ndvi_data: hot path (API), years=", length(api_years),
                  " rows=", nrow(df))
          attr(df, "data_source") <- "api"
          return(df)
        }
      }
    }

    # WARM PATH: read pre-processed ndvi_monthly_by_class.parquet
    pq_path <- get_parquet_path(country_name, resolution, "ndvi_monthly_by_class")
    pq      <- try_read_parquet(pq_path)
    if (!is.null(pq)) {
      df <- pq[pq$aoi == country_name, c("year", "month", "land_cover", "mean_ndvi")]
      if (nrow(df) > 0) {
        # Only use parquet if it covers ALL years available from TIF files.
        # The anomaly/productivity selectors are populated from TIF years; if a
        # selected year is absent from the parquet the plot functions throw errors.
        tif_years <- tryCatch(
          .scenario_avail_years(data_dir, country_name, resolution),
          error = function(e) integer(0)
        )
        pq_years <- unique(as.integer(df$year))
        if (length(tif_years) > 0 && all(tif_years %in% pq_years)) {
          df$year  <- as.integer(df$year)
          df$month <- as.integer(df$month)
          df <- .drop_empty_lc_classes(df)
          message("=== scenario_ndvi_data: warm path (parquet), rows=", nrow(df))
          attr(df, "data_source") <- "parquet"
          return(df)
        }
        message("=== scenario_ndvi_data: parquet missing TIF years (",
                paste(setdiff(tif_years, pq_years), collapse = ", "),
                "), falling to cold path")
      }
    }

    # COLD PATH (unchanged)
    land_use_src <- "S2_10m_LULC_2023"
    lulc_dir     <- file.path(data_dir, "LandUse", country_name, land_use_src)
    message("=== scenario_ndvi_data: cold path, country=", country_name, " resolution=", resolution)
    message("    lulc_dir exists: ", dir.exists(lulc_dir), " | path: ", lulc_dir)
    avail_years  <- .scenario_avail_years(data_dir, country_name, resolution)
    message("    avail_years: ", paste(avail_years, collapse = ", "))
    df <- .load_all_years(avail_years, country_name, resolution, data_dir, lulc_dir)
    message("    loaded rows: ", if (is.null(df)) "NULL" else nrow(df))
    .drop_empty_lc_classes(df)
  })

  # ---------------------------------------------------------------------------------------------------
  # SCENARIO EXPLORER: LAND COVER PRODUCTIVITY
  # ---------------------------------------------------------------------------------------------------
  scenario_productivity_ready  <- reactiveVal(FALSE)
  scenario_productivity_result <- reactiveVal(NULL)

  output$scenario_productivity_bar_output <- plotly::renderPlotly({
    res <- scenario_productivity_result(); shiny::req(res); res$bar
  })
  output$scenario_productivity_scatter_output <- plotly::renderPlotly({
    res <- scenario_productivity_result(); shiny::req(res); res$scatter
  })
  output$scenario_productivity_table_output <- renderTable({
    res <- scenario_productivity_result(); shiny::req(res); res$table
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     sanitize.colnames.function = identity)

  output$scenario_productivity_container <- renderUI({
    if (is.null(error_message_rv()) && isTRUE(scenario_productivity_ready())) {
      res <- scenario_productivity_result()
      shiny::req(res)
      tagList(
        div(
          style = paste0(
            "background: #F1F8E922; border-left: 4px solid #558B2F;",
            "padding: 12px; border-radius: 4px; margin-bottom: 12px;"
          ),
          tags$strong("Key Insight"),
          tags$br(),
          tags$span(res$insight_text)
        ),
        fluidRow(
          column(6, plotlyOutput("scenario_productivity_bar_output",     height = "400px")),
          column(6, plotlyOutput("scenario_productivity_scatter_output", height = "400px"))
        ),
        br(),
        tableOutput("scenario_productivity_table_output")
      )
    } else NULL
  })

  observeEvent(list(input$tabs, input$scenariosubtabs), {
    scenario_productivity_ready(FALSE)
    scenario_productivity_result(NULL)
    error_message_rv(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$generate_productivity, {
    scenario_productivity_ready(FALSE)
    scenario_productivity_result(NULL)
    error_message_rv(NULL)

    sel_year    <- as.integer(input$year)
    cmp_year    <- input$scenario_productivity_compare_year

    tryCatch({
      scenario_df <- scenario_ndvi_data()
      data_source_rv(if (!is.null(attr(scenario_df, "data_source"))) attr(scenario_df, "data_source") else "tif")
      res <- plot_productivity_comparison(
        df           = scenario_df,
        selected_year = sel_year,
        compare_year  = if (nzchar(cmp_year)) cmp_year else NULL
      )
      scenario_productivity_result(res)
      scenario_productivity_ready(TRUE)
      error_message_rv(NULL)
    }, error = function(e) {
      scenario_productivity_ready(FALSE)
      scenario_productivity_result(NULL)
      error_message_rv(e$message)
      showNotification(HTML("The figure cannot be generated due to missing data.
       Please contact us at
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."),
                       type = "error", duration = 6)
      message("Error generating Land Cover Productivity: ", e$message)
    })
  })

  # ---------------------------------------------------------------------------------------------------
  # SCENARIO EXPLORER: AGRICULTURAL MONITORING
  # ---------------------------------------------------------------------------------------------------
  scenario_agri_ready  <- reactiveVal(FALSE)
  scenario_agri_result <- reactiveVal(NULL)

  output$scenario_agri_plot_output <- plotly::renderPlotly({
    res <- scenario_agri_result(); shiny::req(res); res$plot
  })
  output$scenario_agri_table_output <- renderTable({
    res <- scenario_agri_result(); shiny::req(res); res$phenology_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     sanitize.colnames.function = identity)

  output$scenario_agri_container <- renderUI({
    if (is.null(error_message_rv()) && isTRUE(scenario_agri_ready())) {
      res <- scenario_agri_result()
      tagList(
        div(style = "background: #F9FBE722; border-left: 4px solid #558B2F; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
          tags$strong("Season Performance"), tags$br(),
          tags$span(res$insight_text)
        ),
        plotlyOutput("scenario_agri_plot_output", height = "500px"),
        br(),
        tableOutput("scenario_agri_table_output"),
        p(style = "color:#888; font-size:0.85em; margin-top:6px;",
          "— = event not detected. Solid markers in the chart indicate certain detections; outlined markers indicate uncertain ones."
        )
      )
    } else NULL
  })

  observeEvent(list(input$tabs, input$scenariosubtabs), {
    scenario_agri_ready(FALSE)
    scenario_agri_result(NULL)
    error_message_rv(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$agri_class, {
    scenario_agri_ready(FALSE)
    scenario_agri_result(NULL)
    data_source_rv("none")
  }, ignoreInit = TRUE)

  output$agri_callout <- renderUI({
    cls <- if (!is.null(input$agri_class)) input$agri_class else "Crops"
    desc <- switch(cls,
      Crops     = "Coloured lines show crop NDVI across months for each year. Markers show detected phenological events:",
      Rangeland = "Coloured lines show rangeland NDVI across months for each year. Markers show detected vegetation response events:",
      paste("Coloured lines show", cls, "NDVI across months for each year. Markers show detected phenological events:")
    )
    gu_desc  <- switch(cls,
      Crops     = " — when rains trigger initial crop growth",
      Rangeland = " — when vegetation responds to onset of rainfall",
      " — vegetation green-up onset"
    )
    pk_desc  <- switch(cls,
      Crops     = " — when the crop reaches maximum health",
      Rangeland = " — when vegetation reaches peak greenness",
      " — peak vegetation greenness"
    )
    sen_desc <- switch(cls,
      Crops     = " — when the crop matures or dries down",
      Rangeland = " — when vegetation declines as dry season sets in",
      " — vegetation senescence"
    )
    div(style = paste0("background:#f1f8e9; border-left:4px solid #558B2F;",
                       "border-radius:4px; padding:12px 16px; margin-bottom:14px;"),
        p(style = "margin:0 0 6px 0; font-weight:600; font-size:0.93em;", "How to read this chart"),
        p(style = "margin:0 0 4px 0; font-size:0.91em;", desc),
        tags$ul(style = "margin:2px 0 6px 0; padding-left:18px; font-size:0.91em;",
          tags$li(tags$strong("▲ Green-up"), gu_desc),
          tags$li(tags$strong("★ Peak"),     pk_desc),
          tags$li(tags$strong("▼ Senescence"), sen_desc)
        ),
        p(style = "margin:0; font-size:0.91em;",
          "Solid markers = high confidence. Outlined markers = medium/low confidence.")
    )
  })

  observeEvent(input$generate_agri_monitoring, {
    scenario_agri_ready(FALSE)
    scenario_agri_result(NULL)
    error_message_rv(NULL)

    tryCatch({
      scenario_df <- scenario_ndvi_data()
      data_source_rv(if (!is.null(attr(scenario_df, "data_source"))) attr(scenario_df, "data_source") else "tif")
      res <- plot_agricultural_monitoring(df = scenario_df, selected_class = input$agri_class)
      scenario_agri_result(res)
      scenario_agri_ready(TRUE)
      error_message_rv(NULL)
    }, error = function(e) {
      scenario_agri_ready(FALSE)
      scenario_agri_result(NULL)
      error_message_rv(e$message)
      showNotification(HTML("The figure cannot be generated due to missing data.
       Please contact us at
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."),
                       type = "error", duration = 6)
      message("Error generating Agricultural Monitoring: ", e$message)
    })
  })


  # ---------------------------------------------------------------------------------------------------
  # SCENARIO EXPLORER: ANOMALY RESILIENCE
  # ---------------------------------------------------------------------------------------------------
  scenario_anomaly_ready  <- reactiveVal(FALSE)
  scenario_anomaly_result <- reactiveVal(NULL)

  output$scenario_anomaly_heatmap_output <- plotly::renderPlotly({
    res <- scenario_anomaly_result(); shiny::req(res); res$heatmap
  })
  output$scenario_anomaly_recovery_output <- plotly::renderPlotly({
    res <- scenario_anomaly_result(); shiny::req(res); res$recovery
  })
  output$scenario_anomaly_table_output <- renderTable({
    res <- scenario_anomaly_result(); shiny::req(res); res$summary_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE,
     sanitize.text.function = identity, sanitize.colnames.function = identity)

  output$scenario_anomaly_container <- renderUI({
    if (is.null(error_message_rv()) && isTRUE(scenario_anomaly_ready())) {
      res <- scenario_anomaly_result()

      callout_box <- div(
        style = "background:#E8F5E9; border-left:4px solid #1D9E75; border-radius:4px; padding:12px 16px; margin-bottom:14px;",
        p(style = "margin:0 0 6px 0; font-weight:600; font-size:0.93em;", "How to read this chart"),
        p(style = "margin:0 0 4px 0; font-size:0.91em;",
          "This chart shows how quickly land cover classes bounce back from stress. An ",
          tags$strong("anomaly"), " is when NDVI drops below the typical range for that month. ",
          tags$strong("Recovery"), " means NDVI returns to within normal range (within 1 standard deviation of the historical average)."
        ),
        p(style = "margin:0 0 4px 0; font-size:0.91em;",
          "The heatmap shows which classes suffered most (red = largest deficit) and when they recovered (green = back to normal)."
        ),
        p(style = "margin:0 0 4px 0; font-size:0.91em;",
          "The bar chart ranks classes by recovery time. ",
          tags$strong("Faster recovery = more resilient. Slower recovery = more vulnerable.")
        ),
        p(style = "margin:0; font-size:0.91em; color:#E65100; font-weight:600;",
          "⚠️ Flooded vegetation anomalies reflect flood dynamics (water levels), not drought stress — interpret it differently."
        )
      )

      ranking_card <- if (!is.null(res$ranking_card)) {
        rc <- res$ranking_card
        div(
          style = "background:#EDE7F6; border-left:4px solid #6A1B9A; padding:12px; margin-bottom:12px; border-radius:4px;",
          tags$strong("Resilience Ranking"), tags$br(),
          p(style = "margin:6px 0 2px 0; font-size:0.93em;",
            tags$span(style = "color:#2E7D32; font-weight:600;", "✅ "), rc$resilient),
          p(style = "margin:2px 0; font-size:0.93em;",
            tags$span(style = "color:#C62828; font-weight:600;", "🔴 "), rc$vulnerable),
          if (nzchar(rc$other))
            p(style = "margin:2px 0; font-size:0.91em; color:#333;", rc$other) else NULL,
          if (nzchar(rc$flood_note))
            p(style = "margin:6px 0 0 0; font-size:0.91em; color:#E65100;", rc$flood_note) else NULL
        )
      } else NULL

      tagList(
        callout_box,
        div(style = "background: #FCE4EC22; border-left: 4px solid #C62828; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
          tags$strong("Resilience Insight"), tags$br(),
          tags$span(res$insight_text)
        ),
        ranking_card,
        fluidRow(
          column(8, plotlyOutput("scenario_anomaly_heatmap_output",  height = "400px")),
          column(4, plotlyOutput("scenario_anomaly_recovery_output", height = "400px"))
        ),
        br(),
        tableOutput("scenario_anomaly_table_output"),
        p(style = "color:#888; font-size:0.85em; margin-top:6px;",
          "— = no recovery detected within the selected year. This usually means stress peaked late in the year (e.g. December), leaving no months remaining to observe a rebound.")
      )
    } else NULL
  })

  observeEvent(list(input$tabs, input$scenariosubtabs), {
    scenario_anomaly_ready(FALSE)
    scenario_anomaly_result(NULL)
    error_message_rv(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$generate_anomaly_resilience, {
    scenario_anomaly_ready(FALSE)
    scenario_anomaly_result(NULL)
    error_message_rv(NULL)

    anom_year <- as.integer(input$scenario_anomaly_year)

    tryCatch({
      scenario_df <- scenario_ndvi_data()
      data_source_rv(if (!is.null(attr(scenario_df, "data_source"))) attr(scenario_df, "data_source") else "tif")
      res <- plot_anomaly_resilience(
        df           = scenario_df,
        anomaly_year = anom_year
      )
      scenario_anomaly_result(res)
      scenario_anomaly_ready(TRUE)
      error_message_rv(NULL)
    }, error = function(e) {
      scenario_anomaly_ready(FALSE)
      scenario_anomaly_result(NULL)
      error_message_rv(e$message)
      showNotification(HTML("The figure cannot be generated due to missing data.
       Please contact us at
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."),
                       type = "error", duration = 6)
      message("Error generating Anomaly Resilience: ", e$message)
    })
  })

  # Set Reactive values for AoI shape and error message
  aoi_shape_rv <- reactiveVal(NULL)
  error_message_rv <- reactiveVal(NULL)
  # "none" => no plot generated yet, so the label renders blank (switch default).
  data_source_rv <- reactiveVal("none")

  # Each chart area gets its own output ID so they can update independently.
  # All read from the same data_source_rv() but must have unique HTML IDs.
  .ds_label_ids <- c(
    "ds_label_ndvi_ts", "ds_label_lc", "ds_label_ndvi_delta",
    "ds_label_ba_seasonal", "ds_label_ba_daily", "ds_label_ba_map",
    "ds_label_productivity", "ds_label_anomaly", "ds_label_agri"
  )
  for (.lid in .ds_label_ids) {
    local({
      lid <- .lid
      output[[lid]] <- renderText({
        switch(data_source_rv(),
          "parquet" = "Data: pre-processed (fast)",
          "tif"     = "Data: computed from raw files",
          "api"     = "Data: live API (fast)",
          ""
        )
      })
    })
  }

  # Blank the label on every tab / sub-tab navigation (no plot shown yet).
  observeEvent(
    list(input$tabs, input$ndvisubtabs, input$basubtabs, input$scenariosubtabs),
    { data_source_rv("none") },
    ignoreInit = TRUE
  )
  
  # Update month selector based on the months available for the selected year.
  observeEvent(list(input$tabs, input$country, input$resolution, input$year), {
    req(input$tabs, input$country, input$resolution, input$year)

    months <- get_available_month_names(
      data_dir      = data_dir,
      data_type     = sidebar_data_type(),
      country_name  = input$country,
      resolution    = input$resolution,
      year          = input$year
    )

    if (length(months) == 0L) {
      months <- month.name
    }

    selected_month <- if (!is.null(input$month) && input$month %in% months) {
      input$month
    } else {
      months[[1]]
    }

    updateSelectInput(session, "month",
                      choices = months,
                      selected = selected_month)
  }, ignoreInit = FALSE)
  
  # Create Leaflet map with AoI selected
  observeEvent(input$country, {
    req(input$country)
    error_message_rv(NULL) # Clear any previous errors

    tryCatch({
      # Use only the base country name for file lookup (e.g. "Zambia_Mponda" -> "Zambia")
      aoi_country_key <- sub("_.*", "", input$country)
      aoi_files <- list.files(file.path(data_dir, "AoI"), pattern = paste0("AoI_.*", aoi_country_key, ".*\\.geojson$"))
      if (length(aoi_files) == 0) {
        stop("No Area of Interest file found for the selected country.")
      }
      shape <- read_aoi_geometry(file.path(data_dir, "AoI", aoi_files[[1]]), input$country)
      shape <- sf::st_transform(shape, crs = 4326)
      
      aoi_shape_rv(shape) # Update the reactive value with the loaded shape
      error_message_rv(NULL) # Clear any previous error messages
      
    }, error = function(e) {
      error_message_rv(e$message) 
      aoi_shape_rv(NULL) # Set to NULL on error
      showNotification(HTML("The project area cannot be loaded due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
    })
  }, ignoreNULL = TRUE, ignoreInit = FALSE) # 'ignoreInit = FALSE' makes it run on startup
  
  # Return empty world map if no shape, otherwise create map with selected AoI
  output$map <- renderLeaflet({
    shape <- aoi_shape_rv()
    if (is.null(shape)) { 
      return(leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
               addTiles()) # Return empty world map if no shape
    } 
    
    bounds <- sf::st_bbox(shape)
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      addPolygons(data = shape, color = "green", opacity = 0.5, fillOpacity = 0.2, weight = 2) %>%
      fitBounds(bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]])
  })
  
  
  # ---------------------------------------------------------------------------------------------------
  # NDVI TIME SERIES CHART
  # ---------------------------------------------------------------------------------------------------
  
  # Reactive flag to control whether the NDVI timeseries UI should be shown
  ndvi_ts_ready <- reactiveVal(FALSE)
  # Plotly figure: set after successful generate (works with conditional UI + renderPlotly).
  ndvi_ts_plot_obj <- reactiveVal(NULL)
  ndvi_ts_stats    <- reactiveVal(NULL)
  ndvi_ts_view_rv  <- reactiveVal("monthly")
  
  output$ndvi_ts_plot_output <- plotly::renderPlotly({
    p <- ndvi_ts_plot_obj()
    shiny::req(p)
    p
  })
  
  # Render a container for the plot or error message
  output$ndvi_ts_plot_container <- renderUI({
    error_msg <- error_message_rv()
    
    if (is.null(error_msg) && isTRUE(ndvi_ts_ready())) { # If no errors and the reactive flag is TRUE (after successful figure generation), show output, otherwise empty
      div(
        class = "image-fill top-center ndvi-ts-plot-stack",
        ndvi_anomaly_titles_ui(input$resolution, land_cover_class = input$ndvi_ts_lc_class,
                               view = ndvi_ts_view_rv()),
        plotlyOutput("ndvi_ts_plot_output", height = "550px"),
        height = "auto"
      )
    } else {
      ndvi_error_ui(error_msg)
    }
  })
  
  output$wilcoxon_card <- renderUI({
    s <- ndvi_ts_stats()
    if (is.null(s) || !isTRUE(ndvi_ts_ready())) return(NULL)
    if (!identical(s$view, "monthly")) return(NULL)
    ndvi_insight_wilcox_card_ui(s, land_cover_class = input$ndvi_ts_lc_class)
  })

  output$smk_card <- renderUI({
    s <- ndvi_ts_stats()
    if (is.null(s) || !isTRUE(ndvi_ts_ready())) return(NULL)
    if (!identical(s$view, "annual")) return(NULL)
    ndvi_annual_trend_card_ui(s, land_cover_class = input$ndvi_ts_lc_class)
  })

  output$ndvi_data_source_guidance <- renderUI({
    req(input$country)
    s2_years  <- get_available_years(data_dir, "NDVI", input$country, "Sentinel_1000")
    mod_years <- get_available_years(data_dir, "NDVI", input$country, "MODIS_1000")

    s2_range  <- if (length(s2_years)  > 0) paste0(min(s2_years),  "–", max(s2_years))  else "N/A"
    mod_range <- if (length(mod_years) > 0) paste0(min(mod_years), "–", max(mod_years)) else "N/A"

    tags$table(
      style = "width:100%; border-collapse:collapse; font-size:0.93em;",
      tags$thead(
        tags$tr(
          tags$th(class = "source-callout row-odd", "Goal"),
          tags$th(class = "source-callout row-odd", "Recommended settings")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(class="source-callout row-even", paste0("Long-term trend analysis (", mod_range, ")")),
          tags$td(class="source-callout row-even", "MODIS, 1000m, maximum year range")
        ),
        tags$tr(
          tags$td(class = "source-callout row-odd", paste0("Recent vegetation monitoring (", s2_range, ")")),
          tags$td(class = "source-callout row-odd", "Sentinel-2, 100m or 1000m")
        ),
        tags$tr(
          tags$td(class="source-callout row-even", "Intervention monitoring (plot scale)"),
          tags$td(class="source-callout row-even", "Sentinel-2, 100m, narrow time window")
        ),
        tags$tr(
          tags$td(class = "source-callout row-odd", "Fire and burn area analysis"),
          tags$td(class = "source-callout row-odd", "MODIS, 500m, Burned Area Explorer")
        )
      )
    )
  })

  output$ndvi_ts_callout <- renderUI({
    view <- if (!is.null(input$ndvi_ts_view)) input$ndvi_ts_view else "monthly"
    text <- if (identical(view, "annual")) {
      "This chart shows annual mean NDVI per year. Grey band = typical range (middle 50% of years). Dotted lines = historical min/max. Red dashed line = statistically significant long-term trend. Orange circles = incomplete years, excluded from statistics."
    } else {
      "This chart shows average monthly vegetation health for the selected year compared to previous years. The shaded band shows the typical range. Use it to see whether this year's vegetation is better or worse than usual."
    }
    div(class = "ndvi-callout", p(text))
  })

  output$ndvi_health_summary_card <- renderUI({
    s <- ndvi_ts_stats()
    if (is.null(s) || !isTRUE(ndvi_ts_ready())) return(NULL)
    if (identical(s$view, "annual")) return(NULL)

    # Monthly view status is based entirely on Wilcoxon test:
    # selected year's monthly NDVI anomalies vs historical monthly average.
    wilcox_p   <- if (!is.null(s$wilcox_p))      s$wilcox_p      else NA_real_
    wilcox_med <- if (!is.null(s$wilcox_median))  s$wilcox_median else NA_real_

    status <- if (is.na(wilcox_p)) {
      "Insufficient data"
    } else if (wilcox_p < 0.05 && !is.na(wilcox_med) && wilcox_med > 0) {
      "Above normal"
    } else if (wilcox_p < 0.05 && !is.na(wilcox_med) && wilcox_med < 0) {
      "Below normal"
    } else {
      "Within normal range"
    }

    status_color <- switch(status,
      "Above normal"       = "#009E73",
      "Below normal"       = "#D55E00",
      "Within normal range" = "#4CAF50",
      "#9E9E9E"
    )

    train_min <- if (!is.null(s$train_year_min)) s$train_year_min else NA_integer_
    train_max <- if (!is.null(s$train_year_max)) s$train_year_max else NA_integer_
    test_yr   <- if (!is.null(s$test_year))      s$test_year      else NA_integer_
    hist_label <- if (!is.na(train_min) && !is.na(train_max)) sprintf("%d–%d", train_min, train_max) else "historical"
    yr_label   <- if (!is.na(test_yr)) as.character(test_yr) else "this year"

    status_explanation <- switch(status,
      "Above normal"       = paste0("Monthly vegetation health in ", yr_label,
                                    " is significantly above the ", hist_label, " historical average."),
      "Below normal"       = paste0("Monthly vegetation health in ", yr_label,
                                    " is significantly below the ", hist_label, " historical average."),
      "Within normal range" = paste0("Monthly vegetation health in ", yr_label,
                                     " is within the expected range of the ", hist_label, " historical average."),
      "Not enough monthly data to compare against the historical baseline."
    )

    # Source label
    res_label <- dplyr::case_when(
      grepl("Sentinel", input$resolution) ~ "Sentinel-2",
      grepl("MODIS",    input$resolution) ~ "MODIS",
      TRUE                                ~ "satellite"
    )

    # Analysis period: what year is shown vs what baseline was used
    has_year_range <- !is.na(test_yr) && !is.na(train_min) && !is.na(train_max)
    analysis_period <- if (has_year_range) {
      sprintf("%d (%s), compared against %d–%d baseline (%d years)",
              test_yr, res_label, train_min, train_max, train_max - train_min + 1L)
    } else {
      sprintf("%s data", res_label)
    }

    # MODIS recommendation when Sentinel-2 is selected
    is_sentinel <- grepl("Sentinel", input$resolution, ignore.case = TRUE)
    recommendation <- if (is_sentinel) {
      modis_years <- get_available_years(data_dir, "NDVI", input$country, "MODIS_1000")
      if (length(modis_years) >= 2L) {
        sprintf("Switch to MODIS for a longer-term perspective covering %d–%d.",
                min(modis_years), max(modis_years))
      } else NULL
    } else NULL

    div(
      style = paste0(
        "background:", status_color, "22; border-left:4px solid ", status_color,
        "; padding:12px; margin-bottom:16px; border-radius:4px;"
      ),
      fluidRow(
        column(5,
          tags$strong(paste0("Vegetation status in ", yr_label, ":")), tags$br(),
          tags$span(
            style = paste0("font-size:1.2em; color:", status_color, "; font-weight:bold;"),
            tags$span(class = "ndvi-status-dot", style = paste0("background:", status_color, ";")),
            status
          ),
          tags$br(),
          tags$span(style = "font-size:0.85em; color:#444;", status_explanation),
          tags$br(),
          tags$span(
            style = "font-size:0.80em; color:#666; font-style:italic; margin-top:4px; display:inline-block;",
            if (!is.na(wilcox_p)) paste0("Wilcoxon test, p = ", format(round(wilcox_p, 3), nsmall = 3)) else "p-value: N/A"
          )
        ),
        column(7,
          tags$strong("Analysis period:"), tags$br(),
          tags$span(style = "font-size:0.92em;", analysis_period),
          if (!is.null(recommendation)) tagList(tags$br(), tags$em(style = "font-size:0.85em; color:#555;", recommendation)) else NULL
        )
      )
    )
  })

  output$ndvi_annual_summary_card <- renderUI({
    s <- ndvi_ts_stats()
    if (is.null(s) || !isTRUE(ndvi_ts_ready())) return(NULL)
    if (!identical(s$view, "annual")) return(NULL)

    p     <- s$mk_p
    slope <- s$mk_slope

    trend_status <- if (is.na(p) || is.na(slope)) {
      "Insufficient data"
    } else if (p < 0.05 && slope > 0) {
      "Improving"
    } else if (p < 0.05 && slope < 0) {
      "Declining"
    } else {
      "No significant trend"
    }

    trend_color <- switch(trend_status,
      "Improving"            = "#4CAF50",
      "Declining"            = "#F44336",
      "No significant trend" = "#9E9E9E",
      "#9E9E9E"
    )

    trend_explanation <- switch(trend_status,
      "Improving"            = "Annual vegetation health has been consistently increasing over the data record.",
      "Declining"            = "Annual vegetation health has been consistently declining — this may indicate long-term degradation.",
      "No significant trend" = "No consistent upward or downward trend detected across the data record.",
      "Not enough data to determine a trend direction."
    )

    slope_label <- if (!is.na(slope)) {
      sprintf("Sen's slope: %+.4f NDVI/year", slope)
    } else {
      "Slope: N/A"
    }

    years_avail <- get_available_years(data_dir, "NDVI", input$country, input$resolution)
    res_label   <- dplyr::case_when(
      grepl("Sentinel", input$resolution) ~ "Sentinel-2",
      grepl("MODIS",    input$resolution) ~ "MODIS",
      TRUE                                ~ "satellite"
    )
    coverage <- if (length(years_avail) > 0) {
      sprintf("Based on %s data from %d to %d (%d years)",
              res_label, min(years_avail), max(years_avail), length(years_avail))
    } else {
      "Based on available data"
    }

    is_sentinel <- grepl("Sentinel", input$resolution, ignore.case = TRUE)
    recommendation <- if (is_sentinel) {
      modis_years <- get_available_years(data_dir, "NDVI", input$country, "MODIS_1000")
      if (length(modis_years) >= 2L) {
        sprintf("Switch to MODIS for a longer-term perspective covering %d–%d.",
                min(modis_years), max(modis_years))
      } else NULL
    } else NULL

    div(
      style = paste0(
        "background:", trend_color, "22; border-left:4px solid ", trend_color,
        "; padding:12px; margin-bottom:16px; border-radius:4px;"
      ),
      fluidRow(
        column(4,
          tags$strong("Overall trend:"), tags$br(),
          tags$span(
            style = paste0("font-size:1.2em; color:", trend_color, "; font-weight:bold;"),
            tags$span(class = "ndvi-status-dot", style = paste0("background:", trend_color, ";")),
            trend_status
          ),
          tags$br(),
          tags$span(style = "font-size:0.92em; color:#333;", trend_explanation),
          tags$br(),
          tags$span(style = "font-size:0.85em; color:#666;", slope_label)
        ),
        column(8,
          tags$strong("Data coverage:"), tags$br(),
          tags$span(coverage),
          if (!is.null(recommendation)) tagList(tags$br(), tags$em(recommendation)) else NULL
        )
      )
    )
  })

  # Clear the image when switching tabs, subtabs, or land cover class
  observeEvent(list(input$tabs, input$ndvisubtabs), {
    ndvi_ts_ready(FALSE)
    error_message_rv(NULL)
    ndvi_ts_plot_obj(NULL)
    ndvi_ts_stats(NULL)
  })

  observeEvent(input$ndvi_ts_lc_class, {
    ndvi_ts_ready(FALSE)
    ndvi_ts_plot_obj(NULL)
    ndvi_ts_stats(NULL)
    data_source_rv("none")
  }, ignoreInit = TRUE)

  observeEvent(input$ndvi_ts_view, {
    ndvi_ts_ready(FALSE)
    ndvi_ts_plot_obj(NULL)
    ndvi_ts_stats(NULL)
    data_source_rv("none")
  }, ignoreInit = TRUE)
  
  # Observe the Generate Figure button
  observeEvent(input$generate_ndvi_ts_figures, {
    message("=========== Starting NDVI Time Series Generation =============")
    
    # To be extra sure that no figure is shown, clear previous error messages
    ndvi_ts_ready(FALSE)
    ndvi_ts_plot_obj(NULL)
    ndvi_ts_stats(NULL)
    error_message_rv(NULL)
    
    # Get user inputs
    country_name <- input$country
    resolution <- input$resolution

    selected_end <- available_year_end("NDVI", country_name, resolution, input$year)
    end_month <- selected_end$end_month
    end_year  <- selected_end$end_year
    
    # Wrap data generation in tryCatch to handle missing files/errors
    tryCatch({
      
      # Ensure the figures directory exists (used by other exports; NDVI TS is plotly)
      if (!dir.exists(figures_dir)) {
        dir.create(figures_dir, recursive = TRUE)
      }
      
      lc_class <- if (!is.null(input$ndvi_ts_lc_class) && nzchar(input$ndvi_ts_lc_class))
        input$ndvi_ts_lc_class else NULL
      ts_view <- if (!is.null(input$ndvi_ts_view)) input$ndvi_ts_view else "monthly"

      ndvi_result <- generate_timeseries(
        country_name     = country_name,
        resolution       = resolution,
        end_year         = end_year,
        end_month        = end_month,
        figures_dir      = figures_dir,
        data_dir         = data_dir,
        return_plot      = TRUE,
        figure_filename  = NULL,
        land_cover_class = lc_class,
        view             = ts_view
      )

      ndvi_ts_ready(TRUE)
      ndvi_ts_plot_obj(ndvi_result$plot)
      ndvi_ts_stats(ndvi_result$stats)
      ndvi_ts_view_rv(ts_view)
      data_source_rv(if (!is.null(ndvi_result$data_source)) ndvi_result$data_source else "tif")
      error_message_rv(NULL) # Clear any previous error messages
      
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_ts_ready(FALSE)
      ndvi_ts_plot_obj(NULL)
      ndvi_ts_stats(NULL)
      # Improvement 9: Use friendly error message with dynamic values
      error_message_rv(friendly_ndvi_no_data_message(input$year, input$resolution, input$country))
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Log the error to the console for debugging
      message("Error generating NDVI timeseries: ", e$message)
    })
    message("=========== End of NDVI Time Series Generation =============")
  })
  

  # ---------------------------------------------------------------------------------------------------
  # NDVI LAND COVER EXPLORER SERIES CHART
  # ---------------------------------------------------------------------------------------------------
  
  # Reactive flag to control whether the NDVI Land Cover UI should be shown
  ndvi_lc_ready <- reactiveVal(FALSE)
  lc_ts_plot_obj <- reactiveVal(NULL)
  lc_lc_highlight <- reactiveVal(NULL)
  lc_plot_year <- reactiveVal(NULL)
  lc_map_bbox_by_stem <- reactiveVal(NULL)
  
  output$lc_ndvi_plot_output <- plotly::renderPlotly({
    p <- lc_ts_plot_obj()
    shiny::req(p)
    p <- plotly::event_register(p, "plotly_click")
    p <- plotly::event_register(p, "plotly_restyle")
    p <- plotly::event_register(p, "plotly_legendclick")
    p
  })
  
  # Render a container for the plot or error message
  output$lc_plot_container <- renderUI({
    error_msg <- error_message_rv()
    
    if (is.null(error_msg) && isTRUE(ndvi_lc_ready())) { # If no errors and the reactive flag is TRUE (after successful figure generation), show output, otherwise empty
      fluidRow(
        column(12,
               div(
                 class = "image-fill top-center ndvi-ts-plot-stack",
                 ndvi_landcover_titles_ui(input$resolution, year = lc_plot_year()),
                 plotlyOutput("lc_ndvi_plot_output", height = "1050px"),
                 height = "auto"
               ))
      )
    } else {
      ndvi_error_ui(error_msg)
    }
  })
  
  # Shared logic for plotly_click and plotly_legendclick: highlight, optional geo fitBounds
  lc_handle_ndvi_plotly_selection <- function(ed) {
    shiny::req(ndvi_lc_ready())
    p <- lc_ts_plot_obj()
    shiny::req(p)
    if (!is.data.frame(ed) || nrow(ed) < 1L) {
      return(invisible(NULL))
    }
    cn <- ed$curveNumber[1]
    if (length(cn) == 0L || is.na(cn)) {
      return(invisible(NULL))
    }
    pb <- plotly::plotly_build(p)
    idx <- as.integer(cn) + 1L
    if (idx < 1L || idx > length(pb$x$data)) {
      return(invisible(NULL))
    }
    nm <- pb$x$data[[idx]]$name
    if (is.null(nm)) {
      nm <- ""
    } else {
      nm <- as.character(nm)[1]
    }
    if (identical(nm, "Study area")) {
      return(invisible(NULL))
    }
    if (grepl("^Historical range", nm)) {
      lc_lc_highlight(NULL)
      lc_landcover_emphasize_plotly_traces(session, "lc_ndvi_plot_output", p, NULL)
      return(invisible(NULL))
    }
    lab <- nm
    labs_vals <- unname(land_cover_class_legend_labels())
    if (!lab %in% labs_vals) {
      return(invisible(NULL))
    }
    cur <- lc_lc_highlight()
    bbs <- lc_map_bbox_by_stem()
    if (!is.null(cur) && identical(cur, lab)) {
      lc_lc_highlight(NULL)
      lc_landcover_emphasize_plotly_traces(session, "lc_ndvi_plot_output", p, NULL)
    } else {
      lc_lc_highlight(lab)
      lc_landcover_emphasize_plotly_traces(session, "lc_ndvi_plot_output", p, lab)
      stem <- landcover_label_to_stem(lab, bbs)
      if (!is.na(stem) && !is.null(bbs[[stem]])) {
        lc_plotly_relayout_geo_bbox(session, "lc_ndvi_plot_output", p, bbs[[stem]])
      }
    }
  }
  
  # Plotly chart or map click: highlight toggle + pan map to selected class
  observeEvent(plotly::event_data("plotly_click", source = "lc_ndvi_plot_output"), {
    ed <- plotly::event_data("plotly_click", source = "lc_ndvi_plot_output")
    shiny::req(ed)
    lc_handle_ndvi_plotly_selection(ed)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Plotly legend (line names): same behavior when legend fires events
  observeEvent(plotly::event_data("plotly_legendclick", source = "lc_ndvi_plot_output"), {
    ed <- plotly::event_data("plotly_legendclick", source = "lc_ndvi_plot_output")
    shiny::req(ed)
    lc_handle_ndvi_plotly_selection(ed)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Clear the image when switching tabs or subtabs
  observeEvent(list(input$tabs, input$ndvisubtabs), {
    ndvi_lc_ready(FALSE)
    error_message_rv(NULL)
    lc_ts_plot_obj(NULL)
    lc_lc_highlight(NULL)
    lc_plot_year(NULL)
    lc_map_bbox_by_stem(NULL)
  }, ignoreInit = TRUE)
  
  # Observe the Generate Figure button
  observeEvent(input$generate_lc_figures, {
    message("=========== Starting NDVI Land Cover Generation =============")
    
    # To be extra sure that no figure is shown, clear previous error messages
    ndvi_lc_ready(FALSE)
    error_message_rv(NULL)
    lc_ts_plot_obj(NULL)
    lc_lc_highlight(NULL)
    lc_plot_year(NULL)
    lc_map_bbox_by_stem(NULL)
    
    # Get user inputs
    country_name <- input$country
    resolution <- input$resolution
    
    selected_end <- available_year_end("NDVI", country_name, resolution, input$year)
    end_month <- selected_end$end_month
    end_year  <- selected_end$end_year
    
    map_year <- "2023"
    vector_src <- "S2_10m_LULC"
    lc_figure_filename <- paste0("figure_LULCmap_", country_name, "_", vector_src, "_", map_year, ".html")
    lc_figure_path <- file.path(figures_dir, lc_figure_filename)
    data_path <- file.path(data_dir, "LandUse", country_name, paste0(vector_src, "_", map_year))
    
    # NDVI time series + Plotly geo map (shared legend via legendgroup); falls back to chart-only if no map folder
    tryCatch({
      if (!dir.exists(figures_dir)) {
        dir.create(figures_dir, recursive = TRUE)
      }
      
      lc_result <- generate_timeseries_landcover(
        country_name = country_name,
        resolution   = resolution,
        end_year     = end_year,
        end_month    = end_month,
        figures_dir  = figures_dir,
        data_dir     = data_dir,
        land_use_src = "S2_10m_LULC_2023",
        return_plot  = TRUE,
        lulc_map_folder_path = data_path
      )
      lc_ts_plot_obj(lc_result$plot)
      lc_map_bbox_by_stem(lc_result$bbox_by_stem)
      lc_plot_year(end_year)
      data_source_rv(if (!is.null(lc_result$data_source)) lc_result$data_source else "tif")
      
      htmlwidgets::saveWidget(
        lc_result$plot,
        file.path(figures_dir, lc_figure_filename),
        selfcontained = TRUE
      )
      
      ndvi_lc_ready(TRUE)
      error_message_rv(NULL)
      
    }, error = function(e) {
      ndvi_lc_ready(FALSE)
      # Improvement 9: Use friendly error message with dynamic values
      error_message_rv(friendly_ndvi_no_data_message(input$year, input$resolution, input$country))
      lc_ts_plot_obj(NULL)
      lc_lc_highlight(NULL)
      lc_plot_year(NULL)
      lc_map_bbox_by_stem(NULL)
      
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      message("Error generating Land Cover NDVI / map: ", e$message)
    })
    message("=========== End of NDVI Land Cover Generation =============")
  })

  
  # ---------------------------------------------------------------------------------------------------
  # NDVI DELTA MAP AND CHARTS
  # --------------------------------------------------------------------------------------------------- 

  # Reactive flag to control whether the NDVI Delta Map UI should be shown
  ndvi_dm_ready <- reactiveVal(FALSE)
  ndvi_annual_ready  <- reactiveVal(FALSE)
  ndvi_annual_result <- reactiveVal(NULL)

  output$ndvi_annual_leaflet_output <- renderLeaflet({
    res <- ndvi_annual_result()
    shiny::req(res)
    plot_annual_ndvi_leaflet(res)
  })

  output$ndvi_annual_year_selectors <- renderUI({
    req(input$country, input$resolution)
    years <- get_available_years(data_dir, "NDVI", input$country, input$resolution)
    if (length(years) < 2L) {
      return(p("Not enough years available for annual comparison."))
    }
    tagList(
      selectInput("ndvi_annual_year_a", "Baseline year",
                  choices = years, selected = min(years)),
      selectInput("ndvi_annual_year_b", "Comparison year",
                  choices = years, selected = max(years))
    )
  })

  observeEvent(input$generate_ndvi_annual_change, {
    ndvi_annual_ready(FALSE)
    ndvi_annual_result(NULL)
    error_message_rv(NULL)

    country_name <- input$country
    resolution   <- input$resolution
    year_a       <- as.integer(input$ndvi_annual_year_a)
    year_b       <- as.integer(input$ndvi_annual_year_b)

    if (is.na(year_a) || is.na(year_b) || year_b <= year_a) {
      showNotification("Comparison year must be after baseline year.", type = "warning", duration = 5)
      return()
    }

    tryCatch({
      # HOT PATH: per-pixel annual grids from the API; falls through to cold TIF.
      sr <- get_sensor_resolution(resolution)
      res <- if (!is.null(sr)) {
        compute_ndvi_delta_hot(country_name, sr$sensor, sr$resolution, year_a, year_b)
      } else NULL
      if (!is.null(res)) {
        ndvi_annual_result(res)
        ndvi_annual_ready(TRUE)
        data_source_rv("api")
        error_message_rv(NULL)
        return()
      }

      # COLD PATH: compute annual means from raw TIF rasters (unchanged)
      res <- compute_annual_ndvi_change(year_a, year_b, country_name, resolution, data_dir)
      ndvi_annual_result(res)
      ndvi_annual_ready(TRUE)
      data_source_rv("tif")  # annual change is computed from raw TIF rasters
      error_message_rv(NULL)
    }, error = function(e) {
      ndvi_annual_ready(FALSE)
      ndvi_annual_result(NULL)
      # Improvement 9: Use friendly error message with dynamic values
      error_message_rv(friendly_ndvi_no_data_message(input$year, input$resolution, input$country))
      showNotification(HTML("The figure cannot be generated due to missing data.
       Please contact us at
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."),
                       type = "error", duration = 6)
      message("Error generating Annual NDVI Change: ", e$message)
    })
  })

  # Render a container for the plot or error message
  output$dm_plot_container <- renderUI({
    view <- if (is.null(input$ndvi_delta_view)) "monthly" else input$ndvi_delta_view
    error_msg <- error_message_rv()

    if (view == "monthly") {
      if (is.null(error_msg) && isTRUE(ndvi_dm_ready())) {
        fluidRow(
          column(8, div(class = "image-fill top-center",
                        imageOutput("ndvi_histmap_output"), height = "100%")),
          column(4, htmlOutput("ndvi_delta_map_output"))
        )
      } else ndvi_error_ui(error_msg)
    } else {
      if (is.null(error_msg) && isTRUE(ndvi_annual_ready())) {
        res <- ndvi_annual_result()
        net_gain <- res$pos_km2 >= res$neg_km2
        net_dir <- if (net_gain) "Net vegetation gain" else "Net vegetation loss"
        net_color <- if (net_gain) "#1D9E75" else "#C62828"
        total_km2 <- if (!is.null(res$total_km2) && !is.na(res$total_km2) && res$total_km2 > 0) res$total_km2 else res$pos_km2 + res$neg_km2
        gain_pct <- if (total_km2 > 0) round((res$pos_km2 / total_km2) * 100) else NA_integer_
        loss_pct <- if (total_km2 > 0) round((res$neg_km2 / total_km2) * 100) else NA_integer_
        gain_pct_label <- if (!is.na(gain_pct)) paste0(" (", gain_pct, "% of study area)") else ""
        loss_pct_label <- if (!is.na(loss_pct)) paste0(" (", loss_pct, "% of study area)") else ""
        interpretation <- if (net_gain) {
          paste0("Most of the study area had higher average vegetation health in ", res$year_b,
                 " compared to ", res$year_a,
                 ". This may reflect a wetter than usual year, vegetation recovery, or land management changes.")
        } else {
          paste0("Most of the study area had lower average vegetation health in ", res$year_b,
                 " compared to ", res$year_a,
                 ". This may reflect a drier than usual year or ongoing land cover change.")
        }
        tagList(
          div(
            style = "background: #E8F5E922; border-left: 4px solid #1D9E75; padding: 12px; margin-bottom: 12px; border-radius: 4px;",
            tags$strong("Annual Change Summary"), tags$br(),
            tags$span(style = paste0("font-weight:700; color:", net_color, ";"), net_dir),
            tags$br(),
            tags$span(style = "display:none;", sprintf(
              "Between %s and %s: %.1f km² showed vegetation gain and %.1f km² showed vegetation loss — %s.",
              res$year_a, res$year_b, res$pos_km2, res$neg_km2, net_dir
            )),
            tags$br(),
            tags$span(sprintf(
              "Vegetation gain: %.1f km2%s. Vegetation loss: %.1f km2%s.",
              res$pos_km2, gain_pct_label, res$neg_km2, loss_pct_label
            )),
            tags$br(),
            tags$span(interpretation)
          ),
          leafletOutput("ndvi_annual_leaflet_output", height = "500px")
        )
      } else ndvi_error_ui(error_msg)
    }
  })

  # Clear the image when switching tabs or subtabs
  observeEvent(list(input$tabs, input$ndvisubtabs), {
    ndvi_dm_ready(FALSE)
    ndvi_annual_ready(FALSE)
    ndvi_annual_result(NULL)
    error_message_rv(NULL)

    # Clear server outputs so nothing can re-appear
    output$ndvi_histmap_output <- NULL
    output$ndvi_delta_map_output <- renderUI(NULL)
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$generate_ndvi_delta_plot, {
    message("=========== Starting NDVI Delta Plot Generation =============")
    
    # To be extra sure that no figure is shown, clear previous error messages
    ndvi_dm_ready(FALSE)
    error_message_rv(NULL)
    
    # NDVI Map Plots
    country_name <- input$country
    map_month <- match(input$month, month.name)
    map_year <- input$year
    resolution <- input$resolution
    figure_filename <- paste0("figure_NDVImaps_", country_name, "_", map_month, "_", map_year, "_", resolution, "m.png")
    figure_path <- file.path(figures_dir, figure_filename)
    
    tryCatch({
      if (!dir.exists(figures_dir)) {
        dir.create(figures_dir, recursive = TRUE)
      }
      # HOT PATH: build the NDVI facet PNG from per-year monthly grids; falls
      # through to the cold TIF facet when the API is unavailable.
      sr_hist <- get_sensor_resolution(resolution)
      hist_hot <- if (!is.null(sr_hist)) tryCatch(
        generate_2Dmap_hot(country_name, sr_hist$sensor, sr_hist$resolution,
                           map_year, map_month, figures_dir, figure_filename),
        error = function(e) NULL) else NULL
      if (is.null(hist_hot)) {
        if (!file.exists(figure_path)) {
          generate_2Dmap(country_name, resolution, map_year, map_month, figures_dir, data_dir, FALSE, FALSE, figure_filename)
        }
      } else {
        data_source_rv("api")
      }
      output$ndvi_histmap_output <- renderImage({
        list(src = figure_path,
             alt = "NDVI 2D map")
      }, deleteFile = FALSE)

    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_dm_ready(FALSE)
      # Improvement 9: Use friendly error message with dynamic values
      error_message_rv(friendly_ndvi_no_data_message(input$year, input$resolution, input$country))
      output$ndvi_histmap_output <- NULL
      output$ndvi_delta_map_output <- renderUI(NULL) 
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      message("Error generating static NDVI map: ", e$message)
    })

    # NDVI Delta Map
    figure_filename_dm <- paste0("figure_deltaNDVImaps_", country_name, "_", map_month, "_", map_year, "_", resolution, "m.html")
    figure_path_dm <- file.path(figures_dir, figure_filename_dm)

    tryCatch({
      # HOT PATH: monthly NDVI anomaly grid (selected month vs same-month
      # historical baseline) rendered as a live Leaflet. Falls through silently
      # to the cold saved-HTML iframe below when the API is unavailable.
      sr_dm <- get_sensor_resolution(resolution)
      anomaly_raw <- if (!is.null(sr_dm)) try_api_grid_call(
        "/api/v1/ndvi/monthly-anomaly-grid",
        list(aoi = country_name, sensor = sr_dm$sensor,
             resolution = sr_dm$resolution, year = map_year, month = map_month)
      ) else NULL

      if (!is.null(anomaly_raw)) {
        anomaly_df <- parse_grid_response(anomaly_raw)
        output$ndvi_monthly_anomaly_leaflet <- renderLeaflet(
          plot_monthly_anomaly_leaflet(anomaly_df, map_year, map_month,
                                       anomaly_raw$metadata$baseline_years)
        )
        output$ndvi_delta_map_output <- renderUI({
          leafletOutput("ndvi_monthly_anomaly_leaflet", height = "500px")
        })
        ndvi_dm_ready(TRUE)
        data_source_rv("api")
        error_message_rv(NULL)
        message("=========== End of NDVI Delta Plot Generation (API) =============")
        return()
      }

      # COLD PATH: saved delta-map HTML in an iframe (unchanged)
      if (!file.exists(figure_path_dm)) {
        if (!dir.exists(figures_dir)) {
          dir.create(figures_dir, recursive = TRUE)
        }
        generate_2Dmap(country_name, resolution, map_year, map_month, figures_dir, data_dir, TRUE, FALSE, figure_filename_dm)
      }
      output$ndvi_delta_map_output <- renderUI({
        tags$iframe(src = paste0("figures/", figure_filename_dm),
                    width = "100%",
                    height = "500px",
                    frameborder = 0)
      })

      ndvi_dm_ready(TRUE) # Mark UI as ready when both figures are rendered successfully (renderUI will now return the container)
      data_source_rv("tif")  # delta maps are computed from raw TIF rasters
      error_message_rv(NULL) # Clear any previous error messages

    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_dm_ready(FALSE)
      # Improvement 9: Use friendly error message with dynamic values
      error_message_rv(friendly_ndvi_no_data_message(input$year, input$resolution, input$country))
      output$ndvi_histmap_output <- NULL
      output$ndvi_delta_map_output <- renderUI(NULL) 
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      message("Error generating delta NDVI map: ", e$message)
    })
    message("=========== End of NDVI Delta Plot Generation =============")
  })
 
  # ---------------------------------------------------------------------------------------------------
  # BURNED AREA MAP AND EXPLORER FUNCTIONALITY
  # ---------------------------------------------------------------------------------------------------  

  # Reactive flag and plot object for BA timeseries
  ba_ts_ready    <- reactiveVal(FALSE)
  ba_ts_plot_obj <- reactiveVal(NULL)

  output$ba_ts_plot_output <- plotly::renderPlotly({
    p <- ba_ts_plot_obj()
    shiny::req(p)
    p
  })

  # Render a container for the plot or error message
  output$ba_plot_container <- renderUI({
    error_msg <- error_message_rv()
    if (is.null(error_msg) && isTRUE(ba_ts_ready())) {
      div(class = "image-fill top-center ndvi-ts-plot-stack",
          plotly::plotlyOutput("ba_ts_plot_output", height = "500px"),
          height = "auto")
    } else {
      return(NULL)
    }
  })

  # Clear when switching tabs or subtabs
  observeEvent(list(input$tabs, input$basubtabs), {
    ba_ts_ready(FALSE)
    ba_ts_plot_obj(NULL)
    error_message_rv(NULL)
  })

  # --- Daily Activity chart ---
  ba_daily_plot_obj <- reactiveVal(NULL)

  # Dynamic year selector populated from available TIF files for selected country/resolution
  output$ba_daily_year_selector <- renderUI({
    country_name <- input$country
    resolution   <- input$resolution
    available_years <- get_available_years(
      data_dir      = data_dir,
      data_type     = "BurnedArea",
      country_name  = country_name,
      resolution    = resolution,
      decreasing    = TRUE
    )
    default_sel <- head(as.character(available_years), 3)
    selectInput("ba_daily_years", "Select years to compare",
                choices  = as.character(available_years),
                selected = default_sel,
                multiple = TRUE)
  })

  output$ba_daily_plot_container <- renderUI({
    p <- ba_daily_plot_obj()
    if (is.null(p)) return(NULL)
    plotly::plotlyOutput("ba_daily_plot_output", height = "500px")
  })

  output$ba_daily_plot_output <- plotly::renderPlotly({
    p <- ba_daily_plot_obj()
    shiny::req(p)
    p
  })

  observeEvent(input$generate_ba_daily_figures, {
    message("=========== Starting Daily Burn Activity Generation =============")
    ba_daily_plot_obj(NULL)
    error_message_rv(NULL)

    country_name   <- input$country
    resolution     <- input$resolution
    selected_years <- as.integer(input$ba_daily_years)
    season_months  <- seq(input$ba_season_months[1], input$ba_season_months[2])
    res_m          <- as.numeric(gsub("[^0-9]", "", resolution))
    pixel_area_km2 <- (res_m / 1000)^2

    # HOT PATH: the /burned-area/daily endpoint takes a single year (int), so
    # fetch each selected year separately and bind. Stop at the first failed year
    # (a down API then costs one timeout, not one per year); any miss = fall back.
    api_parts <- vector("list", length(selected_years))
    api_ok    <- length(selected_years) > 0
    for (i in seq_along(selected_years)) {
      d <- try_api_call(
        endpoint = "/api/v1/burned-area/daily",
        params   = list(aoi = country_name, year = selected_years[i])
      )
      if (is.null(d) || !all(c("date", "burned_km2", "year") %in% names(d))) {
        api_ok <- FALSE
        break
      }
      api_parts[[i]] <- d
    }
    if (api_ok) {
      api_daily <- do.call(rbind, api_parts)
      api_daily$date  <- as.Date(api_daily$date)
      api_daily$month <- as.integer(format(api_daily$date, "%m"))
      api_daily <- api_daily[api_daily$month %in% season_months, ]
      all_daily <- if (nrow(api_daily) > 0) {
        data.frame(
          date = api_daily$date,
          km2  = api_daily$burned_km2,
          year = as.character(api_daily$year),
          stringsAsFactors = FALSE
        )
      } else NULL
      ba_daily_plot_obj(plot_ba_daily_activity(all_daily, as.character(selected_years)))
      error_message_rv(NULL)
      data_source_rv("api")
      message("=========== End of Daily Burn Activity Generation (API) =============")
      return()
    }

    # WARM PATH: read pre-processed ba_daily.parquet
    tryCatch({
      pq_daily <- try_read_parquet(
        get_parquet_path(country_name, resolution, "ba_daily", sensor_override = "burned_area")
      )
      if (!is.null(pq_daily)) {
        pq_daily <- pq_daily[pq_daily$aoi == country_name & pq_daily$year %in% selected_years, ]
        pq_daily$date  <- as.Date(pq_daily$date)
        pq_daily$month <- as.integer(format(pq_daily$date, "%m"))
        pq_daily <- pq_daily[pq_daily$month %in% season_months, ]
        all_daily <- if (nrow(pq_daily) > 0) {
          data.frame(
            date = pq_daily$date,
            km2  = pq_daily$burned_km2,
            year = as.character(pq_daily$year),
            stringsAsFactors = FALSE
          )
        } else NULL
        ba_daily_plot_obj(plot_ba_daily_activity(all_daily, as.character(selected_years)))
        error_message_rv(NULL)
        data_source_rv("parquet")
        message("=========== End of Daily Burn Activity Generation (parquet) =============")
        return()
      }
    }, error = function(e) {
      warning("BA daily warm path failed, falling through to cold path: ", e$message)
    })

    data_path <- file.path(data_dir, "BurnedArea", country_name, paste0(resolution, "m_resolution"))

    tryCatch({
      ba_files <- get_filenames(filepath = data_path, data_type = "BurnedArea",
                                file_extension = ".tif", country_name = country_name)
      files_df <- get_ba_filename_df(ba_files = ba_files) %>%
        dplyr::filter(month %in% season_months)

      all_daily <- dplyr::bind_rows(lapply(selected_years, function(yr) {
        get_ba_daily_activity(files_df, data_path, yr, pixel_area_km2)
      }))
      if (nrow(all_daily) == 0) all_daily <- NULL

      ba_daily_plot_obj(plot_ba_daily_activity(all_daily, selected_years))
      error_message_rv(NULL)
    }, error = function(e) {
      ba_daily_plot_obj(NULL)
      error_message_rv(e$message)
      showNotification(HTML("Daily activity chart could not be generated due to missing data.
       Please contact us at <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a>."),
                       type = "error", duration = 6)
      message("Error generating daily burn activity: ", e$message)
    })
  })
  
  # Set Reactive values for error message, GeoJSON export path checker and map_generated toggle
  error_message_rv <- reactiveVal(NULL)
  geojson_export_path_rv <- reactiveVal(NULL)
  map_generated <- reactiveVal(FALSE)  # Track if map has been generated
  
  # Reset map_generated when year/month/country inputs change
  observeEvent({
    input$year
    input$month
    input$country
  }, {
    map_generated(FALSE)
    error_message_rv(NULL)
  }, ignoreNULL = FALSE)
  
  # Render download UI based on map generation status
  observe({
    if (map_generated()) {
      shinyjs::enable("download_ba_geojson")
    } else {
      shinyjs::disable("download_ba_geojson")
    }
  })
  
  # Observe the Generate Figure button
  observeEvent(input$generate_ba_ts_figures, {
    message("=========== Starting Burned Area Time Series Generation =============")
    
    # To be extra sure that no figure is shown, clear previous error messages
    ba_ts_ready(FALSE)
    error_message_rv(NULL)

    # Get user inputs
    country_name <- input$country
    resolution <- input$resolution
    
    selected_end <- available_year_end("BurnedArea", country_name, resolution, input$year)
    end_month <- selected_end$end_month
    end_year  <- selected_end$end_year
    
    tryCatch({
      ba_result <- generate_ba_timeseries(
        country_name    = country_name,
        resolution      = resolution,
        end_year        = end_year,
        end_month       = end_month,
        figures_dir     = figures_dir,
        data_dir        = data_dir,
        return_plot     = TRUE,
        figure_filename = NULL
      )
      ba_ts_plot_obj(ba_result$plot)
      data_source_rv(if (!is.null(ba_result$data_source)) ba_result$data_source else "tif")
      ba_ts_ready(TRUE)
      error_message_rv(NULL)
    }, error = function(e) {
      ba_ts_ready(FALSE)
      ba_ts_plot_obj(NULL)
      error_message_rv(e$message)
      showNotification(HTML("The figure cannot be generated due to missing data.
       Please contact us at
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."),
                       type = "error", duration = 6)
      message("Error generating Burned Area timeseries: ", e$message)
    })
    message("=========== End of Burned Area Time Series Generation =============")
  })
  
  
  #######################################################################
  ########### BA Map Explorer ###########################################
  #######################################################################
  
  # Reactive flag to control whether the BA Map UI should be shown
  ba_map_ready <- reactiveVal(FALSE)
  
  # Render a container for the historical comparison static map
  output$ba_map_container <- renderUI({
    error_msg <- error_message_rv()
    if (is.null(error_msg) && isTRUE(ba_map_ready())) {
      div(class = "image-fill top-center", imageOutput("ba_map_output"), height = "auto")
    } else {
      return(NULL)
    }
  })

  # Clear outputs and hide the monthly leaflet when switching tabs or subtabs
  observeEvent(list(input$tabs, input$basubtabs), {
    ba_map_ready(FALSE)
    error_message_rv(NULL)
    output$ba_map_output <- NULL
    shinyjs::hide("monthly_leaflet_wrap")
  })
  
  # Session cache for AoI area (km2) from the API — fetched once per AoI and
  # reused for the BA facet's percentage-of-study-area labels.
  .aoi_area_cache <- new.env(parent = emptyenv())
  ba_aoi_area_km2 <- function(aoi) {
    key <- as.character(aoi)
    if (is.null(.aoi_area_cache[[key]])) {
      a <- get_aoi_area_km2(aoi)
      assign(key, if (is.null(a)) NA_real_ else a, envir = .aoi_area_cache)
    }
    val <- .aoi_area_cache[[key]]
    if (is.na(val)) NULL else val
  }

  # Observe the Generate Figure button
  observeEvent(input$generate_ba_map_figures, {
    message("=========== Starting Burned Area Map Generation =============")
    
    # To be extra sure that no figure is shown, clear previous error messages
    ba_map_ready(FALSE)
    error_message_rv(NULL)
    
    # Get user inputs
    country_name <- input$country
    resolution   <- input$resolution
    map_month    <- match(input$month, month.name)
    map_year     <- input$year
    
    # Define script and figure paths
    figure_filename_bam <- paste0("figure_BurnedAreamaps_", country_name, "_", 
                                  map_month, "_", map_year, "_", resolution, "m.png")
    figure_path_bam <- file.path(figures_dir, figure_filename_bam)
    
    tryCatch({
      if (!dir.exists(figures_dir)) {
        dir.create(figures_dir, recursive = TRUE)
      }
      # HOT PATH: build the BA facet PNG from per-year monthly grids; falls
      # through to the cold TIF facet when the API is unavailable.
      ba_hot <- tryCatch(
        generate_ba_2Dmap_hot(country_name, map_year, map_month, figures_dir,
                              figure_filename_bam, ba_aoi_area_km2(country_name)),
        error = function(e) NULL)
      if (is.null(ba_hot) && !file.exists(figure_path_bam)) {
        generate_ba_2Dmap(country_name, resolution, map_year, map_month, figures_dir, data_dir, FALSE, FALSE, figure_filename_bam)
      }

      # GeoJSON export stays on the COLD path (needed by the download button).
      geojson_export_path <- file.path(figures_dir, paste0(tools::file_path_sans_ext(figure_filename_bam), ".geojson"))
      if (!file.exists(geojson_export_path)) {
        geojson_export_path <- generate_ba_export(country_name, resolution, map_year, map_month, figures_dir, data_dir, figure_filename_bam)
      }
      geojson_export_path_rv(geojson_export_path)
      map_generated(TRUE)

      output$ba_map_output <- renderImage({
        list(src = figure_path_bam,
             width = "100%",
             alt = "Burned Area 2D map")
      }, deleteFile = FALSE)

      map_generated(TRUE) # Mark map generated as TRUE to enable the GeoJSON download button
      error_message_rv(NULL) # Clear any previous error messages

    }, error = function(e) {
      ba_map_ready(FALSE)
      error_message_rv(e$message)
      output$ba_map_output <- NULL
      shinyjs::hide("monthly_leaflet_wrap")
      showNotification(HTML("The figure cannot be generated due to missing data.
       Please contact us at
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."),
                       type = "error", duration = 6)
      message("Error generating static Burned Area map: ", e$message)
    })
    
    #### BA interactive Leaflet map (monthly footprint)
    tryCatch({
      # HOT PATH: build the interactive leaflet from the burned-area monthly grid
      # (pixels as markers). Falls through to the GeoJSON-based leaflet when the
      # API is unavailable. The static PNG + GeoJSON download stay on cold path.
      ba_grid_raw <- try_api_grid_call(
        "/api/v1/burned-area/monthly-grid",
        list(aoi = country_name, year = map_year, month = map_month)
      )
      ba_leaflet_source <- "tif"

      local({
        captured_geojson <- geojson_export_path_rv()
        captured_year    <- map_year
        captured_month   <- map_month
        captured_country <- country_name
        captured_grid    <- ba_grid_raw

        if (!is.null(captured_grid)) {
          ba_leaflet_source <<- "api"
          ba_df       <- parse_grid_response(captured_grid)
          burned_km2  <- captured_grid$metadata$burned_km2
          aoi_files   <- list.files(file.path(data_dir, "AoI"),
                                    pattern = paste0("AoI.*", captured_country, ".*\\.geojson$"))
          aoi_shape   <- read_aoi_geometry(file.path(data_dir, "AoI", aoi_files[[1]]), captured_country)
          output$ba_monthly_leaflet <- renderLeaflet({
            build_ba_monthly_leaflet_grid(ba_df, aoi_shape, captured_year,
                                          captured_month, burned_km2)
          })
        } else {
          output$ba_monthly_leaflet <- renderLeaflet({
            build_ba_monthly_leaflet(
              geojson_path = captured_geojson,
              data_dir     = data_dir,
              country      = captured_country,
              year         = captured_year,
              month_num    = captured_month
            )
          })
        }
      })

      shinyjs::show("monthly_leaflet_wrap")
      ba_map_ready(TRUE)
      # "api" when the interactive map came from the grid, else cold TIF/GeoJSON.
      data_source_rv(ba_leaflet_source)
      error_message_rv(NULL)
    }, error = function(e) {
      ba_map_ready(FALSE)
      error_message_rv(e$message)
      output$ba_map_output <- NULL
      shinyjs::hide("monthly_leaflet_wrap")
      showNotification(HTML("The map cannot be generated due to missing data.
       Please contact us at
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."),
                       type = "error", duration = 6)
      message("Error generating Burned Area interactive map: ", e$message)
    })
    message("=========== End of Burned Area Map Generation =============")
  })

  # ------- Fire Return Period reactive + outputs ----------------------------

  frp_result <- reactive({
    req(input$ba_map_view == "frp")
    req(input$country, input$resolution)
    withProgress(message = "Analysing burn patterns across available years...", {
      tryCatch(
        build_ba_frp_leaflet(data_dir  = data_dir,
                             country   = input$country,
                             resolution = input$resolution),
        error = function(e) {
          showNotification(paste("Could not compute fire return period:", e$message),
                           type = "error", duration = 8)
          NULL
        }
      )
    })
  })

  # Fire Return Period uses the geometry hot path: reflect its source in the label.
  observeEvent(frp_result(), {
    r <- frp_result()
    if (!is.null(r)) data_source_rv(if (!is.null(r$data_source)) r$data_source else "tif")
  })

  output$ba_frp_leaflet <- renderLeaflet({
    result <- frp_result()
    req(!is.null(result))
    result$map
  })

  output$frp_year_range_text <- renderUI({
    result <- frp_result()
    req(!is.null(result))
    p(style = "color:#555; font-size:0.9em; margin-bottom:10px;",
      paste0("Based on data from ", result$years_label,
             " (", result$n_years, " year", ifelse(result$n_years == 1, "", "s"), ")"))
  })

  # ------- BA Annual View reactive + outputs (NEW — hot path only) ----------

  # Per-pixel annual burn frequency: HOT (annual-grid API) -> COLD (compute from
  # the monthly burned-area TIFs). Returns a normalised list or NULL if neither
  # source is available. `source` drives the data label ("api" / "tif").
  ba_annual_data <- reactive({
    req(input$ba_map_view == "annual")
    req(input$country, input$year)

    aoi_files <- list.files(file.path(data_dir, "AoI"),
                            pattern = paste0("AoI.*", input$country, ".*\\.geojson$"))

    # HOT PATH
    raw <- try_api_grid_call(
      "/api/v1/burned-area/annual-grid",
      list(aoi = input$country, year = input$year)
    )
    if (!is.null(raw)) {
      aoi_shape <- if (length(aoi_files) > 0)
        tryCatch(read_aoi_geometry(file.path(data_dir, "AoI", aoi_files[[1]]), input$country),
                 error = function(e) NULL) else NULL
      return(list(source = "api", df = parse_grid_response(raw),
                  total_burned_km2 = raw$metadata$total_burned_km2, aoi_shape = aoi_shape))
    }

    # COLD PATH: compute burn frequency from raw TIFs.
    cold <- tryCatch(
      compute_ba_annual_cold(data_dir, input$country, input$resolution, input$year),
      error = function(e) NULL)
    if (!is.null(cold)) {
      return(list(source = "tif", df = cold$df,
                  total_burned_km2 = cold$total_burned_km2, aoi_shape = cold$aoi_shape))
    }
    NULL
  })

  observeEvent(ba_annual_data(), {
    d <- ba_annual_data()
    if (!is.null(d)) data_source_rv(d$source)
  })

  output$ba_annual_summary <- renderUI({
    req(input$ba_map_view == "annual")
    d <- ba_annual_data()
    if (is.null(d)) {
      return(div(class = "infobox",
                 p(style = "margin:0; color:#C62828;",
                   "Annual burn map requires the API server or local burned-area files.")))
    }
    tot <- d$total_burned_km2
    p(style = "color:#555; font-size:0.9em; margin-bottom:10px;",
      paste0("Total burned area in ", input$year, ": ",
             if (!is.null(tot) && !is.na(tot)) paste0(round(tot, 1), " km2") else "n/a"))
  })

  output$ba_annual_leaflet <- renderLeaflet({
    req(input$ba_map_view == "annual")
    d <- ba_annual_data()
    req(!is.null(d), !is.null(d$aoi_shape))
    build_ba_annual_leaflet(d$df, d$aoi_shape, input$year, d$total_burned_km2)
  })

  # ------- Show/hide sidebar year & month when toggling BA map views --------

  observeEvent(input$ba_map_view, {
    if (isTRUE(input$ba_map_view == "frp")) {
      shinyjs::hide("year")
      shinyjs::hide("month")
    } else if (isTRUE(input$ba_map_view == "annual")) {
      shinyjs::show("year")
      shinyjs::hide("month")
    } else {
      shinyjs::show("year")
      shinyjs::show("month")
    }
  }, ignoreNULL = TRUE)
  
  # Shiny download handler that uses the reactive value to access the GeoJSON path and filename
  output$download_ba_geojson <- downloadHandler(
    filename = function() {
      paste0("burned_area_", input$year, "_", match(input$month, month.name), ".geojson")
    },
    content = function(file) {
      # HOT PATH: fetch the vectorized burned-area GeoJSON from the API.
      # Single call, so no Windows short-circuit; longer 10s timeout because
      # server-side vectorization is slower than tabular fetches.
      api_url <- paste0(API_BASE_URL, "/api/v1/burned-area/monthly-geojson")
      response <- tryCatch(
        httr::GET(
          api_url,
          query = list(
            aoi   = input$country,
            year  = input$year,
            month = match(input$month, month.name)  # input$month is the name
          ),
          httr::timeout(10)
        ),
        error = function(e) NULL
      )
      if (!is.null(response) && httr::status_code(response) == 200) {
        content_text <- httr::content(response, "text", encoding = "UTF-8")
        # The endpoint returns 200 with {"status":"error"} when no data exists,
        # so only accept a genuine FeatureCollection before writing.
        parsed <- tryCatch(jsonlite::fromJSON(content_text, simplifyVector = FALSE),
                           error = function(e) NULL)
        if (!is.null(parsed) && identical(parsed$type, "FeatureCollection")) {
          writeLines(content_text, file)
          return()  # early return — skip cold path
        }
      }

      # COLD PATH: copy the pre-generated GeoJSON (existing behaviour, unchanged).
      path <- geojson_export_path_rv()
      req(path, file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    }
  )

  # ===================================================================================================
  # AI ASSISTANT (standalone chat tab — Part 3, text only)
  # ===================================================================================================

  # --- Global app-context tracker: lets the agent know what the user last viewed,
  #     even though the chat lives on its own tab. Updated as the user changes filters.
  current_context <- reactiveValues(
    tab        = "NDVIexplorerTab",
    aoi        = "Zambia_Mponda",
    sensor     = "sentinel2",
    resolution = 1000,
    year       = NULL
  )
  observeEvent(input$country,    { current_context$aoi <- input$country })
  observeEvent(input$resolution, {
    sr <- get_sensor_resolution(input$resolution)
    if (!is.null(sr)) {
      current_context$sensor     <- sr$sensor
      current_context$resolution <- sr$resolution
    }
  })
  observeEvent(input$year, { current_context$year <- input$year })
  observeEvent(input$tabs, { current_context$tab  <- input$tabs })

  # Hide the data filters (year/month/resolution) on the AI Assistant tab; the
  # AoI selector + mini-map stay visible (AoI is still passed to the agent).
  observeEvent(input$tabs, {
    if (identical(input$tabs, "AIassistantTab")) {
      shinyjs::hide("sidebar_filters")
    } else {
      shinyjs::show("sidebar_filters")
    }
  }, ignoreInit = FALSE)

  # --- Per-session conversation state (never stored server-side) ---
  # Each entry: list(role, content, chart=NULL|list, table=NULL|list, msg_id).
  session_history  <- reactiveVal(list())
  pending_question <- reactiveVal(NULL)      # set while the agent is processing
  msg_counter      <- reactiveVal(0)         # increments once per exchange (pair)

  # Strip <chart>{...}</chart> / <table>{...}</table> blocks from the display text
  # (the structured chart/table come back as separate response fields).
  strip_agent_tags <- function(text) {
    if (is.null(text)) return("")
    text <- gsub("<chart>\\s*\\{[^}]*(?:\\{[^}]*\\}[^}]*)*\\}\\s*</chart>",
                 "", text, perl = TRUE)
    text <- gsub("<table>\\s*\\{[^}]*(?:\\{[^}]*\\}[^}]*)*\\}\\s*</table>",
                 "", text, perl = TRUE)
    trimws(text)
  }

  # Render an assistant reply as HTML: markdown (incl. pipe tables via extensions)
  # minus the trailing empty <p></p> that commonmark appends.
  render_markdown_bubble <- function(text) {
    if (is.null(text)) return(HTML(""))
    html <- commonmark::markdown_html(text, extensions = TRUE)
    # Remove all empty / whitespace-only paragraph tags, then any trailing run.
    html <- gsub("<p>\\s*</p>", "", html, perl = TRUE)
    html <- gsub("<p>\\n</p>", "", html, perl = TRUE)
    html <- sub("(\\s*<p>[[:space:]]*</p>\\s*)+$", "", html, perl = TRUE)
    html <- trimws(html)
    HTML(html)
  }

  # Unique per-message output IDs (used to register renderers per message).
  ai_output_ids <- function(msg_id) {
    list(
      chart = paste0("ai_chart_", msg_id),
      table = paste0("ai_table_", msg_id),
      image = paste0("ai_image_", msg_id)
    )
  }

  # The .ai-chat container is rebuilt by renderUI on every change, and MathJax
  # loads asynchronously — so a one-shot scroll/typeset can fire before the bubble
  # exists in the DOM or before MathJax is ready. Poll every 200ms (up to ~6s):
  # keep scrolling to the bottom, and once MathJax is available, typeset the chat
  # so any $$...$$ / $...$ LaTeX renders. Stop when the height is stable AND the
  # formulas have been typeset.
  ai_scroll_to_bottom <- function() {
    shinyjs::runjs("
      (function renderFinish() {
        // window.__etsUserHeld is a single shared guard (multiple concurrent
        // scroll loops can run for one response). It turns true only when the
        // user scrolls UP away from the bottom, so we never yank them back down
        // while they read earlier messages. Reset on each new response below.
        window.__etsUserHeld = false;
        var attempt = 0, maxAttempts = 60;       // 60 * 200ms = 12s safety window
        var lastHeight = -1, stableTicks = 0;
        var typesetStarted = false, typesetDone = false;

        function stick(c) { if (!window.__etsUserHeld) c.scrollTop = c.scrollHeight; }

        // Re-scroll when any image finishes loading (comparison PNGs, leaflet
        // tiles). New images are added late, so we re-hook on every tick.
        function hookImages(c) {
          var imgs = c.querySelectorAll('img');
          for (var i = 0; i < imgs.length; i++) {
            if (!imgs[i].__etsHook && !imgs[i].complete) {
              imgs[i].__etsHook = true;
              imgs[i].addEventListener('load', function() {
                var cc = document.querySelector('.ai-chat');
                if (cc) stick(cc);
              }, { once: true });
            }
          }
        }

        // Bind a scroll listener once per (rebuilt) container to detect a
        // deliberate scroll-up. Programmatic scroll-to-bottom lands near the
        // bottom (distance ~0), so it won't falsely trip this.
        function bindScroll(c) {
          if (c.__etsBound) return;
          c.__etsBound = true;
          c.addEventListener('scroll', function() {
            var distance = c.scrollHeight - c.scrollTop - c.clientHeight;
            window.__etsUserHeld = distance > 120;
          });
        }

        var interval = setInterval(function() {
          attempt++;
          var c = document.querySelector('.ai-chat');
          if (c) {
            bindScroll(c);
            hookImages(c);

            if (!typesetStarted) {
              if (window.MathJax && MathJax.typesetPromise) {
                typesetStarted = true;
                MathJax.typesetPromise([c]).then(function() {
                  typesetDone = true;
                  var cc = document.querySelector('.ai-chat');
                  if (cc) stick(cc);
                }).catch(function() { typesetDone = true; });
              } else if (window.MathJax === undefined) {
                typesetDone = true;   // no MathJax on the page: nothing to wait for
              }
            }

            stick(c);

            if (c.scrollHeight === lastHeight) { stableTicks++; } else { stableTicks = 0; }
            lastHeight = c.scrollHeight;

            // Stop only once the height has held steady for ~1s (covers async
            // charts/maps/images settling) AND MathJax has finished.
            if (stableTicks >= 5 && (typesetDone || !typesetStarted && attempt > 3)) {
              clearInterval(interval);
            }
          }
          if (attempt >= maxAttempts) clearInterval(interval);
        }, 200);
      })();
    ")
  }

  # Register a renderPlotly / renderTable output per assistant message that
  # carries a chart/table ref (once each). render_agent_* live in agent_renderers.R.
  registered_outputs <- reactiveVal(character(0))
  observe({
    history <- session_history()
    for (m in history) {
      if (!identical(m$role, "assistant")) next
      ids <- ai_output_ids(m$msg_id)

      .leaflet_types <- c("delta_map", "frp_map", "burned_area_map")

      # Plotly: every chart type EXCEPT Leaflet maps / static comparison images.
      if (!is.null(m$chart) &&
          !(m$chart$type %in% c(.leaflet_types, "comparison_image")) &&
          !(ids$chart %in% registered_outputs())) {
        local({
          local_chart <- m$chart
          local_id    <- ids$chart
          output[[local_id]] <- plotly::renderPlotly({ render_agent_chart(local_chart) })
        })
        registered_outputs(c(registered_outputs(), ids$chart))
        ai_scroll_to_bottom()  # re-scroll once the chart has rendered
      }

      # Leaflet maps (delta_map / frp_map / burned_area_map) on ids$chart.
      if (!is.null(m$chart) && m$chart$type %in% .leaflet_types &&
          !(ids$chart %in% registered_outputs())) {
        local({
          local_chart <- m$chart
          local_id    <- ids$chart
          output[[local_id]] <- renderLeaflet({ render_agent_leaflet(local_chart) })
        })
        registered_outputs(c(registered_outputs(), ids$chart))
        ai_scroll_to_bottom()
      }

      # Static comparison image (PNG) on ids$image.
      if (!is.null(m$chart) && identical(m$chart$type, "comparison_image") &&
          !(ids$image %in% registered_outputs())) {
        local({
          local_chart <- m$chart
          local_id    <- ids$image
          output[[local_id]] <- renderImage({ render_agent_image(local_chart, local_id) },
                                            deleteFile = FALSE)
        })
        registered_outputs(c(registered_outputs(), ids$image))
        ai_scroll_to_bottom()
      }

      if (!is.null(m$table) && !(ids$table %in% registered_outputs())) {
        local({
          local_table <- m$table
          local_id    <- ids$table
          output[[local_id]] <- renderTable({ render_agent_table(local_table) },
                                            striped = TRUE, hover = TRUE, bordered = TRUE)
        })
        registered_outputs(c(registered_outputs(), ids$table))
        ai_scroll_to_bottom()  # re-scroll once the table has rendered
      }
    }
  })

  # --- Provider / server-key discovery (GET /agent/providers) ---
  providers_info <- reactive({
    tryCatch({
      resp <- httr::GET(paste0(API_BASE_URL, "/agent/providers"), httr::timeout(5))
      if (httr::status_code(resp) != 200) return(NULL)
      jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                         simplifyVector = FALSE)
    }, error = function(e) NULL)
  })

  output$server_key_configured <- reactive({
    pi   <- providers_info()
    prov <- if (!is.null(input$llm_provider)) input$llm_provider else "anthropic"
    !is.null(pi) && isTRUE(pi[[prov]]$server_key_configured)
  })
  outputOptions(output, "server_key_configured", suspendWhenHidden = FALSE)

  # If the selected provider has no server key but another one does, switch to it
  # (so "set OPENAI_API_KEY on the server" shows "ready" without manual selection).
  observeEvent(providers_info(), {
    pi <- providers_info()
    if (is.null(pi)) return()
    prov <- if (!is.null(input$llm_provider)) input$llm_provider else "anthropic"
    if (!isTRUE(pi[[prov]]$server_key_configured)) {
      configured <- names(pi)[vapply(pi, function(x) isTRUE(x$server_key_configured), logical(1))]
      if (length(configured) > 0) {
        updateSelectInput(session, "llm_provider", selected = configured[[1]])
      }
    }
  })

  # --- Collapsible settings panel state (expanded on first load) ---
  settings_expanded <- reactiveVal(TRUE)
  output$settings_expanded <- reactive(settings_expanded())
  outputOptions(output, "settings_expanded", suspendWhenHidden = FALSE)

  observeEvent(input$toggle_settings, {
    settings_expanded(!settings_expanded())
  })
  observeEvent(settings_expanded(), {
    updateActionButton(session, "toggle_settings",
                       label = if (settings_expanded()) "Settings ▲" else "Settings ▼")
  })

  # Collapsed-state one-line summary: provider + key status.
  .provider_label <- function(p) {
    switch(p, anthropic = "Anthropic (Claude)", openai = "OpenAI (GPT-4o)", p)
  }
  output$settings_summary <- renderText({
    prov <- if (!is.null(input$llm_provider)) input$llm_provider else "anthropic"
    pi   <- providers_info()
    key_on_server <- !is.null(pi) && isTRUE(pi[[prov]]$server_key_configured)
    status <- if (!is.null(input$user_api_key) && nzchar(input$user_api_key)) {
      "API key set ✓"
    } else if (key_on_server) {
      "Server key active"
    } else {
      "No API key — expand to configure"
    }
    paste0(.provider_label(prov), " · ", status)
  })

  # --- Conversation display: chat bubbles (user right, agent left) ---
  output$conversation_display <- renderUI({
    msgs <- session_history()
    pend <- pending_question()
    if (length(msgs) == 0 && is.null(pend)) {
      return(div(class = "ai-chat",
                 div(class = "ai-empty",
                     "Ask a question about vegetation, fire, or land cover to get started.")))
    }
    bubbles <- lapply(msgs, function(m) {
      role <- if (identical(m$role, "user")) "user" else "agent"
      # Agent replies are markdown -> render to HTML; user input stays plain text.
      bubble_content <- if (identical(m$role, "assistant")) {
        render_markdown_bubble(m$content)
      } else {
        m$content
      }
      bubble <- div(class = paste("ai-row", role),
                    div(class = paste("ai-bubble", role), bubble_content))

      # Rendered chart/table output under an assistant message that carries refs.
      # Renderers are registered by the observer above; here we place the outputs.
      outputs <- NULL
      if (identical(m$role, "assistant") &&
          (!is.null(m$chart) || !is.null(m$table))) {
        ids   <- ai_output_ids(m$msg_id)
        parts <- list()
        if (!is.null(m$chart)) {
          ctype <- m$chart$type %||% ""
          ctitle <- if (!is.null(m$chart$title)) strong(m$chart$title)
          parts[[length(parts) + 1]] <- if (ctype %in% c("delta_map", "frp_map", "burned_area_map")) {
            div(class = "ai-output-row ai-output-map",
                ctitle, leafletOutput(ids$chart, height = "400px"))
          } else if (identical(ctype, "comparison_image")) {
            div(class = "ai-output-row ai-output-image",
                ctitle, imageOutput(ids$image, height = "auto"))
          } else {
            div(class = "ai-output-row ai-output-chart",
                ctitle, plotly::plotlyOutput(ids$chart, height = "350px"))
          }
        }
        if (!is.null(m$table)) {
          parts[[length(parts) + 1]] <- div(
            class = "ai-output-row ai-output-table",
            if (!is.null(m$table$title)) strong(m$table$title),
            tableOutput(ids$table)
          )
        }
        outputs <- do.call(tagList, parts)
      }
      tagList(bubble, outputs)
    })
    if (!is.null(pend)) {
      bubbles <- c(bubbles, list(
        div(class = "ai-row user",  div(class = "ai-bubble user", pend)),
        div(class = "ai-row agent", div(class = "ai-bubble agent", "…thinking"))
      ))
    }
    div(class = "ai-chat", bubbles)
  })

  # --- Send a question to /agent/chat (async so the spinner shows + app stays responsive) ---
  observeEvent(input$send_question, {
    q <- trimws(input$user_question)
    if (nchar(q) == 0) return()

    hist <- session_history()
    app_context <- list(
      tab        = current_context$tab,
      aoi        = current_context$aoi,
      sensor     = current_context$sensor,
      resolution = current_context$resolution,
      year       = current_context$year
    )
    body_list <- list(
      question    = q,
      history     = hist,
      app_context = app_context,
      provider    = if (!is.null(input$llm_provider)) input$llm_provider else "anthropic"
    )
    if (!is.null(input$user_api_key) && nzchar(input$user_api_key)) {
      body_list$user_api_key <- input$user_api_key
    }
    body_json <- jsonlite::toJSON(body_list, auto_unbox = TRUE, null = "null")

    pending_question(q)
    shinyjs::disable("send_question")
    updateTextAreaInput(session, "user_question", value = "")

    fallback <- "Sorry, I couldn't process that. Please try again."

    promises::finally(
      promises::catch(
        promises::then(
          promises::future_promise({
            resp <- httr::POST(
              paste0(API_BASE_URL, "/agent/chat"),
              body = body_json,
              httr::content_type_json(),
              httr::timeout(90)
            )
            list(status = httr::status_code(resp),
                 text   = httr::content(resp, "text", encoding = "UTF-8"))
          }),
          onFulfilled = function(res) {
            warn        <- "⚠️ "   # warning sign + VS16
            agent_chart <- NULL
            agent_table <- NULL
            status      <- res$status

            if (isTRUE(status == 401)) {
              answer <- paste0(warn, "**Authentication error** — your API key appears to be ",
                               "invalid or expired. Please check your key in the Settings panel above.")
            } else if (isTRUE(status == 400)) {
              answer <- paste0(warn, "**Request error** — the selected AI provider could not ",
                               "process the request. Try switching provider in Settings.")
            } else if (isTRUE(status >= 500)) {
              answer <- paste0(warn, "**Server error** — the data service is temporarily ",
                               "unavailable. Please try again in a moment.")
            } else if (isTRUE(status == 200)) {
              parsed <- tryCatch(jsonlite::fromJSON(res$text, simplifyVector = FALSE),
                                 error = function(e) NULL)
              if (!is.null(parsed) && !is.null(parsed$error) && nzchar(parsed$error)) {
                answer <- paste0(warn, parsed$error)
              } else if (!is.null(parsed) && !is.null(parsed$response) && nzchar(parsed$response)) {
                answer      <- strip_agent_tags(parsed$response)
                agent_chart <- parsed$chart
                agent_table <- parsed$table
              } else {
                answer <- paste0(warn, "The assistant returned an empty response. Please try again.")
              }
            } else {
              answer <- paste0(warn, "Could not reach the data service. ",
                               "Please check your connection and try again.")
            }

            n <- msg_counter() + 1; msg_counter(n); mid <- paste0("msg_", n)
            session_history(c(session_history(), list(
              list(role = "user",      content = q,      chart = NULL,        table = NULL,        msg_id = mid),
              list(role = "assistant", content = answer, chart = agent_chart, table = agent_table, msg_id = mid)
            )))
            ai_scroll_to_bottom()  # scroll + typeset LaTeX once the bubble renders
          }
        ),
        onRejected = function(e) {
          n <- msg_counter() + 1; msg_counter(n); mid <- paste0("msg_", n)
          err <- paste0("⚠️ Could not reach the data service. ",
                        "Please check your connection and try again.")
          session_history(c(session_history(), list(
            list(role = "user",      content = q,   chart = NULL, table = NULL, msg_id = mid),
            list(role = "assistant", content = err, chart = NULL, table = NULL, msg_id = mid)
          )))
        }
      ),
      onFinally = function() {
        pending_question(NULL)
        shinyjs::enable("send_question")
      }
    )
  })

  # --- Clear the conversation ---
  observeEvent(input$clear_history, {
    session_history(list())
    pending_question(NULL)
    msg_counter(0)
    registered_outputs(character(0))
  })

} # END SERVER
