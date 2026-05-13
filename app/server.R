
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
    choice_set =   list(NDVIexplorerTab = c("Mponda, Zambia" = "Zambia_Mponda", "Ancares Courel, Spain" = "Spain", 
                                            "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya"),
                        BAexplorerTab = c("West Lunga, Zambia" = "Zambia_WL")),
    selected_set = list(NDVIexplorerTab = "Zambia_Mponda", 
                        BAexplorerTab = "Zambia_WL")
  )
  
  resolutionchoices_rv <- reactiveValues(
    choice_set =   list(NDVIexplorerTab = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                            "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100"),
                        BAexplorerTab = c("500 (Terra MODIS)" = "500")),
    selected_set = list(NDVIexplorerTab = "Sentinel_1000", 
                        BAexplorerTab = "500")
  )
  
  # Observe selected tab and update choices according to the one selected
  observeEvent(input$tabs, { 
    updateSelectInput(session, "country",
                      choices = countrychoices_rv$choice_set[[input$tabs]], 
                      selected = countrychoices_rv$selected_set[[input$tabs]])
    updateSelectInput(session, "resolution",
                      choices = resolutionchoices_rv$choice_set[[input$tabs]], 
                      selected = resolutionchoices_rv$selected_set[[input$tabs]])
  })
  
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
  
  # Set Reactive values for AoI shape and error message
  aoi_shape_rv <- reactiveVal(NULL)
  error_message_rv <- reactiveVal(NULL)
  
  # Initialise year and month selector accounting for change of year
  observeEvent(TRUE, {
    current_year  <- lubridate::year(Sys.Date())
    current_month <- lubridate::month(Sys.Date())
    
    if (current_month == 1) {
      updateSelectInput(session, "year",
                        selected = current_year - 1)
    }
  }, once = TRUE)
  
  # Update month selector based on the selected year
  observeEvent(input$year, {
    req(input$year)
    
    if (input$year == lubridate::year(Sys.Date())) {
      updateSelectInput(session, "month",
                        choices = month.name[1:(lubridate::month(Sys.Date()) - 1)])
    } else {
      updateSelectInput(session, "month",
                        choices = month.name[1:12])
    }
  })
  
  # Create Leaflet map with AoI selected
  observeEvent(input$country, {
    req(input$country)
    error_message_rv(NULL) # Clear any previous errors
    
    tryCatch({
      aoi_files <- list.files(file.path(data_dir, "AoI"), pattern = paste0("AoI_.*", input$country, ".*\\.geojson$"))
      if (length(aoi_files) == 0) {
        stop("No Area of Interest file found for the selected country.")
      }
      shape <- sf::st_read(file.path(data_dir, "AoI", aoi_files[[1]]))
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
  ndvi_ts_stats <- reactiveVal(NULL)
  
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
        ndvi_anomaly_titles_ui(input$resolution),
        plotlyOutput("ndvi_ts_plot_output", height = "550px"),
        height = "auto"
      )
    } else {
      return(NULL) # Return empty UI
    }
  })
  
  output$wilcoxon_card <- renderUI({
    s <- ndvi_ts_stats()
    if (is.null(s) || !isTRUE(ndvi_ts_ready())) {
      return(NULL)
    }
    ndvi_insight_wilcox_card_ui(s)
  })
  
  output$smk_card <- renderUI({
    s <- ndvi_ts_stats()
    if (is.null(s) || !isTRUE(ndvi_ts_ready())) {
      return(NULL)
    }
    ndvi_insight_smk_card_ui(s)
  })
  
  # Clear the image when switching tabs or subtabs
  observeEvent(list(input$tabs, input$ndvisubtabs), {
    ndvi_ts_ready(FALSE)
    error_message_rv(NULL)
    ndvi_ts_plot_obj(NULL)
    ndvi_ts_stats(NULL)
  })
  
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

    # Handling to avoid end/start of new year errors
    if(input$year == lubridate::year(Sys.Date())) {
      end_month <- lubridate::month(Sys.Date()) - 1
      end_year <- input$year
      if(end_month == 0) {
        end_month <- 12
        end_year <- end_year - 1
      }
    } else {
      end_month <- 12
      end_year <- input$year
    }
    
    # Wrap data generation in tryCatch to handle missing files/errors
    tryCatch({
      
      # Ensure the figures directory exists (used by other exports; NDVI TS is plotly)
      if (!dir.exists(figures_dir)) {
        dir.create(figures_dir, recursive = TRUE)
      }
      
      ndvi_result <- generate_timeseries(
        country_name    = country_name,
        resolution      = resolution,
        end_year        = end_year,
        end_month       = end_month,
        figures_dir     = figures_dir,
        data_dir        = data_dir,
        return_plot     = TRUE,
        figure_filename = NULL
      )
      
      ndvi_ts_ready(TRUE)
      ndvi_ts_plot_obj(ndvi_result$plot)
      ndvi_ts_stats(ndvi_result$stats)
      error_message_rv(NULL) # Clear any previous error messages
      
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_ts_ready(FALSE)
      ndvi_ts_plot_obj(NULL)
      ndvi_ts_stats(NULL)
      error_message_rv(e$message)
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Optionally, also log the error to the console for debugging
      message("---Error generating NDVI timeseries (old message)---", "\n",
              "An error occurred while generating or reading the NDVI timeseries data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("End Year:", end_year), "\n",
              paste("End Month:", end_month), "\n",
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir))
      
      # Optionally, also log the error to the console for debugging
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
      return(NULL) # Return empty UI
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
    
    # Handling to avoid end/start of new year errors
    if(input$year == lubridate::year(Sys.Date())) {
      end_month <- lubridate::month(Sys.Date()) - 1
      end_year <- input$year
      if(end_month == 0) {
        end_month <- 12
        end_year <- end_year - 1
      }
    } else {
      end_month <- 12
      end_year <- input$year
    }
    
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
      
      htmlwidgets::saveWidget(
        lc_result$plot,
        file.path(figures_dir, lc_figure_filename),
        selfcontained = TRUE
      )
      
      ndvi_lc_ready(TRUE)
      error_message_rv(NULL)
      
    }, error = function(e) {
      ndvi_lc_ready(FALSE)
      error_message_rv(e$message)
      lc_ts_plot_obj(NULL)
      lc_lc_highlight(NULL)
      lc_plot_year(NULL)
      lc_map_bbox_by_stem(NULL)
      
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      message("---Error generating NDVI Land Cover---", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("End Year:", end_year), "\n",
              paste("End Month:", end_month), "\n",
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir), "\n",
              paste("Land Cover Figure Directory:", lc_figure_path))
      
      message("Error generating Land Cover NDVI / map: ", e$message)
    })
    message("=========== End of NDVI Land Cover Generation =============")
  })

  
  # ---------------------------------------------------------------------------------------------------
  # NDVI DELTA MAP AND CHARTS
  # --------------------------------------------------------------------------------------------------- 

  # Reactive flag to control whether the NDVI Delta Map UI should be shown
  ndvi_dm_ready <- reactiveVal(FALSE)
  
  # Render a container for the plot or error message
  output$dm_plot_container <- renderUI({
    error_msg <- error_message_rv()
    
    if (is.null(error_msg) && isTRUE(ndvi_dm_ready())) { # If no errors and the reactive flag is TRUE (after successful figure generation), show output, otherwise empty
      fluidRow(
        column(8, div(class = "image-fill top-center",
                      imageOutput("ndvi_histmap_output"), height = "100%")),
        column(4, htmlOutput("ndvi_delta_map_output"))
      )
    } else {
      return(NULL) # Return empty UI
    }
  })
  
  # Clear the image when switching tabs or subtabs
  observeEvent(list(input$tabs, input$ndvisubtabs), {
    ndvi_dm_ready(FALSE)
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
      if (!file.exists(figure_path)) {
        if (!dir.exists(figures_dir)) {
          dir.create(figures_dir, recursive = TRUE)
        }
        generate_2Dmap(country_name, resolution, map_year, map_month, figures_dir, data_dir, FALSE, FALSE, figure_filename)
      }
      output$ndvi_histmap_output <- renderImage({
        list(src = figure_path, 
             alt = "NDVI 2D map")
      }, deleteFile = FALSE)
    
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_dm_ready(FALSE)
      error_message_rv(e$message)
      output$ndvi_histmap_output <- NULL
      output$ndvi_delta_map_output <- renderUI(NULL) 
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Optionally, also log the error to the console for debugging
      message("---Error generating NDVI Delta Map (old message)---", "\n",
              "An error occurred while generating or reading the NDVI timeseries data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("Map Year:", map_year), "\n",
              paste("Map Month:", map_month), "\n", 
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir), "\n",
              paste("Delta Map Figure Directory:", figure_path))
      
      message("Error generating static NDVI map: ", e$message)
    })

    # NDVI Delta Map
    figure_filename_dm <- paste0("figure_deltaNDVImaps_", country_name, "_", map_month, "_", map_year, "_", resolution, "m.html")
    figure_path_dm <- file.path(figures_dir, figure_filename_dm)

    tryCatch({
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
      error_message_rv(NULL) # Clear any previous error messages
      
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_dm_ready(FALSE)
      error_message_rv(e$message)
      output$ndvi_histmap_output <- NULL
      output$ndvi_delta_map_output <- renderUI(NULL) 
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Optionally, also log the error to the console for debugging
      message("---Error generating NDVI Delta Map (old message)---", "\n",
              "An error occurred while generating or reading the NDVI timeseries data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("Map Year:", map_year), "\n",
              paste("Map Month:", map_month), "\n", 
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir), "\n",
              paste("Delta Map Figure Directory:", figure_path))
      
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
    data_path    <- file.path(data_dir, "BurnedArea", country_name, paste0(resolution, "m_resolution"))
    ba_files     <- tryCatch(
      get_filenames(filepath = data_path, data_type = "BurnedArea",
                    file_extension = ".tif", country_name = country_name),
      error = function(e) character(0)
    )
    available_years <- if (length(ba_files) > 0) {
      sort(unique(as.integer(sub("^(\\d{4})-.*", "\\1", ba_files))), decreasing = TRUE)
    } else {
      seq(2018, lubridate::year(Sys.Date()))
    }
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
    
    # Handling to avoid end/start of new year errors
    if(input$year == lubridate::year(Sys.Date())) {
      end_month <- lubridate::month(Sys.Date()) - 2
      end_year <- input$year
      if(end_month <= 0) {
        end_month <- 12
        end_year <- end_year - 1
      }
    } else {
      end_month <- 12
      end_year <- input$year
    }
    
    tryCatch({
      ba_plot <- generate_ba_timeseries(
        country_name    = country_name,
        resolution      = resolution,
        end_year        = end_year,
        end_month       = end_month,
        figures_dir     = figures_dir,
        data_dir        = data_dir,
        return_plot     = TRUE,
        figure_filename = NULL
      )
      ba_ts_plot_obj(ba_plot)
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
      if (!file.exists(figure_path_bam)) {
        if (!dir.exists(figures_dir)) {
          dir.create(figures_dir, recursive = TRUE)
        }
        # Generate figure and create GeoJSON file, store path
        generate_ba_2Dmap(country_name, resolution, map_year, map_month, figures_dir, data_dir, FALSE, FALSE, figure_filename_bam)
        geojson_export_path <- generate_ba_export(country_name, resolution, map_year, map_month, figures_dir, data_dir, figure_filename_bam)
        
        # Update reactive values for the download button
        geojson_export_path_rv(geojson_export_path)
        map_generated(TRUE)
      }
      output$ba_map_output <- renderImage({
        list(src = figure_path_bam,
             width = "100%",
             alt = "Burned Area 2D map")
      }, deleteFile = FALSE)
      
      # Create GeoJSON export path (same logic as in the function) and update reactive values
      geojson_export_path <- file.path(figures_dir, paste0(tools::file_path_sans_ext(figure_filename_bam), ".geojson"))
      geojson_export_path_rv(geojson_export_path)
      
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
      # Capture current values for the renderer (avoids reactive dependency)
      local({
        captured_geojson <- geojson_export_path_rv()
        captured_year    <- map_year
        captured_month   <- map_month
        captured_country <- country_name

        output$ba_monthly_leaflet <- renderLeaflet({
          build_ba_monthly_leaflet(
            geojson_path = captured_geojson,
            data_dir     = data_dir,
            country      = captured_country,
            year         = captured_year,
            month_num    = captured_month
          )
        })
      })

      shinyjs::show("monthly_leaflet_wrap")
      ba_map_ready(TRUE)
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

  # ------- Show/hide sidebar year & month when toggling BA map views --------

  observeEvent(input$ba_map_view, {
    if (isTRUE(input$ba_map_view == "frp")) {
      shinyjs::hide("year")
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
      path <- geojson_export_path_rv()
      req(path, file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    }
  )
    
} # END SERVER
