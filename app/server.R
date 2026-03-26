
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
  
  # Render a container for the plot or error message
  output$lc_plot_container <- renderUI({
    error_msg <- error_message_rv()
    
    if (is.null(error_msg) && isTRUE(ndvi_lc_ready())) { # If no errors and the reactive flag is TRUE (after successful figure generation), show output, otherwise empty
      fluidRow(
        column(7, 
               div(class = "image-fill top-center",
                   imageOutput("ndvi_plot_output"), height = "100%")),
        column(5, uiOutput("landcover_map_output"))
      )
    } else {
      return(NULL) # Return empty UI
    }
  })
  
  # Clear the image when switching tabs or subtabs
  observeEvent(list(input$tabs, input$ndvisubtabs), {
    ndvi_lc_ready(FALSE)
    error_message_rv(NULL)
    
    # Clear server outputs so nothing can re-appear
    output$ndvi_plot_output <- NULL
    output$landcover_map_output <- renderUI(NULL)   
  }, ignoreInit = TRUE)
  
  # Observe the Generate Figure button
  observeEvent(input$generate_lc_figures, {
    message("=========== Starting NDVI Land Cover Generation =============")
    
    # To be extra sure that no figure is shown, clear previous error messages
    ndvi_lc_ready(FALSE)
    error_message_rv(NULL)
    
    # Get user inputs
    country_name <- input$country
    resolution <- input$resolution
    landcover_Type <- input$landcover_Type
    
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
    
    # Define script and figure paths
    figure_filename <- paste0("figure_landCover_", country_name, "_", 
                              end_month, "_", end_year, "_", resolution, "m", "_", landcover_Type, ".png")
    figure_path <- file.path(figures_dir, figure_filename)
    
    # Wrap data generation in tryCatch to handle missing files/errors
    tryCatch({
      
      # If figure not stored yet, attempt to generate it
      if (!file.exists(figure_path)) {
        
        # Ensure the figures directory exists
        if (!dir.exists(figures_dir)) {
          dir.create(figures_dir, recursive = TRUE)
        }
        
        # Create timeseries plot
        generate_timeseries_landcover(
          country_name   = country_name,
          resolution     = resolution,
          end_year       = end_year,
          end_month      = end_month,
          figures_dir    = figures_dir,
          data_dir       = data_dir,
          land_use_src   = "S2_10m_LULC_2023",
          land_cover_type = landcover_Type,
          return_plot    = FALSE,
          figure_filename= figure_filename
        )
      }
      
      # If no error so far, render the image
      output$ndvi_plot_output <- renderImage({
        list(src = figure_path, alt = "Land Cover")
      }, deleteFile = FALSE)
      
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_lc_ready(FALSE)
      error_message_rv(e$message)
      output$ndvi_plot_output <- NULL
      output$landcover_map_output <- renderUI(NULL)
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Optionally, also log the error to the console for debugging
      message("---Error generating NDVI Land Cover (old message)---", "\n",
              "An error occurred while generating or reading the NDVI timeseries data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("End Year:", end_year), "\n",
              paste("End Month:", end_month), "\n", 
              paste("Land Cover Type:", landcover_Type), "\n",
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir), "\n",
              paste("Land Cover Figure Directory:", lc_figure_path))
      
      # Optionally, also log the error to the console for debugging
      message("Error generating Land Cover NDVI Timeseries: ", e$message)
    })
    
    #### Land Cover map
    # Get additional inputs for folder selection
    map_year <- "2023"
    vector_src <- "S2_10m_LULC"
    
    # Input geojsons stored in country folder. 
    data_path <- paste0(data_dir, "/", "LandUse", "/", 
                        country_name, "/", 
                        vector_src, "_", map_year, "/")
    
    # Define script and figure paths
    lc_figure_filename <- paste0("figure_LULCmap_", country_name, "_", vector_src, "_", map_year, ".html")
    lc_figure_path <- file.path(figures_dir, lc_figure_filename)
    
    # Use tryCatch to handle missing data or any errors
    tryCatch({
      
      # If figure not stored yet, try to generate it
      if (!file.exists(lc_figure_path)) {
        
        # Ensure the figures directory exists
        if (!dir.exists(figures_dir)) {
          dir.create(figures_dir, recursive = TRUE)
        }
        
        # Create explorer plot
        plot_geojsons_from_a_folder(
          folder_path = data_path,
          save_path = figures_dir,
          filename = lc_figure_filename
        )
      }
      
      # Render the generated (or existing) HTML
      output$landcover_map_output <- renderUI({
        tags$iframe(
          src = paste0("figures/", lc_figure_filename),
          width = "100%",
          height = "500px",
          frameborder = 0
        )
      })
      
      ndvi_lc_ready(TRUE) # Mark UI as ready as both figures are generated (renderUI will now return the container)
      error_message_rv(NULL) # Clear any previous error messages

    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ndvi_lc_ready(FALSE)
      error_message_rv(e$message)
      output$ndvi_plot_output <- NULL
      output$landcover_map_output <- renderUI(NULL)
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Optionally, also log the error to the console for debugging
      message("---Error generating NDVI Land Cover (old message)---", "\n",
              "An error occurred while generating or reading the NDVI timeseries data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("End Year:", end_year), "\n",
              paste("End Month:", end_month), "\n", 
              paste("Land Cover Type:", landcover_Type), "\n",
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir), "\n",
              paste("Land Cover Figure Directory:", lc_figure_path))
      
      # You could also log the error or print it to console
      message("Error generating land use map: ", e$message)
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

  # Reactive flag to control whether the BA timeseries UI should be shown
  ba_ts_ready <- reactiveVal(FALSE)
  
  # Render a container for the plot or error message
  output$ba_plot_container <- renderUI({
    error_msg <- error_message_rv()
    
    if (is.null(error_msg) && isTRUE(ba_ts_ready())) { # If no errors and the reactive flag is TRUE (after successful figure generation), show output, otherwise empty
      div(class = "image-fill top-center",
          imageOutput("ba_plot_output"), height = "auto")
    } else {
      return(NULL) # Return empty UI
    }
  })
  
  # Clear the image when switching tabs or subtabs
  observeEvent(list(input$tabs, input$basubtabs), {
    ba_ts_ready(FALSE)
    error_message_rv(NULL) 
    
    # Clear server outputs so nothing can re-appear
    output$ba_plot_output <- NULL
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
    
    # Define script and figure paths
    figure_filename <- paste0("figure_BurnedAreatimeseries_", country_name, "_", 
                              end_month, "_", end_year, "_", resolution, "m", ".png")
    figure_path <- file.path(figures_dir, figure_filename)
    
    # Wrap data generation in tryCatch to handle missing files/errors
    tryCatch({
      
      # If figure not stored yet, attempt to generate it
      if (!file.exists(figure_path)) {
        # Ensure the figures directory exists
        if (!dir.exists(figures_dir)) {
          dir.create(figures_dir, recursive = TRUE)
        }
        
        # Create timeseries plot
        generate_ba_timeseries(
          country_name   = country_name,
          resolution     = resolution,
          end_year       = end_year,
          end_month      = end_month,
          figures_dir    = figures_dir,
          data_dir       = data_dir,
          return_plot    = FALSE,
          figure_filename= figure_filename
        )
      }
      
      # If no error so far, render the image
      output$ba_plot_output <- renderImage({
        list(src = figure_path, 
             # width = "100%", 
             alt = "Burned Area timeseries")
      }, deleteFile = FALSE)
      
      ba_ts_ready(TRUE) # Mark UI as ready (renderUI will now return the container)
      error_message_rv(NULL) # Clear any previous error messages
      
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ba_ts_ready(FALSE)
      error_message_rv(e$message) 
      output$ba_plot_output <- NULL
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Also log the error to the console for debugging
      message("---Error generating Burned Area timeseries (old message)---", "\n",
              "An error occurred while generating or reading the Burned Area timeseries data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("End Year:", end_year), "\n",
              paste("End Month:", end_month), "\n",
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir))
      
      # Also log the error to the console for debugging
      message("Error generating Burned Area timeseries: ", e$message)
    })
    message("=========== End of Burned Area Time Series Generation =============")
  })
  
  
  #######################################################################
  ########### BA Map Explorer ###########################################
  #######################################################################
  
  # Reactive flag to control whether the BA Map UI should be shown
  ba_map_ready <- reactiveVal(FALSE)
  
  # Render a container for the plot or error message
  output$ba_map_container <- renderUI({
    error_msg <- error_message_rv()
    
    if (is.null(error_msg) && isTRUE(ba_map_ready())) { # If no errors and the reactive flag is TRUE (after successful figure generation), show output, otherwise empty
      fluidRow(
        column(7, div(class = "image-fill top-center",
                      imageOutput("ba_map_output"), height = "100%")),
        column(5, uiOutput("ba_leaflet_map"))
      )
    } else {
      return(NULL) # Return empty UI
    }
  })
  
  # Clear the image when switching tabs or subtabs
  observeEvent(list(input$tabs, input$basubtabs), {
    ba_map_ready(FALSE)
    error_message_rv(NULL) 
    
    # Clear server outputs so nothing can re-appear
    output$ba_map_output <- NULL
    output$ba_leaflet_map <- NULL
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
             # width = "100%", 
             alt = "Burned Area 2D map")
      }, deleteFile = FALSE)
      
      # Create GeoJSON export path (same logic as in the function) and update reactive values
      geojson_export_path <- file.path(figures_dir, paste0(tools::file_path_sans_ext(figure_filename_bam), ".geojson"))
      geojson_export_path_rv(geojson_export_path)
      
      map_generated(TRUE) # Mark map generated as TRUE to enable the GeoJSON download button
      error_message_rv(NULL) # Clear any previous error messages
      
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ba_map_ready(FALSE)
      error_message_rv(e$message)
      output$ba_map_output <- NULL
      output$ba_leaflet_map <- NULL
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Optionally, also log the error to the console for debugging
      message("---Error generating BA Map (old message)---", "\n",
              "An error occurred while generating or reading the BA map data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("Map Year:", map_year), "\n",
              paste("Map Month:", map_month), "\n", 
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir), "\n",
              paste("BA Map Figure Directory:", figure_path_bam), "\n",
              paste("GeoJSON Export Directory:", geojson_export_path_rv()))
      
      message("Error generating static Burned Area map: ", e$message)
    })
    
    #### BA Leaflet map
    # Define script and figure paths
    ba_figure_input_filename <- paste0("figure_BurnedAreamaps_", country_name, "_", 
                                       map_month, "_", map_year, "_", resolution, "m.geojson")
    ba_figure_output_filename <- paste0("figure_BurnedAreamaps_", country_name, "_", 
                                        map_month, "_", map_year, "_", resolution, "m.html")
    ba_figure_path <- file.path(figures_dir, ba_figure_output_filename)
    ba_figure_input_path <- file.path(figures_dir, ba_figure_input_filename)
    
    # Use tryCatch to handle missing data or any errors
    tryCatch({
      # If figure not stored yet, try to generate it
      if (!file.exists(ba_figure_path)) {
        
        # Ensure the figures directory exists
        if (!dir.exists(figures_dir)) {
          dir.create(figures_dir, recursive = TRUE)
        }
        
        # Create explorer plot
        plot_ba_geojson_from_a_folder(
          input_file_path = ba_figure_input_path,
          data_dir = data_dir,
          country = country_name,
          save_path = figures_dir,
          filename = ba_figure_output_filename)
      }
      
      # Render the generated (or existing) HTML
      output$ba_leaflet_map <- renderUI({
        tags$iframe(
          src = paste0("figures/", ba_figure_output_filename),
          width = "100%",
          height = "400px",
          frameborder = 0)
      })
      
      ba_map_ready(TRUE) # Mark UI as ready when both figures are rendered successfully (renderUI will now return the container)
      error_message_rv(NULL) # Clear any previous error messages
    }, error = function(e) {
      # Clear server outputs so nothing can re-appear
      ba_map_ready(FALSE)
      error_message_rv(e$message)
      output$ba_map_output <- NULL
      output$ba_leaflet_map <- NULL
      
      # Show error notification to user
      showNotification(HTML("The figure cannot be generated due to missing GeoJSON data. 
       Please contact us at 
       <a href='mailto:helpdesk@sensingclues.org'>helpdesk@sensingclues.org</a> for assistance."), 
                       type = "error", 
                       duration = 6)
      
      # Optionally, also log the error to the console for debugging
      message("---Error generating BA Map or export (old message)---", "\n",
              "An error occurred while generating or reading the BA map data. ", "\n",
              "This may be due to missing files or incorrect file paths. ", "\n",
              "Please verify that the necessary data files exist in '", data_dir, "'.", "\n",
              paste("Details:", e$message), "\n",
              paste("Country Name:", country_name), "\n",
              paste("Resolution:", resolution), "\n",
              paste("Map Year:", map_year), "\n",
              paste("Map Month:", map_month), "\n", 
              paste("Figures Directory:", figures_dir), "\n",
              paste("Data Directory:", data_dir), "\n",
              paste("BA Map Figure Directory:", figure_path_bam), "\n",
              paste("GeoJSON Export Directory:", geojson_export_path_rv()))
      
      # You could also log the error or print it to console
      message("Error generating Burned Area map/export: ", e$message)
      })   
    message("=========== End of Burned Area Map Generation =============")
    })
  
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
