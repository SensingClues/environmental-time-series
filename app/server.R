
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
  
  # Start  NDVI Timeseries Tab
  
  # Code to adjust input choices based on subtab (not entirely working but good inspiration)
  # countrychoices_rv <- reactiveValues(
  #   choise_set = list(
  #     NDVItsTab = c("Mponda, Zambia" = "Zambia", "Ancares Courel, Spain" = "Spain", 
  #                   "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya"),
  #     NDVIdeltaTab = c("Mponda, Zambia" = "Zambia", "Ancares Courel, Spain" = "Spain", 
  #                      "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya"),
  #     LCexplorerTab = c("Mponda, Zambia" = "Zambia", "Ancares Courel, Spain" = "Spain", 
  #                       "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya"),
  #     BAtimeseries = c("West Lunga, Zambia" = "Zambia_WL"),
  #     BAmapexplorer = c("West Lunga, Zambia" = "Zambia_WL")
  #   ),
  #   selected_set = list(NDVItsTab = "Zambia", NDVIdeltaTab = "Zambia", LCexplorerTab = "Zambia",
                        # BAtimeseries = "Zambia_WL",BAmapexplorer = "Zambia_WL"))
  # 
  # resolutionchoices_rv <- reactiveValues(
  #   choise_set = list(
  #     NDVItsTab = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
  #                   "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100"),
  #     NDVIdeltaTab = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
  #                      "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100"),
  #     LCexplorerTab = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
  #                       "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100"),
  #     BAtimeseries = c("500 (Terra MODIS)" = "500"),
  #     BAmapexplorer = c("500 (Terra MODIS)" = "500")
  #   ),
  #   selected_set = list(NDVItsTab = "Sentinel_1000", NDVIdeltaTab = "Sentinel_1000", LCexplorerTab = "Sentinel_1000",
                        # BAtimeseries = "500", BAmapexplorer = "500"))
  #
  # observeEvent(c(input$tabs, input$basubtabs, input$ndvisubtabs), {
  #   if (input$tabs == "NDVIexplorerTab") {
  #     updateSelectInput(session, "country",
  #                       choices = countrychoices_rv$choise_set[[input$ndvisubtabs]], 
  #                       selected = countrychoices_rv$selected_set[[input$ndvisubtabs]])
  #     updateSelectInput(session, "resolution",
  #                       choices = resolutionchoices_rv$choise_set[[input$ndvisubtabs]], 
  #                       selected = resolutionchoices_rv$selected_set[[input$ndvisubtabs]])
  #   } else if (input$tabs == "BAexplorerTab") {
  #     updateSelectInput(session, "country",
  #                       choices = countrychoices_rv$choise_set[[input$basubtabs]], 
  #                       selected = countrychoices_rv$selected_set[[input$basubtabs]])
  #     updateSelectInput(session, "resolution",
  #                       choices = resolutionchoices_rv$choise_set[[input$basubtabs]], 
  #                       selected = resolutionchoices_rv$selected_set[[input$basubtabs]])}})
  
  # Update selector inputs based on the selected tab
  countrychoices_rv <- reactiveValues(
    choise_set =   list(NDVIexplorerTab = c("Mponda, Zambia" = "Zambia_Mponda", "Ancares Courel, Spain" = "Spain", 
                                            "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya"),
                        BAexplorerTab = c("West Lunga, Zambia" = "Zambia_WL")),
    selected_set = list(NDVIexplorerTab = "Zambia_Mponda", 
                        BAexplorerTab = "Zambia_WL")
  )
  
  resolutionchoices_rv <- reactiveValues(
    choise_set =   list(NDVIexplorerTab = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                            "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100"),
                        BAexplorerTab = c("500 (Terra MODIS)" = "500")),
    selected_set = list(NDVIexplorerTab = "Sentinel_1000", 
                        BAexplorerTab = "500")
  )
  
  observeEvent(input$tabs, {
    updateSelectInput(session, "country",
                      choices = countrychoices_rv$choise_set[[input$tabs]], 
                      selected = countrychoices_rv$selected_set[[input$tabs]])
    updateSelectInput(session, "resolution",
                      choices = resolutionchoices_rv$choise_set[[input$tabs]], 
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
  
  # Set Reactive values
  aoi_shape_rv <- reactiveVal(NULL)
  error_message_rv <- reactiveVal(NULL)
  
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
      
    }, error = function(e) {
      error_message_rv(e$message)
      aoi_shape_rv(NULL) # Set to NULL on error
    })
  }, ignoreNULL = TRUE, ignoreInit = FALSE) # 'ignoreInit = FALSE' makes it run on startup
  
  output$map <- renderLeaflet({
    shape <- aoi_shape_rv()
    if (is.null(shape)) { # Return empty map if no shape
      return(leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addTiles())
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
  # Render a container for the plot or error message
  output$ndvi_ts_plot_container <- renderUI({
    # Default UI is just an image placeholder
    div(class = "image-fill top-center",
        imageOutput("ndvi_ts_plot_output"), height = "auto")
  })
  
  # Observe the Generate Figure button
  observeEvent(input$generate_ndvi_ts_figures, {
    
    # Get user inputs
    country_name <- input$country
    resolution <- input$resolution

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
    figure_filename <- paste0("figure_NDVItimeseries_", country_name, "_", 
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
        generate_timeseries(
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
      output$ndvi_ts_plot_output <- renderImage({
        list(
          src   = figure_path,
          alt   = "NDVI timeseries"
        )
      }, deleteFile = FALSE)
      
    }, error = function(e) {
      
      # If an error occurs (commonly missing data), replace the default UI with a message
      output$ndvi_ts_plot_container <- renderUI({
        div(
          style = "color: red; margin-left: 10px; margin-top: 20px;",
          strong("Error: "),
          "An error occurred while generating or reading the NDVI timeseries data. ",
          "This may be due to missing files or incorrect file paths. ",
          "Please verify that the necessary data files exist in '", data_dir, "'.",
          br(), br(),
          paste("Details:", e$message), br(),
          paste("Country Name:", country_name), br(), 
          paste("Resolution:", resolution), br(),
          paste("End Year:", end_year), br(),
          paste("End Month:", end_month), br(),
          paste("Figures Directory:", figures_dir), br(),
          paste("Data Directory:", data_dir))
      })
      
      # Optionally, also log the error to the console for debugging
      message("Error generating NDVI timeseries: ", e$message)
    })
  })
  
  #### End NDVI timeseries part
  
  # ---------------------------------------------------------------------------------------------------
  # NDVI LAND COVER EXPLORER SERIES CHART
  # ---------------------------------------------------------------------------------------------------
  
  # Render a container for the plot or error message
  output$lc_plot_container <- renderUI({
    # Default UI is just an image placeholder
    fluidRow(
      column(7, 
             div(class = "image-fill top-center",
                 imageOutput("ndvi_plot_output"), height = "100%")),
      column(5, htmlOutput("landcover_map_output"))
    )
  })
  
  # Observe the Generate Figure button
  observeEvent(input$generate_lc_figures, {
    
    # Get user inputs
    country_name <- input$country
    # end_month <- ifelse(input$year == lubridate::year(Sys.Date()), lubridate::month(Sys.Date())-1, 12)
    # end_year <- input$year
    resolution <- input$resolution
    landcover_Type <- input$landcover_Type
    
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
        list(src   = figure_path,
             alt   = "Land Cover")
      }, deleteFile = FALSE)
      
    }, error = function(e) {
      
      # If an error occurs (commonly missing data), replace the default UI with a message
      output$ndvi_plot_output <- renderUI({
        div(
          style = "color: red; margin-left: 10px; margin-top: 20px;",
          strong("Error: "),
          "An error occurred while generating or reading the Land Cover NDVI Timeseries data. ",
          "This may be due to missing files or incorrect file paths. ",
          "Please verify that the necessary data files exist in '", data_dir, "'.",
          br(), br(),
          paste("Details:", e$message)
        )
      })
      
      # Optionally, also log the error to the console for debugging
      message("Error generating Land Cover NDVI Timeseries: ", e$message)
    })
    
    #### Land Cover map
    # Get user inputs
    country_name <- input$country
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
      
    }, error = function(e) {
      
      # In case of error (likely missing data files), display a helpful message in the UI
      output$landcover_map_output <- renderUI({
        div(
          style = "color: red; margin-left: 10px; margin-top: 10px;",
          strong("Error: "),
          "An error occurred while generating or reading the land use geojson data. ",
          "This may be due to missing files or incorrect file paths. ",
          "Please verify that the necessary data files exist in '", data_dir, "'.",
          br(), 
          br(),
          paste("Details:", e$message)
        )
      })
      
      # You could also log the error or print it to console
      message("Error generating land use map: ", e$message)
    })
  })

  # ---------------------------------------------------------------------------------------------------
  # NDVI DELTA MAP AND CHARTS
  # --------------------------------------------------------------------------------------------------- 

  # Render a container for the plot or error message
  output$dm_plot_container <- renderUI({
    # Default UI is just an image placeholder
    fluidRow(
      column(8, div(class = "image-fill top-center",
                    imageOutput("ndvi_histmap_output"), height = "100%")),
      column(4, htmlOutput("ndvi_delta_map_output"))
    )
  })  
  
  observeEvent(input$generate_ndvi_delta_plot, {
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
      output$ndvi_histmap_output <- renderUI({
        div(style = "color: red;", strong("Error: "), "An error occurred while generating the NDVI data. Check that the necessary data files exist.", br(), br(), paste("Details:", e$message))
      })
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
    }, error = function(e) {
      output$ndvi_delta_map_output <- renderUI({
        div(style = "color: red;", strong("Error: "), "An error occurred while generating the delta NDVI data. Check that the necessary data files exist.", br(), br(), paste("Details:", e$message))
      })
      message("Error generating delta NDVI map: ", e$message)
    })
  })
 
  # ---------------------------------------------------------------------------------------------------
  # BURNED AREA MAP AND EXPLORER FUNCTIONALITY
  # ---------------------------------------------------------------------------------------------------  

  # Render a container for the timeseries plot or error message
  output$ba_plot_container <- renderUI({
    # Default UI is just an image placeholder
    div(class = "image-fill top-center",
        imageOutput("ba_plot_output"), height = "auto")
  })  
  
  # Render a container for the plot or error message
  output$ba_map_container <- renderUI({
    # Default UI is just an image placeholder
    fluidRow(
      column(7, div(class = "image-fill top-center",
                    imageOutput("ba_map_output"), height = "100%")),
      column(5, htmlOutput("ba_leaflet_map"))
    )
  }) 
    
  # Set Reactive values
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
    
    # BA Plot
    # Get user inputs
    country_name <- input$country
    #end_month <- ifelse(input$year == lubridate::year(Sys.Date()), lubridate::month(Sys.Date())-1, 12)
    #end_year <- input$year
    resolution <- input$resolution
    
    if(input$year == lubridate::year(Sys.Date())) {
      end_month <- lubridate::month(Sys.Date()) - 1
      end_year <- input$year
      if(end_month == 0) {
        end_month <- 1
        # end_year <- end_year - 1
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
    }, error = function(e) {
      # If an error occurs (commonly missing data), replace the default UI with a message
      output$ba_plot_output <- renderUI({
        div(
          style = "color: red; margin-top: 20px;",
          strong("Error: "),
          "An error occurred while generating or reading the Burned Area timeseries data. ",
          "This may be due to missing files or incorrect file paths. ",
          "Please verify that the necessary data files exist in '", data_dir, "'.",
          br(), br(),
          paste("Details:", e$message), br(),
          paste("Country Name:", country_name), br(),
          paste("Resolution:", resolution), br(),
          paste("End Year:", end_year), br(),
          paste("End Month:", end_month), br(),
          paste("Figures Directory:", figures_dir), br(),
          paste("Data Directory:", data_dir))
      })
      
      # Optionally, also log the error to the console for debugging
      message("Error generating Burned Area timeseries: ", e$message)
    })
  })
  
  # Observe the Generate Figure button
  observeEvent(input$generate_ba_map_figures, {
    
    # BA Map
    # Get user inputs
    country_name <- input$country
    resolution <- input$resolution
    map_month <- match(input$month, month.name)
    map_year <- input$year
    
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
      map_generated(TRUE)
    }, error = function(e) {
      output$ba_map_output <- renderUI({
        div(style = "color: red;", strong("Error: "), "An error occurred while generating the Burned Area data. Check that the necessary data files exist.", br(), br(), paste("Details:", e$message))
      })
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
    }, error = function(e) {
      # In case of error (likely missing data files), display a helpful message in the UI
      output$ba_leaflet_map <- renderUI({
        div(style = "color: red; margin-left: 10px; margin-top: 10px;",
            strong("Error: "),
            "An error occurred while generating or reading the Burned Area geojson data. ",
            "This may be due to missing files or incorrect file paths. ",
            "Please verify that the necessary data files exist in '", data_dir, "'.", br(), br(),
            paste("Details:", e$message)
        )})      
      # You could also log the error or print it to console
      message("Error generating Burned Area map: ", e$message)
    })
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
