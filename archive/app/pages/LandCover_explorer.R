library(shiny)
library(ggplot2)
library(terra)
library(leaflet)
library(tidyr)
library(dplyr)
library(plotly)
library(htmlwidgets)

# Dynamically detect the directory of the app.R file
app_dir <- normalizePath(getwd()) # Works in deployed apps or locally

# Define paths relative to app.R
source("utils.R")
source("visualize.R")
source("generate_plots.R")

# UI function for Land Cover Dashboard
landCoverUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    sidebarLayout(
      sidebarPanel(
        # Title and description
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            h2(class = "text-3xl font-bold text-white-800 mb-4", "Time Series x Land Cover: NDVI"),
            p(class = "text-lg text-white-700 leading-relaxed text-justify mb-2", 
              "Generate and explore the Normalized Difference Vegetation Index (NDVI) values for a specific land cover type, averaged over a selected area of interest. Select the year and month for which you want to analyze the 12 months preceding your selection.",
              a(class = "text-blue-500 hover:underline", "Read more", href = "http://sensingclues.org/environmental-time-series-about")),
        ),
        # Controls for user input
        div(class = "controls max-w-4xl mx-auto px-6 py-4",
            selectInput(ns("country"), "Select Project Area:", 
                        choices = c("Mponda, Zambia" = "Zambia", "Ancares Courel, Spain" = "Spain", 
                                    "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya")), # Add more countries as needed
            selectInput(ns("year"), "Select Year:", selected = lubridate::year(Sys.Date()), choices = seq(2018, lubridate::year(Sys.Date()), 1)),
            selectInput(ns("month"), "Select Month:", selected="January" , choices = month.name[1:lubridate::month(Sys.Date())-1]),
            selectInput(ns("resolution"), "Select spatial resolution (m):", 
                        selected = "100 (ESA Sentinel-2)", 
                        choices = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                    "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100")),
            selectInput(ns("land_cover"), "Select Land Cover Source:", 
                        selected = "LULC, 2023",
                        choices = c("LULC, 2023" = "S2_10m_LULC,2023")),
            selectInput(ns("landcover_Type"), "Select Land Cover Type:", 
                        choices = c("Crops", "Rangeland", "Water", "Trees", "Flooded Vegetation" = "Flooded_vegetation", 
                                    "Built Area" = "Built_Area", "Bare Ground" = "Bare_ground")),    
            actionButton(ns("generate_plot"), "Generate Figure", class = "action_button"),
            br(), br(),
            actionButton(ns("generate_landcover_explorer"), "Show Land Cover Map", class = "action_button")
        ),
      ),
      
      mainPanel(
        # Title and description
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            # p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
            #   "Users can analyze how NDVI fluctuates throughout the year, observing peaks during growing seasons and declines during dry or non-growing periods. This visualization is particularly useful for agricultural monitoring, ecosystem assessments, and climate impact studies."),
            p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
              "To generate the figure, please select a country, month, year and land cover class. The system will process the NDVI values for the selected region and display the corresponding time-series chart for the land cover type.")
        ),
        # Output container (we'll fill this dynamically with either an image or an error message)
        uiOutput(ns("plot_container")),
        br(),
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
              "The visualization shows the distribution of land cover types from a static snapshot."),
        ),
        # Plot street view image (or error message)
        uiOutput(ns("landcover_map_container"))
      )
    )
  )
}

# Server function for Land Cover Dashboard
landCoverServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set directories
    figures_dir <- file.path("www/figures")
    data_dir <- file.path("www/data")
    
    observe({
      req(input$year)
      if (input$year == lubridate::year(Sys.Date())) { # current year
        updateSelectInput(session, "month", choices = month.name[1:lubridate::month(Sys.Date())-1])
      } else {
        updateSelectInput(session, "month", choices = month.name)
      }
    })
    
    # Render a container for the plot or error message
    output$plot_container <- renderUI({
      # Default UI is just an image placeholder
      imageOutput(ns("plot_output"), width = "100%", height = "auto")
    })

    output$landcover_map_container <- renderUI({
      # Placeholder content
      htmlOutput(ns("landcover_map_output"), width = "100%", height = "auto")
    })

    # Observe the Generate Figure button
    observeEvent(input$generate_plot, {

      # Get user inputs
      country_name <- input$country
      end_month <- match(input$month, month.name)
      end_year <- input$year
      resolution <- input$resolution
      landcover_Type <- input$landcover_Type

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
        output$plot_output <- renderImage({
          list(
            src   = figure_path,
            width = "100%",
            alt   = "Land Cover"
          )
        }, deleteFile = FALSE)

      }, error = function(e) {
        
        # If an error occurs (commonly missing data), replace the default UI with a message
        output$plot_container <- renderUI({
          div(
            style = "color: red; margin-top: 20px;",
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
    })

    # Observe the Generate Figure button for the street view plot
    observeEvent(input$generate_landcover_explorer, {

      # Get user inputs
      country_name <- input$country
      map_year <- unlist(strsplit(input$land_cover, ","))[2]
      vector_src <- unlist(strsplit(input$land_cover, ","))[1] # "S2_10m_LULC"

      # Input geojsons stored in country folder. 
      data_path <- paste0(data_dir, "/", "LandUse", "/", 
                                country_name, "/", 
                                vector_src, "_", map_year, "/")

      # Define script and figure paths
      figure_filename <- paste0("figure_LULCmap_", country_name, "_", vector_src, "_", map_year, ".html")
      figure_path <- file.path(figures_dir, figure_filename)

      # Use tryCatch to handle missing data or any errors
      tryCatch({

        # If figure not stored yet, try to generate it
        if (!file.exists(figure_path)) {
          
          # Ensure the figures directory exists
          if (!dir.exists(figures_dir)) {
            dir.create(figures_dir, recursive = TRUE)
          }

          # Create explorer plot
          plot_geojsons_from_a_folder(
            folder_path = data_path,
            save_path = figures_dir,
            filename = figure_filename
          )
        }
        
        # Render the generated (or existing) HTML
        output$landcover_map_output <- renderUI({
          tags$iframe(
            src = paste0("figures/", figure_filename),
            width = "100%",
            height = "500px",
            frameborder = 0
          )
        })

      }, error = function(e) {

        # In case of error (likely missing data files), display a helpful message in the UI
        output$landcover_map_container <- renderUI({
          div(
            style = "color: red;",
            strong("Error: "),
            "An error occurred while generating or reading the land use geojson data. ",
            "This may be due to missing files or incorrect file paths. ",
            "Please verify that the necessary data files exist in '", data_dir, "'.",
            br(), br(),
            paste("Details:", e$message)
          )
        })
        
        # You could also log the error or print it to console
        message("Error generating land use map: ", e$message)
      })
    })
  })
}

