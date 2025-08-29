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

# Source helper scripts
source("utils.R")
source("visualize.R")
source("generate_plots.R")

# UI function for NDVI Timeseries Dashboard
ndviTimeseriesUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    sidebarLayout(
      sidebarPanel(
        # Title and description
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            h2(class = "text-3xl font-bold text-white-800 mb-4", "Time Series Chart: NDVI"),
            p(class = "text-lg text-white-700 leading-relaxed text-justify mb-2", 
              "Generate and explore the Normalized Difference Vegetation Index (NDVI) values, averaged over the area of interest. Select the year and month for which you want to analyze the 12 months preceding your selection.",
              a(class = "text-blue-500 hover:underline", "Read more", href = "http://sensingclues.org/environmental-time-series-about"))
        ),
        # Controls for user input
        div(class = "controls max-w-4xl mx-auto px-6 py-4",
            selectInput(ns("country"), "Select Project Area:", selected = "Zambia",
                        choices = c("Mponda, Zambia" = "Zambia", "Ancares Courel, Spain" = "Spain", 
                                    "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya")), # Add more countries as needed
            selectInput(ns("year"), "Select Year:", selected = lubridate::year(Sys.Date()), choices = seq(2018, lubridate::year(Sys.Date()), 1)),
            selectInput(ns("month"), "Select Month:", selected="January" , choices = month.name[1:lubridate::month(Sys.Date())-1]),
            selectInput(ns("resolution"), "Select spatial resolution (m):", 
                        selected = "100 (ESA Sentinel-2)", 
                        choices = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                    "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100")),

            br(),
            actionButton(ns("generate_plot"), "Generate Figure", class = "action_button"),
            br(),
            br(),
            # Adds leaflet map for the AoI
            p(class = "text-lg text-white-700 leading-relaxed text-justify mb-2",
              "Selected project area:"),
            leafletOutput(ns("map"), height = "250px")
        )
      ),
      
      mainPanel(
        # Title and description
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            # p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2",
            #   "The timeseries visualization shows the temporal dynamics of vegetation for the selected region over a 12-month period, highlighting trends and seasonal variations in vegetation health. Higher NDVI values typically indicate denser and healthier vegetation, whereas lower values may signal sparse vegetation, stress, or land cover changes due to environmental factors such as drought, deforestation, or agricultural activity."),
            p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
              "To generate the figure, please select a country, month, and year. The system will process the NDVI values for the selected region and display the corresponding time-series chart, allowing for easy interpretation and comparison across different time periods.")
        ),
        # Output container (we'll fill this dynamically with either an image or an error message)
        uiOutput(ns("plot_container"))
      )
    )
  )
}

# Server function for NDVI Timeseries Dashboard
ndviTimeseriesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set directories
    figures_dir <- file.path("www/figures")
    data_dir <- file.path("home/timeseries")
    
    # Set Reactive values
    aoi_shape_rv <- reactiveVal(NULL)
    error_message_rv <- reactiveVal(NULL)
    
    observe({
      req(input$year)
      if (input$year == lubridate::year(Sys.Date())) { # current year
        updateSelectInput(session, "month", choices = month.name[1:lubridate::month(Sys.Date())-1])
      } else {
        updateSelectInput(session, "month", choices = month.name)
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
        return(leaflet() %>% addTiles())
        } 
      
      bounds <- sf::st_bbox(shape)
      
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = shape, color = "green", opacity = 0.5, fillOpacity = 0.2, weight = 2) %>%
        fitBounds(bounds[[1]], bounds[[2]], bounds[[3]], bounds[[4]])
    })
    
    # Render a container for the plot or error message
    output$plot_container <- renderUI({
      # Default UI is just an image placeholder
      imageOutput(ns("plot_output"), width = "100%", height = "auto")
    })

    # Observe the Generate Figure button
    observeEvent(input$generate_plot, {

      # Get user inputs
      country_name <- input$country
      end_month <- match(input$month, month.name)
      end_year <- input$year
      resolution <- input$resolution

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
        output$plot_output <- renderImage({
          list(
            src   = figure_path,
            width = "100%",
            alt   = "NDVI timeseries"
          )
        }, deleteFile = FALSE)

      }, error = function(e) {
        
        # If an error occurs (commonly missing data), replace the default UI with a message
        output$plot_container <- renderUI({
          div(
            style = "color: red; margin-top: 20px;",
            strong("Error: "),
            "An error occurred while generating or reading the NDVI timeseries data. ",
            "This may be due to missing files or incorrect file paths. ",
            "Please verify that the necessary data files exist in '", data_dir, "'.",
            br(), br(),
            paste("Details:", e$message),
            br(),
            paste("Country Name:", country_name),
            br(),
            paste("Resolution:", resolution),
            br(),
            paste("End Year:", end_year),
            br(),
            paste("End Month:", end_month),
            br(),
            paste("Figures Directory:", figures_dir),
            br(),
            paste("Data Directory:", data_dir)
          )
        })
        
        # Optionally, also log the error to the console for debugging
        message("Error generating NDVI timeseries: ", e$message)
      })
    })
  })
}

