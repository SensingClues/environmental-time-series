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

# UI function for NDVI Heatmap Dashboard
ndviHeatmapUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    sidebarLayout(
      sidebarPanel(
        # Title and description
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            h2(class = "text-3xl font-bold text-white-800 mb-4", "Time Series Map: NDVI"),
            p(class = "text-lg text-white-700 leading-relaxed text-justify mb-2", 
              "Visualize the Normalized Difference Vegetation Index (NDVI) values over time, and create a delta output. Select the year and month for which you want to analyze the 12 months preceding your selection.",
              a(class = "text-blue-500 hover:underline", "Read more", href = "http://sensingclues.org/environmental-time-series-about")),
        ),
        # Controls for user input
        div(class = "controls max-w-4xl mx-auto px-6 py-4",
            selectInput(ns("country"), "Select Project Area:", selected = "Zambia",
                        choices = c("Mponda, Zambia" = "Zambia", "Ancares Courel, Spain" = "Spain", 
                                    "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya")),
            selectInput(ns("year"), "Select Year:", selected = lubridate::year(Sys.Date()), choices = seq(2018, lubridate::year(Sys.Date()), 1)),
            selectInput(ns("month"), "Select Month:", selected="January" , choices = month.name[1:lubridate::month(Sys.Date())-1]),
            selectInput(ns("resolution"), "Select spatial resolution (m):", 
                        selected = "100 (ESA Sentinel-2)", 
                        choices = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                    "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100")),
            actionButton(ns("generate_static_plot"), "Generate Map", class = "action_button"),
            br(),
            div(class = "controls-delta max-w-4xl mx-auto px-6 py-4",
                actionButton(ns("generate_streetview_plot"), "Generate Delta", class = "action_button")
            ),
        ),
      ),
      
      mainPanel(
        # Title and description
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            # p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
            #   "The 2D heatmap provides a spatial representation of NDVI values across the selected region, with each pixel displaying the NDVI value at a specific geographic coordinate. This visualization allows users to identify spatial patterns in vegetation health, detect localized anomalies, and compare NDVI values across different areas within the AOI."),
            # p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
            #   "In addition to absolute NDVI values, users can also compute the Delta NDVI, which represents the difference between the NDVI of the current month and the NDVI of the same month in previous years. The Delta NDVI heatmap highlights areas where vegetation health has improved or deteriorated compared to historical averages."),
            p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
              "To generate the heatmap, select a country, month, and year. The system will process the NDVI values and, if requested, compute the Delta NDVI to show how vegetation has changed relative to historical data.")
        ),
        # Output container
        uiOutput(ns("map_output_container")),
        br(),
        uiOutput(ns("streetmap_output_container"))
      )
    )
  )
}

# Server function for NDVI Heatmap Dashboard
ndviHeatmapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
     # Set directories
    figures_dir <- file.path("www/figures")
    data_dir <- file.path("/home/timeseries")
    
    observe({
      req(input$year)
      if (input$year == lubridate::year(Sys.Date())) { # current year
        updateSelectInput(session, "month", choices = month.name[1:lubridate::month(Sys.Date())-1])
      } else {
        updateSelectInput(session, "month", choices = month.name)
      }
    })

    output$map_output_container <- renderUI({
      imageOutput(ns("map_output"), width = "100%", height = "auto")
    })
    
    output$streetmap_output_container <- renderUI({
      htmlOutput(ns("streetmap_output"), width = "100%", height = "auto")
    })

    observeEvent(input$generate_static_plot, {
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
        output$map_output <- renderImage({
          list(src = figure_path, width = "100%", alt = "NDVI 2D map")
        }, deleteFile = FALSE)
      }, error = function(e) {
        output$map_output_container <- renderUI({
          div(style = "color: red;", strong("Error: "), "An error occurred while generating the NDVI data. Check that the necessary data files exist.", br(), br(), paste("Details:", e$message))
        })
        message("Error generating static NDVI map: ", e$message)
      })
    })

    observeEvent(input$generate_streetview_plot, {
      country_name <- input$country
      map_month <- match(input$month, month.name)
      map_year <- input$year
      resolution <- input$resolution
      figure_filename <- paste0("figure_deltaNDVImaps_", country_name, "_", map_month, "_", map_year, "_", resolution, "m.html")
      figure_path <- file.path(figures_dir, figure_filename)

      tryCatch({
        if (!file.exists(figure_path)) {
          if (!dir.exists(figures_dir)) {
            dir.create(figures_dir, recursive = TRUE)
          }
          generate_2Dmap(country_name, resolution, map_year, map_month, figures_dir, data_dir, TRUE, FALSE, figure_filename)
        }
        output$streetmap_output <- renderUI({
          tags$iframe(src = paste0("figures/", figure_filename), width = "100%", height = "500px", frameborder = 0)
        })
      }, error = function(e) {
        output$streetmap_output_container <- renderUI({
          div(style = "color: red;", strong("Error: "), "An error occurred while generating the delta NDVI data. Check that the necessary data files exist.", br(), br(), paste("Details:", e$message))
        })
        message("Error generating delta NDVI map: ", e$message)
      })
    })
  })
}

