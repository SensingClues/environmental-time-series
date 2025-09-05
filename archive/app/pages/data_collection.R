
library(shiny)

# UI function for Data Collection and Preprocessing Overview

dataOverviewUI <- function(id) {
    ns <- NS(id)
    tagList(
        # Page Header 
        div(class = "text-center py-6", 
            h1(class = "text-4xl font-bold text-gray-800 mb-6", "Data Collection and Preprocessing") 
        ),

        # Data Source Section
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            h2(class = "text-3xl font-bold text-gray-800 mb-4", "Data Source"),
            p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
            "The NDVI (Normalized Difference Vegetation Index) data used in this project was obtained from the Sentinel-2 satellite imagery, specifically the \"COPERNICUS/S2_SR_HARMONIZED\" dataset, through Google Earth Engine (GEE). This dataset provides atmospherically corrected surface reflectance imagery, making it suitable for vegetation analysis and monitoring.")
        ),

        # AOI and Time Period Section
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            h2(class = "text-3xl font-bold text-gray-800 mb-4", "Area of Interest (AOI) and Time Period"),
            p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
            "The AOI is defined based on asset files available within GEE. The data collection is conducted over a user-specified time period, with the minimum granularity set at a single month. This allows for consistent temporal analysis and trend identification.")
        ),

        # Data Preprocessing Section
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            h2(class = "text-3xl font-bold text-gray-800 mb-4", "Data Preprocessing Steps"),
            tags$ul(class = "list-disc list-outside text-lg text-gray-700 leading-relaxed space-y-2 pl-8 text-justify",
            tags$li("Sentinel-2 surface reflectance images were filtered based on the AOI and specified date range."),
            tags$li("Cloud and other unwanted pixels were masked using the Scene Classification Layer (SCL) band."),
            tags$li("NDVI was computed for each image using the formula: NDVI = (B8 - B4) / (B8 + B4); where B8 corresponds to the near-infrared (NIR) band, which captures vegetation reflectance, and B4 corresponds to the red band, which captures vegetation absorption."),
            tags$li("A mosaic operation was performed to create a composite NDVI image for each month. This composite represents the best available NDVI values within the given month, reducing the impact of missing or low-quality data.")
            )
        ),

        # Data Export Section
        div(class = "project-section max-w-4xl mx-auto px-6 py-4",
            h2(class = "text-3xl font-bold text-gray-800 mb-4", "Data Export"),
            p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
            "The processed NDVI composite images were exported to Google Drive, clipped to the AOI, and stored using the defined spatial resolution and coordinate reference system (CRS). The exported filenames include the processing month and country name for easy identification.")
        )
    )
}

# Server function for Data Overview (no plot rendering needed)

dataOverviewServer <- function(id) {
    moduleServer(id, function(input, output, session) { 
    # No specific server logic required 
    }) 
}

# Shiny App

shinyApp( 
    ui = fluidPage(
        tags$head(
            tags$link(rel = "stylesheet", href = "[https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css](https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css)") 
        ), 
        dataOverviewUI("dataOverview")  # Calling the data overview UI 
        ), 
        server = function(input, output, session) { 
            dataOverviewServer("dataOverview")  # Calling the data overview server logic 
        } 
)

