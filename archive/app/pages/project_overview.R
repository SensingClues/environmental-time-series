library(shiny)

# UI function for Project Overview
projectOverviewUI <- function(id) {
  ns <- NS(id)  # Create a namespace for modularization
  tagList(
    # Page Header
    div(class = "text-center py-6",
        h1(class = "text-4xl font-bold text-gray-800 mb-6", "Project Overview")
    ),
    
    # Project Details Section
    div(class = "project-section max-w-4xl mx-auto px-6 py-4",
        h2(class = "text-3xl font-bold text-gray-800 mb-4", "Overview of the Project"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
          "Partner organization: ", 
          a(class = "text-blue-500 hover:underline", "https://sensingclues.org/", href = "https://sensingclues.org/")),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
          "Team: 3 Data Enthusiasts, 1 Project Manager, 1 Project Trainee"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
          "Topic: Tracking Landscape Vitality using Geo-Spatial Data through Sensor Deployment"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", 
          "Skills: R, statistical modeling, visualization, GitHub. Familiarity with Geographic Information Systems (GIS) and Shiny app development is considered a plus"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", "Project start: ~ 04-11-2024"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", "Project end: ~ 31-01-2025"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", "Place: Remote"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-2", "Application deadline: 18-10-2024")
    ),
    
    # About Section
    div(class = "project-section max-w-4xl mx-auto px-6 py-4",
        h2(class = "text-3xl font-bold text-gray-800 mb-4", "About Sensing Clues"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify", 
          "Sensing Clues is a non-profit organization dedicated to global nature conservation through the development of a sophisticated suite of data-driven tools. By integrating real-time data collection, monitoring, and predictive analytics, Sensing Clues supports wildlife rangers, conservationists, and communities in their mission to protect biodiversity and ecosystems. The organization offers hands-on support to its users, making advanced technology accessible to field operators around the world, from Africa to Europe.")
    ),
    
    # Vision Section
    div(class = "project-section max-w-4xl mx-auto px-6 py-4",
        h2(class = "text-3xl font-bold text-gray-800 mb-4", "Vision"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify", 
          "Sensing Clues envisions a future where the power of data transforms conservation efforts, by enabling more proactive protection of wildlife and natural habitats globally.")
    ),
    
    # Mission Section
    div(class = "project-section max-w-4xl mx-auto px-6 py-4",
        h2(class = "text-3xl font-bold text-gray-800 mb-4", "Mission"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify", 
          "Sensing Clues’ mission is to provide nature conservation organizations with tools and interdisciplinary workspaces to turn data into actionable information. Our goal is to help professionals make timely, informed decisions that prevent threats to biodiversity and promote the sustainable coexistence of humans and wildlife.")
    ),
    
    # Project Goals Section
    div(class = "project-section max-w-4xl mx-auto px-6 py-4",
        h2(class = "text-3xl font-bold text-gray-800 mb-4", "Project Goals"),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-4", 
          "The main objective of this project is to implement a system for tracking vegetation conditions in protected areas using satellite imagery from publicly available sources, provided by the Sensing Clues team. In particular, the system will monitor the vegetation index over time, ensuring that it remains within its healthy range. This system will be integrated into Sensing Clues’ existing suite of tools, enabling local management and nature conservation organizations to utilize the information effectively and take timely actions to address any potential environmental concerns."),
        p(class = "text-lg text-gray-700 leading-relaxed text-justify mb-4", "The project tasks can be broken down into the following parts:"),
        tags$ul(class = "list-disc list-outside text-lg text-gray-700 leading-relaxed space-y-2 pl-8 text-justify",
          tags$li("Analysis of the currently available historical geospatial data"),
          tags$li("Using statistical approaches to determine the ‘normal’ bandwidth of the vegetation index"),
          tags$li("Implementing time series visualization maps for the levels of vegetation vigor"),
          tags$li("(Optional) Embedding of the implemented algorithms into a Shiny application")
        )
    )
  )
}

# Server function for Project Overview (no plot rendering needed)
projectOverviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No specific server logic required
  })
}

# Shiny App
shinyApp(
  ui = fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css")
    ),
    projectOverviewUI("projectOverview")  # Calling the project overview UI
  ),
  server = function(input, output, session) {
    projectOverviewServer("projectOverview")  # Calling the project overview server logic
  }
)

