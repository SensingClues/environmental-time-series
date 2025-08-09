library(shiny)

# Define UI for the application
shinyUI(fluidPage(
  
  includeCSS("www/style.css"),
  
  # Include Tailwind CDN
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"),
    tags$style(HTML("
      /* Remove default Shiny CSS for container-fluid */
      .container-fluid {
        padding: 0 !important;
        margin: 0 auto !important;
      }

      .black-logo {
        filter: brightness(0) saturate(100%);
      }

      .mt-extra {
        margin-top: 8rem; /* Extra space below navbar */
      }
    "))
  ),
  
  # Top navigation bar
  div(class = "navbar bg-gray-100 fixed top-0 w-full flex items-center justify-between px-6 py-4 shadow z-10",
    div(class = "collab-logo flex items-center space-x-4",
        a(href = "https://sensingclues.org", target = "_blank",
          img(src = "sensing-clues-logo.png", alt = "Sensing Clues Logo", class = "collab-image w-20 h-auto black-logo")
        ),
        span(class = "collab-separator text-gray-700 font-bold text-lg", "×"),
        a(href = "https://correlaid.nl", target = "_blank",
          img(src = "logo.svg", alt = "Correlaid Logo", class = "collab-image w-20 h-auto")
        )
    ),
    div(class = "flex space-x-6 relative",
      div(class = "nav-item group cursor-pointer font-bold text-gray-700 hover:text-blue-500 relative", "Home"),
      div(class = "nav-item group cursor-pointer font-bold text-gray-700 hover:text-blue-500 relative", "About",
        div(class = "absolute left-0 mt-2 bg-gray-100 rounded-md shadow-lg opacity-0 group-hover:opacity-100 transition-opacity duration-300 w-48",
           div(class = "py-2 px-4 hover:bg-gray-200 hover:text-blue-500", 
                actionLink("project_overview", "Project Overview")),
           div(class = "py-2 px-4 hover:bg-gray-200 hover:text-blue-500", 
               actionLink("data_collection", "Data Collection"))
        )
      ),
      div(class = "nav-item group cursor-pointer font-bold text-gray-700 hover:text-blue-500 relative", "Tools",
        div(class = "absolute left-0 mt-2 bg-gray-100 rounded-md shadow-lg opacity-0 group-hover:opacity-100 transition-opacity duration-300 w-56",
            div(class = "py-2 px-4 hover:bg-gray-200 hover:text-blue-500", 
                actionLink("NDVI_timeseries", "Time Series Chart")),
            div(class = "py-2 px-4 hover:bg-gray-200 hover:text-blue-500", 
                actionLink("NDVI_heatmap", "Time Series Map")),
            div(class = "py-2 px-4 hover:bg-gray-200 hover:text-blue-500",
                actionLink("landCover_explorer", "Land Cover Explorer")),
            div(class = "py-2 px-4 hover:bg-gray-200 hover:text-blue-500",
                actionLink("BurnedArea_explorer", "Burned Area Explorer"))
        )
      )
    )
  ),
  
  # Main content area
  div(class = "content mt-extra px-8 py-6 text-center",
      div(class = "prose max-w-screen-lg mx-auto",
          uiOutput("pageContent")
      )
  ),
  
  # Footer
  div(class = "footer bg-gradient-to-r from-blue-700 to-red-500 text-white text-center py-8 mt-12",
      p("CorrelAid × Sensing Clues"),
      p("Built with ❤️ by volunteers.")
  )
))

