# ui/mod_body_ui.R
source("ui/mod_busy_spinner_ui.R")

mod_body_ui <- function(id) {
  # ns <- NS(id)
  tagList(
    # JS-override for notification position
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50% - 100px);
             max-width: 300px}"))
    ),
    
    # Tabset with panels for separate pasted (nested, currently one for the NDVI Explorer and one for the BA Explorer)
    tabsetPanel(
      id   = "tabs",
      type = "tabs",
      
      # NDVI Explorer
      tabPanel(
        title = "NDVI Explorer",
        value = "NDVIexplorerTab",
        div(class="tab-explain", 
            span("
            NDVI, or Normalised Difference Vegetation Index, is commonly used to track changes in vegetation, particularly in protected areas. 
            It is used to quantify vegetation greenness and is useful in understanding vegetation density and assessing changes in plant health.
            NDVI values range from -1 to 1, with higher values indicating healthier vegetation, and lower values indicating stressed vegetation or barren areas like sand or snow."),
            br(),
            br(),
            span("In this section you will find the NDVI Time Series displaying average NDVI values per month, the NDVI Land Cover Explorer for detailed analysis by land cover class, and the NDVI Delta Map showing geospatial distributions of NDVI values by month.")
        ),
        
        tabsetPanel(id   = "ndvisubtabs",
                    type = "tabs",
                    
                    tabPanel(
                      title = "NDVI Time Series",
                      value = "NDVItsTab",
                      div(class="tab-pane-explain",
                          span("
                           The NDVI Time Series reveals the seasonal dynamics of vegetation health within the selected region over a 12-month period. 
                           It highlights key trends and variations, offering insights into ecological patterns and changes. 
                           Higher NDVI values generally indicate denser, healthier vegetation, while lower values may reflect sparse growth, environmental stress, or land cover changes driven by factors such as drought, deforestation, or agricultural activity."), 
                          br(), br(),
                          span("Use the sidepanel to generate a graph.")
                      ),
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVItsTab'", # Show this figure only when on this tab/subtab combination
                                       div(
                                         class = "plot-container",
                                         style = "margin-left: 10px; margin-right: 10px;",
                                         uiOutput("ndvi_ts_plot_container"),
                                         div(
                                           class = "ndvi-ts-insight-cards",
                                           style = "margin-top: 16px;",
                                           fluidRow(
                                             column(6, uiOutput("wilcoxon_card")),
                                             column(6, uiOutput("smk_card"))
                                           )
                                         )
                                       )),
                      # Busy Spinner always available for this tab
                      mod_busy_spinner_ui("busy_spinner"),
                    ),
                    
                    tabPanel(
                      title = "NDVI Land Cover Explorer",
                      value = "LCexplorerTab",
                      div(class="tab-pane-explain",
                          span("
                               The NDVI Land Cover Explorer offers insights into average NDVI values for specific land cover types within the area of interest. 
                               Users can track NDVI fluctuations throughout the year, observing peaks during growing seasons and declines during dry or dormant periods. 
                               This visualisation is ideal for agricultural monitoring, ecosystem assessments, and climate impact studies."), 
                          br(), br(),
                          span("Use the sidepanel to generate a graph.")
                      ),
                      # Busy Spinner always available for this tab
                      mod_busy_spinner_ui("busy_spinner"),
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'LCexplorerTab'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container", uiOutput("lc_plot_container"))),
                    ),
                    
                    tabPanel(
                      title = "NDVI Delta Map",
                      value = "NDVIdeltaTab",
                      div(class="tab-pane-explain",
                          span("
                          The NDVI Delta Map visualises NDVI values across the selected region, with each pixel representing the value at a specific geographic location. 
                          This allows users to identify spatial patterns, detect anomalies, and compare NDVI values within the Area of Interest (AoI). Users can also calculate Delta NDVI, the difference between current and historical NDVI values for the same month. 
                          The Delta NDVI Heatmap highlights areas where vegetation health has improved or worsened compared to past years."),
                          br(), br(),
                          span("Use the sidepanel to generate a graph.")
                      ),
                      # Busy Spinner always available for this tab
                      mod_busy_spinner_ui("busy_spinner"),
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVIdeltaTab'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container", uiOutput("dm_plot_container"))),
                    ),
        )
      ),
      
      # Burned Area Explorer
      tabPanel(
        title = "Burned Area Explorer",
        value = "BAexplorerTab",
        
        tabsetPanel(id   = "basubtabs",
                    type = "tabs",
                    
                    tabPanel(
                      title = "Burned Area Time Series",
                      value = "BAtimeseries",
                      div(class="tab-pane-explain",
                          span("
                          The Burned Area Time Series reveals the seasonal dynamics of burned areas within the selected region over a 12-month period. 
                          It also shows the burned area up to the selected year, as well as the monthly averages up to that year. 
                          It highlights key trends and variations, offering insights into ecological patterns and changes."),
                          br(), br(),
                          span("Use the sidepanel to generate a graph.")
                      ),
                      # Busy Spinner always available for this tab
                      mod_busy_spinner_ui("busy_spinner"),
                      conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAtimeseries'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container", uiOutput("ba_plot_container"))),
                    ),

                    tabPanel(
                      title = "Burned Area Map Explorer",
                      value = "BAmapexplorer",
                      div(class="tab-pane-explain",
                          span("
                          The Burned Area Map Explorer displays the location and extent of burned areas for a given month and compares them with the same period in the previous year. 
                          This allows users to identify spatial patterns, detect anomalies, and compare burned areas within the Area of Interest (AOI)."),
                          br(), br(),
                          span("Use the sidepanel to generate a graph.")
                      ),
                      # Busy Spinner always available for this tab
                      mod_busy_spinner_ui("busy_spinner"),
                      conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAmapexplorer'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container", br(),
                                           shinyjs::disabled(downloadButton("download_ba_geojson", "Download Burned Area GeoJSON",
                                                                            class = "action_button",
                                                                            style = "width:255px; color: white; background-color: #00897B;")), br(), br(),
                                           uiOutput("ba_map_container")),
                      )
                    )
        )
      )
    )
  )
}
