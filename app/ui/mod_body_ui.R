# ui/mod_body_ui.R

mod_body_ui <- function(id) {
  # ns <- NS(id)
  tagList(
    
    # JS-override for tab colours
    tags$script(HTML(sprintf(
      "
      $(function() {
        var $tabs = $('#%s');
        // Niet-actieve tabs helder grijs
        $tabs.find('.nav-tabs > li > a, .nav-pills > li > a')
             .css('color', '#aaa');
        // Actieve tab donkerder grijs
        $tabs.find('.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover,\n                    .nav-pills > li.active > a, .nav-pills > li.active > a:hover')
             .css('color', '#666');
      });
      ", "tabs"))
    ),
    
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
        
        tabsetPanel(id   = "ndvisubtabs",
                    type = "tabs",
                    
                    tabPanel(
                      title = "NDVI Time Series",
                      value = "NDVItsTab",
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVItsTab'", # Show this figure only when on this tab/subtab combination
                                       div(style = "margin-left: 10px; margin-top: 10px; margin-right: 10px;",
                                           uiOutput("ndvi_ts_plot_container"))),
                      # Busy Spinner always available for this tab
                      div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);", 
                          add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
                    ),

                    tabPanel(
                      title = "NDVI Land Cover Explorer",
                      value = "LCexplorerTab",
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'LCexplorerTab'", # Show this figure only when on this tab/subtab combination
                                       div(style = "margin-left: 10px; margin-top: 10px; margin-right: 10px;",
                                           uiOutput("lc_plot_container"))),
                      # Busy Spinner always available for this tab
                      div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
                          add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
                    ),
                    
                    tabPanel(
                      title = "NDVI Delta Map",
                      value = "NDVIdeltaTab",
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVIdeltaTab'", # Show this figure only when on this tab/subtab combination
                                       div(style = "margin-left: 10px; margin-top: 10px; margin-right: 10px;",
                                           uiOutput("dm_plot_container"))),
                      # Busy Spinner always available for this tab
                      div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
                          add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
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
                      conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAtimeseries'", # Show this figure only when on this tab/subtab combination
                                       div(style = "margin-left: 10px; margin-top: 10px; margin-right: 10px;",
                                           uiOutput("ba_plot_container"))),
                      # Busy Spinner always available for this tab
                      div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
                          add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
                    ),
                    
                    tabPanel(
                      title = "Burned Area Map Explorer",
                      value = "BAmapexplorer",
                      conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAmapexplorer'", # Show this figure only when on this tab/subtab combination
                                       div(style = "margin-left: 10px; margin-top: 14px; margin-right: 10px;", br(),
                                           shinyjs::disabled(downloadButton("download_ba_geojson", "Download Burned Area GeoJSON", 
                                                                            class = "action_button",
                                                                            style = "width:255px; color: white; background-color: #00897B;")), br(), br(),
                                           uiOutput("ba_map_container")),
                                       # Busy Spinner always available for this tab
                                       div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
                                           add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
                                       )
                            )
                    )
        )
      )
    )  
}
