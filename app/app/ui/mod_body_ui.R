# ui/mod_body_ui.R

mod_body_ui <- function(id) {
  # ns <- NS(id)
  tagList(
    
    # JS-override voor tabbladtitels kleur
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
    
    # Tabset met panels voor de verschillende pagina's
    tabsetPanel(
      id   = "tabs",
      type = "tabs",
      
      # NDVI Time Series Chart
      tabPanel(
        title = i18n$t("labels.NDVItsTab"),
        value = "NDVItsTab",
        uiOutput("ndvi_ts_plot_container"),
        div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
            add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
      ),
      
      # NDVI Land Cover Explorer
      tabPanel(
        title = "NDVI Land Cover Explorer",
        value = "LCexplorerTab",
        div(style = "margin-left: 10px; margin-top: 10px; margin-right: 10px;",
            selectInput("landcover_Type", "Select land cover type",
                        selected = "Crops",
                        choices = c("Crops", "Rangeland", "Water", "Trees", "Flooded Vegetation" = "Flooded_vegetation", 
                                    "Built Area" = "Built_Area", "Bare Ground" = "Bare_ground")), 
            br(),
            uiOutput("lc_plot_container")),
        div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
            add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
      ),
      
      # NDVI Delta Map
      tabPanel(
        title = "NDVI Delta Map",
        value = "NDVIdeltaTab",
        div(style = "margin-left: 10px; margin-top: 10px; margin-right: 10px;",
            uiOutput("dm_plot_container")),
        div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
            add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
      ),
      
      # Burned Area Explorer
      tabPanel(
        title = "Burned Area Explorer",
        value = "BAexplorerTab",
        div(style = "margin-left: 10px; margin-top: 10px; margin-right: 10px;",
            uiOutput("ba_plot_container")),
        div(style = "position: fixed; top: 45%; left: 60%; transform: translate(-50%, -50%);",
            add_busy_spinner(spin = "fading-circle", width = "100px", height = "100px"))
      )
    )
    
  )  
}
