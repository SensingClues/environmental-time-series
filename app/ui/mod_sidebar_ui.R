# ui/mod_sidebar_ui.R

mod_sidebar_ui <- function(id) {
  # ns <- NS(id)
  tagList(
    
    # Material Icons
    tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
              ),
    
    # COLLAPSIBLE ABOUT SECTION
    tags$details(id = "aboutCollapse",
                 class = "collapsible-section",
                 tags$summary(class = "collapsible-header",
                              HTML(sprintf('<span>%s</span><i class="material-icons expand-icon">expand_more</i>',
                                           i18n$t("labels.aboutTitle")))),
                 p(i18n$t("labels.aboutETSAText")), 
                 tags$a(i18n$t("labels.aboutReadmore"),
                        href = "https://www.sensingclues.org/environmental-time-series-analysis",
                        class = "readmore",
                        target = "_blank")
                 ),
    
    # JS: collapse icon for the About section
    tags$script(HTML(sprintf(
      "document.addEventListener('DOMContentLoaded', function() {
        var el = document.getElementById('%s');
        if (el) {
          var summary = el.querySelector('summary');
          summary.addEventListener('click', function(e) {
            setTimeout(function() {
              var icon = summary.querySelector('.expand-icon');
              if (el.hasAttribute('open')) {
                icon.style.transform = 'rotate(180deg)';
              } else {
                icon.style.transform = 'rotate(0deg)';
              }
            }, 100);
          });
        }
      });",
      "aboutCollapse"
    ))),
    
    h3(i18n$t("labels.sidebarTitle")),
    
    # Generic input selectors 
    div(selectInput("country", "Select project area", 
                    selected = "Zambia",
                    choices = c("Mponda, Zambia" = "Zambia", "Ancares Courel, Spain" = "Spain", 
                                "Stara Planina, Bulgaria" = "Bulgaria", "Kasigau, Kenya" = "Kenya")), # Add more countries as needed
        leafletOutput("map", height = "165px"), # Adds leaflet map for the AoI
        br(),
        selectInput("year", "Select year", 
                    selected = lubridate::year(Sys.Date()), 
                    choices = seq(2018, lubridate::year(Sys.Date()), 1)),
        shinyjs::disabled(selectInput("month", "Select month", 
                                      selected="January", 
                                      choices = month.name[1:lubridate::month(Sys.Date())-1])),
        selectInput("resolution", "Select spatial resolution (m)", 
                    selected = "1000 (ESA Sentinel-2)", 
                    choices = c("1000 (ESA Sentinel-2)" = "Sentinel_1000", "1000 (Terra MODIS)" = "MODIS_1000",
                                "500 (Terra MODIS)" = "500", "250 (Terra MODIS)" = "250", "100 (ESA Sentinel-2)" = "100"))
        ),
    
    # NDVI Time Series page-specific button (specific to be able to link output generation to button press)
    conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVItsTab'",
                     div(actionButton("generate_ndvi_ts_figures", "Generate Figure", class = "action_button"))
                     ),
    
    # NDVI Land Cover Explorer page-specific selector and button (specific to be able to link output generation to button press)
    conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'LCexplorerTab'",
                     div(actionButton("generate_lc_figures", "Generate Figures", class = "action_button"))
                     ),
    
    # NDVI Delta Map page-specific button (specific to be able to link output generation to button press)
    conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVIdeltaTab'",
                     div(actionButton("generate_ndvi_delta_plot", "Generate Figures", class = "action_button"))
                     ),
    
    # Burned Area Time Series — Seasonal Overview
    conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAtimeseries' && input.ba_ts_view == 'seasonal'",
                     div(actionButton("generate_ba_ts_figures", "Generate Figure", class = "action_button"))
    ),

    # Burned Area Time Series — Daily Activity
    conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAtimeseries' && input.ba_ts_view == 'daily'",
                     div(
                       uiOutput("ba_daily_year_selector"),
                       sliderInput("ba_season_months", "Fire season (months)",
                                   min = 1, max = 12, value = c(6, 11), step = 1),
                       actionButton("generate_ba_daily_figures", "Generate Figure", class = "action_button")
                     )
    ),
    
    # Burned Area Explorer page-specific button (specific to be able to link output generation to button press)
    conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAmapexplorer'",
                     div(actionButton("generate_ba_map_figures", "Generate Figure", class = "action_button"))
                     )
  )
}
