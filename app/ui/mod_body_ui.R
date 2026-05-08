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

        tabsetPanel(id   = "ndvisubtabs",
                    type = "tabs",

                    tabPanel(
                      title = "Info",
                      value = "NDVIInfoTab",
                      div(class="tab-explain",
                          span("
            NDVI, or Normalised Difference Vegetation Index, is commonly used to track changes in vegetation, particularly in protected areas.
            It is used to quantify vegetation greenness and is useful in understanding vegetation density and assessing changes in plant health.
            NDVI values range from -1 to 1, with higher values indicating healthier vegetation, and lower values indicating stressed vegetation or barren areas like sand or snow."),
                          br(),
                          br(),
                          span("In this section you will find the NDVI Time Series displaying average NDVI values per month, the NDVI Land Cover Explorer for detailed analysis by land cover class, and the NDVI Delta Map showing geospatial distributions of NDVI values by month.")
                      ),
                    ),
                    tabPanel(
                      title = "NDVI Time Series",
                      value = "NDVItsTab",
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVItsTab'", # Show this figure only when on this tab/subtab combination
                                       div(
                                         class = "plot-container",
                                         div(class="infobox",
                                             p("
                           The NDVI Time Series reveals the seasonal dynamics of vegetation health within the selected region over a 12-month period.
                           It highlights key trends and variations, offering insights into ecological patterns and changes.
                           Higher NDVI values generally indicate denser, healthier vegetation, while lower values may reflect sparse growth, environmental stress, or land cover changes driven by factors such as drought, deforestation, or agricultural activity."),
                                         ),
                                         # Busy Spinner always available for this tab
                                         mod_busy_spinner_ui("busy_spinner"),
                                         span("Use the sidepanel to generate a graph."),
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
                    ),

                    tabPanel(
                      title = "NDVI Land Cover Explorer",
                      value = "LCexplorerTab",

                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'LCexplorerTab'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container",
                                           div(class="infobox",
                                               p("
                               The NDVI Land Cover Explorer offers insights into average NDVI values for specific land cover types within the area of interest.
                               Users can track NDVI fluctuations throughout the year, observing peaks during growing seasons and declines during dry or dormant periods.
                               This visualisation is ideal for agricultural monitoring, ecosystem assessments, and climate impact studies."),
                                           ),
                                           # Busy Spinner always available for this tab
                                           mod_busy_spinner_ui("busy_spinner"),
                                           span("Use the sidepanel to generate a graph."),
                                           uiOutput("lc_plot_container"))
                      ),
                    ),

                    tabPanel(
                      title = "NDVI Delta Map",
                      value = "NDVIdeltaTab",

                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVIdeltaTab'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container",
                                           div(class="infobox",
                                               p("
                          The NDVI Delta Map visualises NDVI values across the selected region, with each pixel representing the value at a specific geographic location.
                          This allows users to identify spatial patterns, detect anomalies, and compare NDVI values within the Area of Interest (AoI). Users can also calculate Delta NDVI, the difference between current and historical NDVI values for the same month.
                          The Delta NDVI Heatmap highlights areas where vegetation health has improved or worsened compared to past years."),
                                           ),
                                           # Busy Spinner always available for this tab
                                           mod_busy_spinner_ui("busy_spinner"),
                                           span("Use the sidepanel to generate a graph."),
                                           uiOutput("dm_plot_container"))
                      ),
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
                      conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAtimeseries'",
                                       div(class = "plot-container",

                                           # --- View toggle ---
                                           shinyWidgets::radioGroupButtons(
                                             inputId  = "ba_ts_view",
                                             label    = NULL,
                                             choices  = c("Seasonal Overview" = "seasonal", "Daily Activity" = "daily"),
                                             selected = "seasonal",
                                             size     = "sm",
                                             status   = "default"
                                           ),

                                           # === SEASONAL OVERVIEW ===
                                           conditionalPanel(
                                             condition = "input.ba_ts_view == 'seasonal'",
                                             div(class="infobox",
                                                 p("This chart shows how much land burned each month over the year. The shaded band shows the typical range based on historical data. Use it to see whether this year's fire activity is higher or lower than usual.")
                                             ),
                                             span("Use the sidepanel to generate a graph."),
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),
                                             uiOutput("ba_plot_container")
                                           ),

                                           # === DAILY ACTIVITY ===
                                           conditionalPanel(
                                             condition = "input.ba_ts_view == 'daily'",
                                             div(class="infobox",
                                                 p("This chart shows when fires were detected during the fire season, using the exact day each area burned. Peaks indicate days with the most fire activity. Compare years to see whether fire seasons are shifting earlier or later, or becoming more intense.")
                                             ),
                                             span("Use the sidepanel to generate a graph."),
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),
                                             uiOutput("ba_daily_plot_container")
                                           ),
                                       )
                      )
                    ),

                    tabPanel(
                      title = "Burned Area Map Explorer",
                      value = "BAmapexplorer",
                      conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAmapexplorer'",
                                       div(class = "plot-container",

                                           # --- View toggle ---
                                           shinyWidgets::radioGroupButtons(
                                             inputId  = "ba_map_view",
                                             label    = NULL,
                                             choices  = c("Monthly View" = "monthly", "Fire Return Period" = "frp"),
                                             selected = "monthly",
                                             size     = "sm",
                                             status   = "default"
                                           ),

                                           # === MONTHLY VIEW ===
                                           conditionalPanel(
                                             condition = "input.ba_map_view == 'monthly'",
                                             div(class="infobox",
                                                 p("This map shows where fires occurred in the selected month. Each red area is a patch of land where burning was detected. In the Interactive Burned Area Map, hover over an area to see the exact date it burned. Use the Download button to save the data for use in other tools.")
                                             ),
                                             span("Use the sidepanel to generate a graph."),
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),

                                             shinyjs::disabled(
                                               downloadButton("download_ba_geojson", "Download Burned Area GeoJSON",
                                                              class = "action_button",
                                                              style = "width:255px; color: white; background-color: #00897B;")
                                             ),
                                             # Historical comparison static map (rendered by server on button click)
                                             div(style = "width:100%; overflow-x:auto;",
                                                 uiOutput("ba_map_container")),
                                             # Interactive monthly leaflet (hidden until generated)
                                             div(id = "monthly_leaflet_wrap", style = "display:none;",
                                                 hr(style = "margin: 24px 0 16px 0; border-color: #ddd;"),
                                                 p(style = "font-weight:600; color:#444; margin-bottom:10px;",
                                                   "Interactive Burned Area Map"),
                                                 leafletOutput("ba_monthly_leaflet", height = "450px"))
                                           ),

                                           # === FIRE RETURN PERIOD VIEW ===
                                           conditionalPanel(
                                             condition = "input.ba_map_view == 'frp'",
                                             div(class="infobox",
                                                 p("The fire return period shows how often each area tends to burn. A short return period (e.g. 1–2 years) means the area burns almost every year. A longer return period (e.g. 8–10 years) means fires are rare. Areas that burn frequently may indicate fire-prone vegetation or land management practices.")
                                             ),
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),

                                             uiOutput("frp_year_range_text"),
                                             leafletOutput("ba_frp_leaflet", height = "450px")
                                           ),

                                       )
                      )
                    )
        )
      )
    )
  )
}
