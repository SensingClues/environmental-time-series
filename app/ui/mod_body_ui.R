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
        # ndviInfoSection (collapsible)
        div(class="tab-explain",
            tags$details(
              id    = "ndviInfoSection",
              class = "collapsible-section",
              tags$summary(
                class = "collapsible-header",
                HTML("<span style='bold;'>More information</span><i class='material-icons expand-icon'>expand_more</i>"),
              ),
              tags$strong(HTML("<span>What is NDVI?</span>")),
              p("NDVI (Normalised Difference Vegetation Index) is a satellite-based measure of vegetation greenness. Values range from −1 to 1 — higher values indicate healthy, dense vegetation; lower values indicate stressed vegetation or bare surfaces such as sand, rock, or snow."),
              tags$strong('Which data source should I use?'),
              uiOutput("ndvi_data_source_guidance"),
              br(),
              tags$strong('How to use this section'),
              tags$ol(
                tags$li(
                  tags$strong(
                    tags$a(href = "#", onclick = '$(\"#ndvisubtabs a[data-value=\'NDVItsTab\']\").tab(\'show\'); return false;',
                           "NDVI Time Series")
                  ),
                  " — Start here to see the overall vegetation trend for your area. Check the Long-Term Trend card. If using Sentinel-2, consider switching to MODIS for a longer baseline."
                ),
                tags$li(
                  tags$strong(
                    tags$a(href = "#", onclick = '$(\"#ndvisubtabs a[data-value=\'LCexplorerTab\']\").tab(\'show\'); return false;',
                           "NDVI Land Cover Explorer")
                  ),
                  " — Drill down by land cover class to identify which class (Trees, Crops, Rangeland etc.) is driving any trend you observed."
                ),
                tags$li(
                  tags$strong(
                    tags$a(href = "#", onclick = '$(\"#ndvisubtabs a[data-value=\'NDVIdeltaTab\']\").tab(\'show\'); return false;',
                           "NDVI Delta Map")
                  ),
                  " — Use the Annual Change view to see spatially where gains and losses are occurring within the project area."
                )
              )
            ),
            tags$script(HTML(
              "document.addEventListener('DOMContentLoaded', function() {
                var el = document.getElementById('ndviInfoSection');
                if (el) {
                  var summary = el.querySelector('summary');
                  summary.addEventListener('click', function() {
                    setTimeout(function() {
                      var icon = summary.querySelector('.expand-icon');
                      icon.style.transform = el.hasAttribute('open') ? 'rotate(180deg)' : 'rotate(0deg)';
                    }, 100);
                  });
                }
              });"
            ))),

        tabsetPanel(id   = "ndvisubtabs",
                    type = "tabs",
                    
                    tabPanel(
                      title = tags$span(title = "Displays NDVI time series for the selected area. Switch between monthly and annual views to explore vegetation dynamics over time.", "NDVI Time Series"),
                      value = "NDVItsTab",
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVItsTab'",
                                       div(
                                         class = "plot-container",
                                         style = "margin-left: 10px; margin-right: 10px;",
                                         shinyWidgets::radioGroupButtons(
                                           inputId  = "ndvi_ts_view",
                                           label    = NULL,
                                           choices  = c("Monthly view" = "monthly", "Annual view" = "annual"),
                                           selected = "monthly",
                                           size     = "sm",
                                           status   = "default"
                                         ),
                                         mod_busy_spinner_ui("busy_spinner"),
                                         uiOutput("ndvi_health_summary_card"),
                                         uiOutput("ndvi_annual_summary_card"),
                                         uiOutput("ndvi_ts_plot_container"),
                                         div(
                                           class = "ndvi-ts-insight-cards",
                                           style = "margin-top: 16px;",
                                           fluidRow(
                                             column(12, uiOutput("wilcoxon_card")),
                                             column(12, uiOutput("smk_card"))
                                           )
                                         )
                                       )),
                    ),

                    tabPanel(
                      title = tags$span(title = "This chart shows vegetation health for each land cover type throughout the year. Each coloured line represents one type - click the legend to show or hide classes. Use the map on the right to see where each type is located.", "NDVI Land Cover Explorer"),
                      value = "LCexplorerTab",
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'LCexplorerTab'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container",
                                           mod_busy_spinner_ui("busy_spinner"),
                                           uiOutput("lc_plot_container"))
                      ),
                    ),

                    tabPanel(
                      title = tags$span(title = "This map shows how vegetation greenness has changed compared to the same month in previous years (Monthly view) or between two selected years (Annual change view). Green = improvement. Red = decline. Use it to identify gains and losses over time across the selected area.", "NDVI Delta Map"),
                      value = "NDVIdeltaTab",
                      conditionalPanel(
                        condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'NDVIdeltaTab'",
                        div(
                          class = "plot-container",
                          shinyWidgets::radioGroupButtons(
                            inputId  = "ndvi_delta_view",
                            label    = NULL,
                            choices  = c("Monthly view" = "monthly", "Annual change view" = "annual"),
                            selected = "monthly",
                            size     = "sm",
                            status   = "default"
                          ),
                          conditionalPanel(
                            condition = "input.ndvi_delta_view == 'monthly'",
                          ),
                          conditionalPanel(
                            condition = "input.ndvi_delta_view == 'annual'",
                          ),
                          mod_busy_spinner_ui("busy_spinner"),
                          uiOutput("dm_plot_container")
                        )
                      )
                    ),
        )
      ),

      # Burned Area Explorer
      tabPanel(
        title = "Burned Area Explorer",
        value = "BAexplorerTab",
        
        div(class="tab-explain",
            tags$details(
              id    = "baInfoSection",
              class = "collapsible-section",
              tags$summary(
                class = "collapsible-header",
                HTML("<span style='bold;'>More information</span><i class='material-icons expand-icon'>expand_more</i>"),
              ),
              tags$strong(HTML("<span>What is the Burned Area Explorer?</span>")),
              p("The Burned Area Explorer uses aggregate satellige image information to identify the so-called burn scar (blackened soil after a fire) and its starting date."),
            ),
            tags$script(HTML(
              "document.addEventListener('DOMContentLoaded', function() {
                var el = document.getElementById('baInfoSection');
                if (el) {
                  var summary = el.querySelector('summary');
                  summary.addEventListener('click', function() {
                    setTimeout(function() {
                      var icon = summary.querySelector('.expand-icon');
                      icon.style.transform = el.hasAttribute('open') ? 'rotate(180deg)' : 'rotate(0deg)';
                    }, 100);
                  });
                }
              });"
            ))),
        
        tabsetPanel(id   = "basubtabs",
                    type = "tabs",

                    tabPanel(
                      title = tags$span(title = "This tab is used to compare current land burned to historic values (Seasonal Overview) or to evaluate how the timing of the fire season compares to that of previous years (Daily Activity).", "Burned Area Time Series"),
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
                                             mod_busy_spinner_ui("busy_spinner"),
                                             uiOutput("ba_plot_container")
                                           ),

                                           # === DAILY ACTIVITY ===
                                           conditionalPanel(
                                             condition = "input.ba_ts_view == 'daily'",
                                             mod_busy_spinner_ui("busy_spinner"),
                                             uiOutput("ba_daily_plot_container")
                                           ),
                                       )
                      )
                    ),

                    tabPanel(
                      title = tags$span(title = "This tab is used to evaluate where fires occured and when they started in the selected month (Monthly View) or to analyze the burn frequency (lower frequency = more regular fires) in different parts of the selected area (Fire Return Period).", "Burned Area Map Explorer"),
                      value = "BAmapexplorer",
                      conditionalPanel(condition = "input.tabs == 'BAexplorerTab' && input.basubtabs == 'BAmapexplorer'",
                                       div(class = "plot-container",

                                           # --- View toggle ---
                                           shinyWidgets::radioGroupButtons(
                                             inputId  = "ba_map_view",
                                             label    = NULL,
                                             choices  = c("Monthly View" = "monthly", "Interactive Map" = "interactive", "Fire Return Period" = "frp"),
                                             selected = "monthly",
                                             size     = "sm",
                                             status   = "default"
                                           ),

                                           # === MONTHLY VIEW ===
                                           conditionalPanel(
                                             condition = "input.ba_map_view == 'monthly'",
                                             shinyjs::disabled(
                                               downloadButton("download_ba_geojson", "Download Burned Area GeoJSON",
                                                              class = "action_button",
                                                              style = "width:255px; color: white; background-color: #00897B;")
                                             ),
                                             
                                             tags$div(
                                               style="height:20px;"
                                             ),
                                             
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),
                                             
                                             # Historical comparison static map (rendered by server on button click)
                                             uiOutput("ba_map_container")
                                           ),
                                           
                                           conditionalPanel(
                                             condition = "input.ba_map_view == 'interactive'",
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),
                                             
                                             # Interactive monthly leaflet (hidden until generated)
                                             div(id = "monthly_leaflet_wrap", style = "display:none;",
                                                 p(style = "font-weight:600; color:#444; margin-bottom:10px;",
                                                   "Interactive Burned Area Map"),
                                                 leafletOutput("ba_monthly_leaflet", height = "425px"))
                                            ),
                                             
                                           # === FIRE RETURN PERIOD VIEW ===
                                           conditionalPanel(
                                             condition = "input.ba_map_view == 'frp'",
                                             uiOutput("frp_year_range_text"),
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),
                                             leafletOutput("ba_frp_leaflet", height = "450px")
                                           ),

                                       )
                      )
                    )
        )
      )
      ,

      # Scenario Explorer
      # tabPanel(
      #   title = "Scenario Explorer",
      #   value = "ScenarioExplorerTab",
      # 
      #   tabsetPanel(
      #     id   = "scenariosubtabs",
      #     type = "tabs",
      # 
      #     tabPanel(
      #       title = "Land Cover Productivity",
      #       value = "ScenarioLandCoverProductivity",
      #       conditionalPanel(
      #         condition = "input.tabs == 'ScenarioExplorerTab' && input.scenariosubtabs == 'ScenarioLandCoverProductivity'",
      #         div(class="plot-container",
      #             div(style = paste0(
      #               "background:#f1f8e9; border-left:4px solid #558B2F;",
      #               "border-radius:4px; padding:12px 16px; margin-bottom:14px;"),
      #                 p(style = "margin:0 0 6px 0; font-weight:600; font-size:0.93em;",
      #                   "How to read this chart"),
      #                 tags$ul(style = "margin:0; padding-left:18px; font-size:0.91em;",
      #                         tags$li(tags$strong("Left bar chart"), " — which land cover class was most productive this year. Higher bars = healthier vegetation."),
      #                         tags$li(tags$strong("Right scatter plot"), " — productivity (horizontal) vs year-to-year stability (vertical). Classes in the top-right are both productive AND stable — ideal."),
      #                         tags$li(tags$strong("Table"), " — exact numbers. Check the Interpretation column for what each class's data means.")
      #                 ),
      #                 p(style = "margin:8px 0 0 0; font-size:0.91em;",
      #                   HTML("⚠️ <strong>Note:</strong> Flooded vegetation's variability is driven by water levels, not vegetation stress — interpret it differently."))
      #             ),
      #             mod_busy_spinner_ui("busy_spinner"),
      #             uiOutput("scenario_productivity_container")
      #         )
      #       )
      #     ),
      #     tabPanel(
      #       title = "Anomaly Resilience",
      #       value = "ScenarioAnomalyResilience",
      #       div(class="tab-pane-explain"),
      #       conditionalPanel(
      #         condition = "input.tabs == 'ScenarioExplorerTab' && input.scenariosubtabs == 'ScenarioAnomalyResilience'",
      #         div(class="plot-container",
      #             mod_busy_spinner_ui("busy_spinner"),
      #             uiOutput("scenario_anomaly_container")
      #         )
      #       )
      #     ),
      #     tabPanel(
      #       title = "Agricultural Monitoring",
      #       value = "ScenarioAgriculturalMonitoring",
      #       conditionalPanel(
      #         condition = "input.tabs == 'ScenarioExplorerTab' && input.scenariosubtabs == 'ScenarioAgriculturalMonitoring'",
      #         div(class="plot-container",
      #             uiOutput("agri_callout"),
      #             mod_busy_spinner_ui("busy_spinner"),
      #             uiOutput("scenario_agri_container")
      #         )
      #       )
      #     )
      #   )
      # )
    )
  )
}
