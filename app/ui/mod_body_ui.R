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
                HTML("<span style='text-decoration: underline;'>More information</span><i class='material-icons expand-icon'>expand_more</i>"),
              ),
              tags$strong(HTML("<span>What is NDVI?</span>")),
              p("NDVI (Normalised Difference Vegetation Index) is a satellite-based measure of vegetation greenness. Values range from −1 to 1 — higher values indicate healthy, dense vegetation; lower values indicate stressed vegetation or bare surfaces such as sand, rock, or snow."),
              tags$strong('Which data source should I use?'),
              uiOutput("ndvi_data_source_guidance"),
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
                      title = "NDVI Time Series",
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
                                         uiOutput("ndvi_ts_callout"),
                                         mod_busy_spinner_ui("busy_spinner"),
                                         uiOutput("ndvi_health_summary_card"),
                                         uiOutput("ndvi_annual_summary_card"),
                                         uiOutput("ndvi_ts_plot_container"),
                                         tags$p(
                                           style = "font-size:0.8em; color:#888; margin-top:4px;",
                                           textOutput("ds_label_ndvi_ts", inline = TRUE)
                                         ),
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
                      title = "NDVI Land Cover Explorer",
                      value = "LCexplorerTab",
                      conditionalPanel(condition = "input.tabs == 'NDVIexplorerTab' && input.ndvisubtabs == 'LCexplorerTab'", # Show this figure only when on this tab/subtab combination
                                       div(class="plot-container",
                                           div(class = "ndvi-callout",
                                               p("This chart shows vegetation health for each land cover type throughout the year. Each coloured line represents one type - click the legend to show or hide classes. Use the map on the right to see where each type is located.")),
                                           mod_busy_spinner_ui("busy_spinner"),
                                           uiOutput("lc_plot_container"),
                                           tags$p(
                                             style = "font-size:0.8em; color:#888; margin-top:4px;",
                                             textOutput("ds_label_lc", inline = TRUE)
                                           ))
                      ),
                    ),

                    tabPanel(
                      title = "NDVI Delta Map",
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
                            div(class = "ndvi-callout",
                                p("This map shows how vegetation health has changed compared to the same month in previous years. Green dots = improvement. Red dots = decline. Use it to spot where conditions are changing within the study area."))
                          ),
                          conditionalPanel(
                            condition = "input.ndvi_delta_view == 'annual'",
                            div(class = "ndvi-callout",
                                p("This map compares annual average vegetation health between two selected years. Green = vegetation gaining. Red = vegetation declining. Use it to identify long-term gains and losses across the landscape."))
                          ),
                          mod_busy_spinner_ui("busy_spinner"),
                          uiOutput("dm_plot_container"),
                          tags$p(
                            style = "font-size:0.8em; color:#888; margin-top:4px;",
                            textOutput("ds_label_ndvi_delta", inline = TRUE)
                          )
                        )
                      )
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

                                             mod_busy_spinner_ui("busy_spinner"),
                                             uiOutput("ba_plot_container"),
                                             tags$p(
                                               style = "font-size:0.8em; color:#888; margin-top:4px;",
                                               textOutput("ds_label_ba_seasonal", inline = TRUE)
                                             )
                                           ),

                                           # === DAILY ACTIVITY ===
                                           conditionalPanel(
                                             condition = "input.ba_ts_view == 'daily'",
                                             div(class="infobox",
                                                 p("This chart shows when fires were detected during the fire season, using the exact day each area burned. Peaks indicate days with the most fire activity. Compare years to see whether fire seasons are shifting earlier or later, or becoming more intense.")
                                             ),

                                             mod_busy_spinner_ui("busy_spinner"),
                                             uiOutput("ba_daily_plot_container"),
                                             tags$p(
                                               style = "font-size:0.8em; color:#888; margin-top:4px;",
                                               textOutput("ds_label_ba_daily", inline = TRUE)
                                             )
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

                                             shinyjs::disabled(
                                               downloadButton("download_ba_geojson", "Download Burned Area GeoJSON",
                                                              class = "action_button",
                                                              style = "width:255px; color: white; background-color: #00897B;")
                                             ),
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),

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

                                             uiOutput("frp_year_range_text"),
                                             # Busy Spinner always available for this tab
                                             mod_busy_spinner_ui("busy_spinner"),
                                             leafletOutput("ba_frp_leaflet", height = "450px")
                                           ),

                                           # Data-source label (shared across Monthly + FRP views)
                                           tags$p(
                                             style = "font-size:0.8em; color:#888; margin-top:4px;",
                                             textOutput("ds_label_ba_map", inline = TRUE)
                                           ),

                                       )
                      )
                    )
        )
      )
      ,

      # Scenario Explorer
      tabPanel(
        title = "Scenario Explorer",
        value = "ScenarioExplorerTab",

        tabsetPanel(
          id   = "scenariosubtabs",
          type = "tabs",

          tabPanel(
            title = "Land Cover Productivity",
            value = "ScenarioLandCoverProductivity",
            conditionalPanel(
              condition = "input.tabs == 'ScenarioExplorerTab' && input.scenariosubtabs == 'ScenarioLandCoverProductivity'",
              div(class="plot-container",
                  div(style = paste0(
                    "background:#f1f8e9; border-left:4px solid #558B2F;",
                    "border-radius:4px; padding:12px 16px; margin-bottom:14px;"),
                      p(style = "margin:0 0 6px 0; font-weight:600; font-size:0.93em;",
                        "How to read this chart"),
                      tags$ul(style = "margin:0; padding-left:18px; font-size:0.91em;",
                              tags$li(tags$strong("Left bar chart"), " — which land cover class was most productive this year. Higher bars = healthier vegetation."),
                              tags$li(tags$strong("Right scatter plot"), " — productivity (horizontal) vs year-to-year stability (vertical). Classes in the top-right are both productive AND stable — ideal."),
                              tags$li(tags$strong("Table"), " — exact numbers. Check the Interpretation column for what each class's data means.")
                      ),
                      p(style = "margin:8px 0 0 0; font-size:0.91em;",
                        HTML("⚠️ <strong>Note:</strong> Flooded vegetation's variability is driven by water levels, not vegetation stress — interpret it differently."))
                  ),
                  mod_busy_spinner_ui("busy_spinner"),
                  uiOutput("scenario_productivity_container"),
                  tags$p(
                    style = "font-size:0.8em; color:#888; margin-top:4px;",
                    textOutput("ds_label_productivity", inline = TRUE)
                  )
              )
            )
          ),
          tabPanel(
            title = "Anomaly Resilience",
            value = "ScenarioAnomalyResilience",
            div(class="tab-pane-explain"),
            conditionalPanel(
              condition = "input.tabs == 'ScenarioExplorerTab' && input.scenariosubtabs == 'ScenarioAnomalyResilience'",
              div(class="plot-container",
                  mod_busy_spinner_ui("busy_spinner"),
                  uiOutput("scenario_anomaly_container"),
                  tags$p(
                    style = "font-size:0.8em; color:#888; margin-top:4px;",
                    textOutput("ds_label_anomaly", inline = TRUE)
                  )
              )
            )
          ),
          tabPanel(
            title = "Agricultural Monitoring",
            value = "ScenarioAgriculturalMonitoring",
            conditionalPanel(
              condition = "input.tabs == 'ScenarioExplorerTab' && input.scenariosubtabs == 'ScenarioAgriculturalMonitoring'",
              div(class="plot-container",
                  uiOutput("agri_callout"),
                  mod_busy_spinner_ui("busy_spinner"),
                  uiOutput("scenario_agri_container"),
                  tags$p(
                    style = "font-size:0.8em; color:#888; margin-top:4px;",
                    textOutput("ds_label_agri", inline = TRUE)
                  )
              )
            )
          )
        )
      ),

      # AI Assistant (standalone chat tab; existing tabs unchanged)
      tabPanel(
        title = "AI Assistant",
        value = "AIassistantTab",
        div(
          class = "plot-container",
          tags$style(HTML("
            .ai-chat { min-height: 320px; max-height: 60vh; overflow-y: auto;
                       padding: 10px; border: 1px solid #e0e0e0; border-radius: 6px;
                       background: #fafafa; }
            .ai-row { display: flex; }
            .ai-row.user  { justify-content: flex-end; }
            .ai-row.agent { justify-content: flex-start; }
            .ai-bubble { display: inline-block; padding: 8px 12px; border-radius: 12px;
                         margin: 6px 0; max-width: 80%; font-size: 0.92em;
                         line-height: 1.35; white-space: pre-wrap; word-wrap: break-word; }
            .ai-bubble.user  { background: #1B5E20; color: #ffffff;
                               border-bottom-right-radius: 2px; }
            .ai-bubble.agent { background: #eceff1; color: #1f2d1f;
                               border-bottom-left-radius: 2px; }
            .ai-empty { color: #888; font-style: italic; padding: 16px; }
          ")),
          fluidRow(
            column(
              4,
              selectInput(
                "llm_provider", "AI provider",
                choices = c("Anthropic (Claude)" = "anthropic",
                            "OpenAI (GPT-4o)"     = "openai")
              ),
              conditionalPanel(
                condition = "output.server_key_configured == false",
                wellPanel(
                  passwordInput("user_api_key", "Your API key",
                                placeholder = "sk-... or sk-ant-..."),
                  helpText("Used for this session only. Never stored.")
                )
              ),
              conditionalPanel(
                condition = "output.server_key_configured == true",
                wellPanel(
                  p(tags$span(style = "color:#2E7D32; font-weight:bold;", "● "),
                    "AI Assistant ready")
                )
              ),
              wellPanel(
                h5("Current context"),
                textOutput("context_summary")
              )
            ),
            column(
              8,
              uiOutput("conversation_display"),
              hr(),
              textAreaInput(
                "user_question", NULL,
                placeholder = "Ask about vegetation trends, fire patterns, land cover changes...",
                rows = 3, width = "100%"
              ),
              actionButton("send_question", "Ask", class = "action_button"),
              actionButton("clear_history", "Clear conversation", class = "btn-default btn-sm")
            )
          )
        )
      )
    )
  )
}
