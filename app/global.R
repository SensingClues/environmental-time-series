library(dplyr)
library(future)
library(ggplot2)
library(htmltools)
library(ipc)
library(jsonlite)
library(leaflet)
library(leaflet.extras2)
library(lubridate)
library(promises)
library(raster)
library(RColorBrewer)
library(shiny)
library(shinybusy)
library(shiny.i18n) # for multilanguage
library(shinyjs)
library(shinyTree)
library(shinyWidgets)
library(sf)
library(terra)
library(tidyr)
library(trend)
library(plotly)
library(htmlwidgets)
library(arrow)
library(httr)
library(commonmark)

# load functions specific for this app
source("src/utilities.R")
source("src/visualization.R")
source("src/generate_plots.R")
source("src/agent_renderers.R")

# as part of future package we need to define where the future is executed,
# multisession means we are launching background R processes on the same machine
# other options are multicore (not on Windows) and multiprocess
plan(multisession)

# load the sensincluesr package with a specific version tag
library(devtools)
devtools::install_github("sensingclues/sensingcluesr@v1.0.3", upgrade = "never")

# --- Global variables ---------------------------------------------------------

languages <- c("Dutch" = "nl", 
               "English"= "en", 
               "French" = "fr")

language_table <- data.frame(lang_short = languages,
                             lang_long = names(languages),
                             row.names = NULL)

# Set input/output directories
figures_dir <- file.path("www/figures")
data_dir <- file.path("/home/timeseries")
cache_dir <- file.path("www/.cache")

# Set test folder structure (uncomment when working locally with a different folder structure)
#test_dir <- file.path("www/data")
#data_dir <- test_dir

# --- API (hot path) configuration --------------------------------------------
# Set NDVI_API_URL to point at the FastAPI server. Defaults to localhost so the
# hot path is attempted by default; on timeout the app falls back to Parquet.
API_BASE_URL     <- Sys.getenv("NDVI_API_URL", unset = "http://localhost:8000")
API_TIMEOUT_SECS <- 2   # hard requirement — do not increase

# --- Multilingual setup -------------------------------------------------------
i18n <- Translator$new(translation_json_path = "translations.json")

# Set initial language to English
i18n$set_translation_language("en")

js_lang <- "var language =  window.navigator.userLanguage || window.navigator.language;
              Shiny.onInputChange('browser_language', language);
              console.log(language);"
