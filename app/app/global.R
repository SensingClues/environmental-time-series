
library(dplyr)
library(future)
library(ggplot2)
library(ipc)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(promises)
library(raster)
library(shiny)
library(shinybusy)
library(shiny.i18n) # for multilanguage
library(shinyjs)
library(shinyTree)
library(shinyWidgets)
library(sf)
library(terra)
library(tidyr)
library(plotly)
library(htmlwidgets)

# load functions specific for this app
source("functions.R")

# as part of future package we need to define where the future is executed,
# multisession means we are launching background R processes on the same machine
# other options are multicore (not on Windows) and multiprocess
plan(multisession)

# load the sensincluesr package with a specific version tag
library(devtools)
devtools::install_github("sensingclues/sensingcluesr@v1.0.3", upgrade = "never")

# --- Global variables ---------------------------------------------------------

# load mapbox credentials
mapbox <- tryCatch(jsonlite::read_json("/home/mapbox_sensingclues.json"), error = function(e) NULL)

languages <- c("Dutch" = "nl", 
               "English"= "en", 
               "French" = "fr")

language_table <- data.frame(lang_short = languages,
                             lang_long = names(languages),
                             row.names = NULL)

# --- Multilingual setup -------------------------------------------------------
i18n <- Translator$new(translation_json_path = "translations.json")

# Set initial language to English
i18n$set_translation_language("en")

js_lang <- "var language =  window.navigator.userLanguage || window.navigator.language;
              Shiny.onInputChange('browser_language', language);
              console.log(language);"
