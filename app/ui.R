source("ui/mod_header_ui.R")
source("ui/mod_sidebar_ui.R")
source("ui/mod_body_ui.R")

ui <- fluidPage(
  useShinyjs(),
  shiny.i18n::usei18n(i18n),
  extendShinyjs(text=js_lang, functions=c()),
  includeCSS("www/style.css"),

  mod_header_ui("header"),
  
  div(class = "mainContainer",
      div(class = "sidebar", mod_sidebar_ui("sidebar")),
      div(class = "content", mod_body_ui("body"))
  )
)
