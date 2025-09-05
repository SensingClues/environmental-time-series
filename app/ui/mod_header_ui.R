# ui/mod_header_ui.R

mod_header_ui <- function(id) {
  # ns <- NS(id)
  tagList(
    div(
      class = "header",
      
      # Logo  title
      tags$a(
        href   = "https://sensingclues.org/portal",
        target = "_blank",
        div(class = "logo", img(src = "logo_white.png"))
      ),
      div(
        class = "title",
        i18n$t("labels.headerTitle"), # in capitals
      ),
      tags$a(
        href   = "https://correlaid.nl/",
        target = "_blank",
        div(class = "logo-ca", img(src = "logo.svg"))
      )
    )
  )
}
