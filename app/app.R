# load the ui and server and call them to start the Shiny application

source("ui.R")
source("server.R")

shinyApp(ui, server)
