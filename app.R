# app.R
library(shiny)

# Source UI and server
source("ui.R")
source("server.R")

shinyApp(ui, server)
