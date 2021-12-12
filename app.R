# app file
library("shiny")

# app_server.R and app_ui.R
source("app_server.R")
source("app_ui.R")

# shinyApp
shinyApp(ui = ui, server = server)