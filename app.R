# INITIALISATION ----------------------------------------------------------------------------------
library(shiny)
library(ggplot2)

# UI - SHINY APP ----------------------------------------------------------------------------------
ui <-
  navbarPage("YC App",
    # panel loads and shows only actual YC data
    tabPanel("YC - actual",
             sidebarLayout(
               sidebarPanel(),
               mainPanel())),
    # panel displays, loads and creates modifications to YC
    tabPanel("YC - modification",
             sidebarLayout(
               sidebarPanel(),
               mainPanel())),
    # panel shows and saves final YC - combination of actual YC and a modification (selectable)
    tabPanel("YC - scenario",
             sidebarLayout(
               sidebarPanel(),
               mainPanel())))

# SERVER - SHINY APP ------------------------------------------------------------------------------
server <- function(input, output) {

}

shinyApp(ui = ui, server = server)

