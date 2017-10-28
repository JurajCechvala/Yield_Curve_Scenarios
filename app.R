# INITIALISATION ----------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(readr)

ReadYieldCurveCsv <- function(file) {

  if (class(file) != "character") {
    stop("File parameter is not a character.")
  } else if (is.null(file) || is.na(file)) {
    stop("File parameter is empty.")
  }

  description <- read_csv2(file = file,
                           skip = 0,
                           n_max = 4)

  data <- read_csv2(file = file,
                    skip = 5,
                    n_max = Inf)

  YC.data <- list()
  YC.data$date <- as.Date(description[[1, 2]], format = "%d.%m.%Y")
  YC.data$type <- as.character(description[[2, 2]])
  YC.data$currency <- as.character(description[[3, 2]])
  YC.data$data <- data

  return(YC.data)
}

# UI - SHINY APP ----------------------------------------------------------------------------------
ui <-
  navbarPage("YC App",
    # panel loads and shows only actual YC data
    tabPanel("YC - actual",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 )
               ),
               mainPanel(
                 tableOutput("yc.data")
               ))),
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

  YC.data <-
    reactive({
      inFile <- input$file1

      if (is.null(inFile))
        data.frame(x = c(0), y = c(0))
      else
        ReadYieldCurveCsv(file = inFile$datapath)
  })

  output$yc.data <- renderTable(YC.data()$data)
}

shinyApp(ui = ui, server = server)

