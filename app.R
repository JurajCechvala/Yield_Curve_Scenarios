# INITIALISATION ----------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(readr)

# function for reading CSV files containing YC data, returns YC object
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

# stress scenarios as a list of functions to be superimposed on real YC
# stress.fun <- list()
# stress.fun$no.stress  <- function(x) {rep(   0, length(x))}
# stress.fun$p200bps    <- function(x) {rep( 200, length(x))}
# stress.fun$m200bps    <- function(x) {rep(-200, length(x))}
# stress.fun$asc200bps  <- approxfun(x = c(0.5, 50),
#                                    y = c(0, 200),
#                                    rule = 2)
# stress.fun$desc200bps <- approxfun(x = c(0.5, 50),
#                                    y = c(200, 0),
#                                    rule = 2)
# names(stress.fun) <- c("No stress", "+200 bps uniform shift", "-200 bps uniform shift",
#                        "linear increase to +200 bps", "linear decrease from +200 bps")

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
                 tabsetPanel(
                   tabPanel("Table",
                            tableOutput("yc.table")),
                   tabPanel("Graph",
                            plotOutput("yc.plot")))

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

  YC.curve <-
    reactive({
      approxfun(x = YC.data()$data[[1]],
                y = YC.data()$data[[2]],
                rule = 2)
    })

  output$yc.table  <-
    renderTable(
      YC.data()$data)

  output$yc.plot <-
    renderPlot(
      # draw the YC function using ggplot
      ggplot(data.frame(x = c(0, max(YC.data()$data[[1]]))), aes(x)) +
        stat_function(fun = YC.curve()) +
        labs(x = colnames(YC.data()$data)[1], y = colnames(YC.data()$data)[2])
    )
}

shinyApp(ui = ui, server = server)

