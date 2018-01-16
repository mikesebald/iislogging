library(shinydashboard)
library(readr)
library(shinyTime)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)

options(shiny.maxRequestSize=100*1024^2)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "IIS Log Analysis"),
  dashboardSidebar(
    fluidRow(
      box(width = 12,
          "File upload",
          background = "green",
          fileInput("file", "Choose file to upload:",
                    accept = c("text/csv",
                               "text/plain",
                               "text/comma-separated-values",
                               ".log")
          )
      )        
    )
  ),
  dashboardBody(
    # fluidRow(
    #   valueBoxOutput("start"),
    #   valueBoxOutput("end"),
    #   valueBoxOutput("requests")
    # ),
    # fluidRow(
    #   valueBoxOutput("min"),
    #   valueBoxOutput("max"),
    #   valueBoxOutput("mean")
    # ),
    # fluidRow(
    #   plotOutput("timetakenHist")
    # ),
    # br(),
    # fluidRow(
    #   plotOutput("reqtypePlot")
    # ),
    # br(),
    # fluidRow(
    #   plotOutput("reqperfPlot")
    # ),
    # br(),
    fluidRow(
      dygraphOutput("dygraph")
    )
  )
)

server <- function(input, output) {
  output$dygraph <- renderDygraph({
    req(input$file)
    
    cat("Entering renderDygraph function")
    columnnames <- "date time s-sitename s-computername s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs-version cs(User-Agent) cs(Cookie) cs(Referer) cs-host sc-status sc-substatus sc-win32-status sc-bytes cs-bytes time-taken"
    columnnames <- gsub("[-()]", "", columnnames)
    columnnames <- unlist(strsplit(columnnames, " "))
    n_columns <- length(columnnames)
    
    # iis <- read_delim("../iislogging_data/iis.log", " ",
    iis <- read_delim(input$file$datapath, " ",
                      escape_double = FALSE, col_names = FALSE, trim_ws = TRUE,
                      col_types = paste0(rep("c", n_columns), collapse = ""),
                      na = "-", comment = "#")
    
    colnames(iis) <- columnnames
    
    iis$time <- as.POSIXct(paste(iis$date, iis$time))
    
    iis$reqtype <- ""
    iis$reqtype[iis$csmethod == "GET" & !is.na(iis$csuriquery)] <- "SEARCH"
    iis$reqtype[iis$csmethod == "GET" & is.na(iis$csuriquery)] <- "GET"
    iis$reqtype[iis$csmethod == "HEAD"] <- "HEAD"
    iis$reqtype[iis$csmethod == "PUT"] <- "PUT"
    iis$reqtype[iis$csmethod == "POST"] <- "POST"
    iis$reqtype[iis$csmethod == "DELETE"] <- "DELETE"
    
    colnames(iis)[2] <- "timestamp"
    iis$timetaken <- as.numeric(iis$timetaken)
    
    df <- iis[, c("timestamp", "reqtype")]
    
    perfbreaks = "1 min"
    
    reqperf <- mutate(df, twindow = cut(timestamp, breaks = perfbreaks)) %>%
      group_by(twindow, reqtype) %>%
      summarise(counter = n())
    
    reqperf <- spread(reqperf, reqtype, counter)
    
    reqperf_xts <- xts(reqperf[, -1], 
                       order.by = as.POSIXct(reqperf$twindow))
    dy <- dygraph(data = reqperf_xts, main = paste0("Requests per ", perfbreaks)) %>% 
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE, 
                  highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyRangeSelector()
    
    return(dy)
  })
}

shinyApp(ui = ui, server = server)
