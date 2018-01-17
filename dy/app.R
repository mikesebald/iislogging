library(shinydashboard)
library(readr)
# library(shinyTime)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(stringr)
library(magrittr)
library(htmltools)

options(shiny.maxRequestSize=100*1024^2)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "IIS Log Analysis"),
  dashboardSidebar(
    fluidRow(
      box(width = 12,
          background = "light-blue",
          radioButtons("source", "Log file source:",
                       c("CDH" = "cdh",
                         "IIS" = "iis")
          )
      ),
      box(width = 12,
          background = "light-blue",
          fileInput("file", "Choose file to upload:",
                    accept = c("text/csv",
                               "text/plain",
                               ".log")
          )
      )
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("reqstart"),
      valueBoxOutput("reqend")
    ),
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
      dygraphOutput("dygraph_cdh")
    ),
    br(),
    fluidRow(
      dygraphOutput("dygraph_iis")
    )
  )
)

server <- function(input, output) {
  output$dygraph_cdh <- renderDygraph({
    req(input$file)
    
    if (isolate(input$source == "cdh")) {
      cat("Entering cdh renderDygraph function\n")
      
      f.tmp <- read_lines(input$file$datapath)
      l.tmp <- lapply(f.tmp, function(x) substring(x, 
                                                   c(1, 6, 30, 37, 46), 
                                                   c(4, 28, 35, 45, 150)))
      df <- as.data.frame(matrix(unlist(l.tmp), ncol = 5, byrow = TRUE),
                          stringsAsFactors = FALSE)
      colnames(df) <- c("session", "timestamp", "thread", "severity", "message")
      
      cleans <- grepl("No changes detected:|Record saved:", df$message)
      df <- df[cleans, ]
      
      df$timestamp <- gsub(",", ".", df$timestamp)
      df$timestamp <- as.POSIXct(strptime(df$timestamp,
                                          format = "%Y-%m-%d %H:%M:%OS"))
      
      df$timetaken <- str_extract(df$message, "\\[\\d*ms\\]$") %>%
        gsub(pattern = "[\\[\\]ms]", replacement = "", perl = TRUE) %>%
        as.numeric()
      
      df <- df[, c(2, 4)]
      perfbreaks = "min"
      
      reqperf <- mutate(df, twindow = cut(timestamp, breaks = perfbreaks)) %>%
        group_by(twindow) %>%
        summarise(counter = n())
      
      reqperf_xts <- xts(reqperf[, -1], 
                         order.by = as.POSIXct(reqperf$twindow))
      dy <- dygraph(group = "group", data = reqperf_xts, main = paste0("CDH requests per ", perfbreaks)) %>% 
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE, 
                    highlightSeriesOpts = list(strokeWidth = 3)) %>%
        dyRangeSelector()
      
      return(dy)
    }
  })
  
  output$dygraph_iis <- renderDygraph({
    req(input$file)
    
    if (isolate(input$source == "iis")) {
      cat("Entering iis renderDygraph function\n")
      columnnames <- "date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) cs(Referer) sc-status sc-substatus sc-win32-status time-taken"
      columnnames <- gsub("[-()]", "", columnnames)
      columnnames <- unlist(strsplit(columnnames, " "))
      n_columns <- length(columnnames)
      
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
      
      perfbreaks = "min"
      
      reqperf <- mutate(df, twindow = cut(timestamp, breaks = perfbreaks)) %>%
        group_by(twindow, reqtype) %>%
        summarise(counter = n())
      
      reqperf <- spread(reqperf, reqtype, counter)
      
      reqperf_xts <- xts(reqperf[, -1], 
                         order.by = as.POSIXct(reqperf$twindow))
      dy <- dygraph(group = "group", data = reqperf_xts, main = paste0("IIS requests per ", perfbreaks)) %>% 
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE, 
                    highlightSeriesOpts = list(strokeWidth = 3)) %>%
        dyRangeSelector()
      
      return(dy)
    }
  })
  
  output$reqstart <- renderValueBox({
    v <- strftime(req(input$dygraph_iis_date_window[[1]]), format = "%H:%M:%S", tz = "GMT")
# "2018-01-15T08:00:00.000Z" "2018-01-15T10:21:00.000Z"
    
    valueBox(
      value = v,
      subtitle = "Start time",
      color = "yellow",
      icon = icon("log-out", lib = "glyphicon")
    )
  })
  
  output$reqend <- renderValueBox({
    v <- strftime(req(input$dygraph_iis_date_window[[2]]), "%Y-%m-%d %H:%M:%S")
    
    valueBox(
      value = v,
      subtitle = "End time",
      color = "yellow",
      icon = icon("log-in", lib = "glyphicon")
    )
  })
}

shinyApp(ui = ui, server = server)
