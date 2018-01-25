library(lubridate)
library(shinydashboard)
library(dplyr)
library(readr)
library(stringr)
library(dygraphs)
library(xts)
library(ggplot2)

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
      valueBoxOutput("reqend"),
      valueBoxOutput("requests_cdh")
    ),
    fluidRow(
      valueBoxOutput("min_cdh"),
      valueBoxOutput("max_cdh"),
      valueBoxOutput("mean_cdh")
    ),
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
    ),
    br(),
    fluidRow(
      plotOutput("timetakenHist")
    )
  )
)

server <- function(input, output) {
  rv <- reactiveValues()
  
  observeEvent(input$file, {
    if (isolate(input$source == "iis")) {
      cat("Reading IIS file\n")

      columnnames <- "date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) cs(Referer) sc-status sc-substatus sc-win32-status time-taken"
      columnnames <- gsub("[-()]", "", columnnames)
      columnnames <- unlist(strsplit(columnnames, " "))
      n_columns <- length(columnnames)
      
#      f.tmp <- read_delim("../iislogging_data/u_ex180117.log", " ",
      f.tmp <- read_delim(input$file$datapath, " ",
                          escape_double = FALSE, col_names = FALSE, trim_ws = TRUE,
                          col_types = paste0(rep("c", n_columns), collapse = ""),
                          na = "-", comment = "#")
      
      colnames(f.tmp) <- columnnames
      
      iis <- f.tmp
      
      iis$timestamp <- as.POSIXct(paste(iis$date, iis$time), tz = "UTC") %>%
        format(tz = "CET", usetz = TRUE)
      
      iis$reqtype <- ""
      iis$reqtype[iis$csmethod == "GET" & !is.na(iis$csuriquery)] <- "SEARCH"
      iis$reqtype[iis$csmethod == "GET" & is.na(iis$csuriquery)] <- "GET"
      iis$reqtype[iis$csmethod == "HEAD"] <- "HEAD"
      iis$reqtype[iis$csmethod == "PUT"] <- "PUT"
      iis$reqtype[iis$csmethod == "POST"] <- "POST"
      iis$reqtype[iis$csmethod == "DELETE"] <- "DELETE"
      
      iis$timetaken <- as.numeric(iis$timetaken)
      
      iis <- iis[, c("timestamp", "csmethod", "timetaken")]
      
      iis_xts <- xts(iis[, 3], order.by = as.POSIXct(iis$timestamp))
      colnames(iis_xts) <- "iis"
      
      rv$iis <- iis_xts
    } else if (isolate(input$source == "cdh")) {
      cat("Reading CDH file\n")
      
      # f.tmp <- read_lines("../iislogging_data/server-2018-01-24.log")
      f.tmp <- read_lines(input$file$datapath)
      l.tmp <- lapply(f.tmp, function(x) substring(x, 
                                                   c(1, 6, 30, 37, 46), 
                                                   c(4, 28, 35, 45, 150)))
      cdh <- as.tbl(as.data.frame(matrix(unlist(l.tmp), ncol = 5, byrow = TRUE),
                                  stringsAsFactors = FALSE))
      colnames(cdh) <- c("session", "timestamp", "thread", "severity", "message")
      
      cleans <- grepl("No changes detected:|Record saved:", cdh$message)
      cdh <- cdh[cleans, ]
      cdh$csmethod <- "PUT"
      
      cdh$timestamp <- gsub(",", ".", cdh$timestamp)
      cdh$timestamp <- as.POSIXct(strptime(cdh$timestamp,
                                           format = "%Y-%m-%d %H:%M:%OS"), 
                                  tz = "CET") %>%
        format(tz = "CET", usetz = TRUE)
      
      cdh$timetaken <- str_extract(cdh$message, "\\[\\d*ms\\]$") %>%
        gsub(pattern = "[\\[\\]ms]", replacement = "", perl = TRUE) %>%
        as.numeric()
      
      cdh <- cdh[, c("timestamp", "csmethod", "timetaken")]
      
      cdh_xts <- xts(cdh[, 3], order.by = as.POSIXct(cdh$timestamp))
      colnames(cdh_xts) <- "cdh"
      
      rv$cdh <- cdh_xts      
    }
  })
  
  output$dygraph <- renderDygraph({
    if (!is.null(rv$iis) | !is.null(rv$cdh)) {
      if (is.null(rv$iis))
        logdata <- rv$cdh
      else if (is.null(rv$cdh))
        logdata <- rv$iis
      else
        logdata <- cbind(rv$iis, rv$cdh)
    
      dy <- dygraph(data = logdata, main = "Request times taken") %>% 
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE, 
                    highlightSeriesOpts = list(strokeWidth = 3)) %>%
        dyRangeSelector()
      
      return(dy)
    }
  })

  output$reqstart <- renderValueBox({
    req(input$dygraph_date_window[[1]])
    v <- format(ymd_hms(input$dygraph_date_window[[1]], tz = "CET"), 
                format = "%X")

    valueBox(
      value = v,
      subtitle = "Start time",
      color = "yellow",
      icon = icon("log-out", lib = "glyphicon")
    )
  })
  
  output$reqend <- renderValueBox({
    req(input$dygraph_date_window[[2]])
    v <- format(ymd_hms(input$dygraph_date_window[[2]], tz = "CET"), 
                format = "%X")

    valueBox(
      value = v,
      subtitle = "End time",
      color = "yellow",
      icon = icon("log-in", lib = "glyphicon")
    )
  })
  
  output$min_cdh <- renderValueBox({
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)

    df <- as.data.frame(rv$cdh)
    
    if (is.null(df) || nrow(df) == 0)
      v = 0
    else {
      from <- ymd_hms(input$dygraph_date_window[[1]])
      to <- ymd_hms(input$dygraph_date_window[[2]])
      df$timestamp <- as.POSIXct(rownames(df))
      df <- df[(df$timestamp >= from) & (df$timestamp <= to), ]
      v = as.character(min(df$cdh))
    }
    
    valueBox(
      value = v,
      subtitle = "Minimum time",
      color = "green",
      icon = icon("time", lib = "glyphicon")
    )
  })
  
  output$max_cdh <- renderValueBox({
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    df <- as.data.frame(rv$cdh)
    if (is.null(df) || nrow(df) == 0)
      v = 0
    else {
      from <- ymd_hms(input$dygraph_date_window[[1]])
      to <- ymd_hms(input$dygraph_date_window[[2]])
      df$timestamp <- as.POSIXct(rownames(df))
      df <- df[(df$timestamp >= from) & (df$timestamp <= to), ]
      v = as.character(max(df$cdh))
    }
    
    valueBox(
      value = v,
      subtitle = "Maximum time",
      color = "red",
      icon = icon("time", lib = "glyphicon")
    )
  })

  output$mean_cdh <- renderValueBox({
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    df <- as.data.frame(rv$cdh)
    if (is.null(df) || nrow(df) == 0)
      v = 0
    else {
      from <- ymd_hms(input$dygraph_date_window[[1]])
      to <- ymd_hms(input$dygraph_date_window[[2]])
      df$timestamp <- as.POSIXct(rownames(df))
      df <- df[(df$timestamp >= from) & (df$timestamp <= to), ]
      v = as.character(round(mean(df$cdh), 1))
    }
    
    valueBox(
      value = v,
      subtitle = "Mean time",
      color = "blue",
      icon = icon("time", lib = "glyphicon")
    )
  })

  output$requests_cdh <- renderValueBox({
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    df <- as.data.frame(rv$cdh)
    if (is.null(df) || nrow(df) == 0)
      v = 0
    else {
      from <- ymd_hms(input$dygraph_date_window[[1]])
      to <- ymd_hms(input$dygraph_date_window[[2]])
      df$timestamp <- as.POSIXct(rownames(df))
      df <- df[(df$timestamp >= from) & (df$timestamp <= to), ]
      v = as.character(nrow(df))
    }
    
    valueBox(
      value = v,
      subtitle = "Number of requests",
      color = "fuchsia",
      icon = icon("road", lib = "glyphicon")
    )
  })
  
  output$timetakenHist <- renderPlot({
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    df <- as.data.frame(rv$cdh)
    if (nrow(df) > 0) {
      from <- ymd_hms(input$dygraph_date_window[[1]])
      to <- ymd_hms(input$dygraph_date_window[[2]])
      df$timestamp <- as.POSIXct(rownames(df))
      df <- df[(df$timestamp >= from) & (df$timestamp <= to), ]
      p <- ggplot(df, aes(x = cdh, fill = "#75AADB")) +
        ggtitle("Distribution of time-taken") + 
        xlab("time-taken interval [secs]") +
        ylab("Number of requests per interval") +
        geom_histogram()
      p
    }
  })
}

shinyApp(ui = ui, server = server)
