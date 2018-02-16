library(shinydashboard)
library(dplyr)
library(readr)
library(stringr)
library(dygraphs)
library(xts)
library(ggplot2)
library(data.table)

options(shiny.maxRequestSize=100*1024^2)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "CDH Log Analysis"),
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
    cat(paste(Sys.time(), "in observeEvent\n"))
    
    if (isolate(input$source == "iis")) {
      cat("Reading IIS file\n")

      columnnames <- "date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) cs(Referer) sc-status sc-substatus sc-win32-status time-taken"
      columnnames <- gsub("[-()]", "", columnnames)
      columnnames <- unlist(strsplit(columnnames, " "))
      n_columns <- length(columnnames)
      
      f.tmp <- read_delim("../iislogging_data/u_ex180117.log", " ",
      # f.tmp <- read_delim(input$file$datapath, " ",
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
      
      rv$iis_xts <- iis_xts
      rv$iis <- iis
    } else if (isolate(input$source == "cdh")) {
      cat("Reading CDH file\n")
      
      # f.tmp <- read_lines("../iislogging_data/server-2018-02-16.log")
      f.tmp <- read_lines(input$file$datapath)
      
      # subsetting on rows only which have a time-taken timestamp
      cleans <- grepl("No changes detected:|Record saved:", f.tmp)
      clean_rows <- f.tmp[cleans]
      
      # extracting the timestamp and the time-taken 
      timestamp <- str_extract(clean_rows, "\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2},\\d{3}")
      timetaken <- str_extract(clean_rows, "\\[\\d+ms\\]$") %>%
        str_extract("\\d+")
      timetaken[is.na(timetaken)] <- "0"
      
      cdh <- as.data.table(cbind(timestamp, timetaken))
      cdh[, timetaken := as.numeric(timetaken)]
      cdh[, timestamp := gsub(",", ".", cdh[, timestamp])]

      # using lubridate::fast_strptime would be much quicker, but using
      # the lubridate package breaks the build on shinyapps.io
      cdh[, ts := as.POSIXct(strptime(cdh[, timestamp],
                                      format = "%Y-%m-%d %H:%M:%OS",
                                      tz = "CET"))]
      
      cdh_xts <- xts(cdh[, timetaken], order.by = cdh[, ts])
      colnames(cdh_xts) <- "cdh"
      
      rv$cdh_xts <- cdh_xts
      rv$cdh <- cdh
      cat(paste(Sys.time(), "out observeEvent\n"))
    }
  })
  
  output$dygraph <- renderDygraph({
    cat(paste(Sys.time(), "in dygraph\n"))
    if (!is.null(rv$iis_xts) | !is.null(rv$cdh_xts)) {
      if (is.null(rv$iis_xts))
        logdata <- rv$cdh_xts
      else if (is.null(rv$cdh_xts))
        logdata <- rv$iis_xts
      else
        logdata <- cbind(rv$iis_xts, rv$cdh_xts)
      
      dy <- dygraph(data = logdata, main = "Request times taken") %>% 
        dyHighlight(highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE, 
                    highlightSeriesOpts = list(strokeWidth = 3)) %>%
        dyRangeSelector()
      
      cat(paste(Sys.time(), "out dygraph\n"))
      return(dy)
    }
  })

  output$reqstart <- renderValueBox({
    cat(paste(Sys.time(), "in reqstart\n"))
    
    req(input$dygraph_date_window[[1]])
    
    # not using lubridate::ymd_hms here, because using the lubridate package 
    # breaks the build on shinyapps.io
    v <- gsub("T", " ", input$dygraph_date_window[[1]]) %>%
      as.POSIXct(tz = "UTC") %>%
      format(tz = "CET") %>%
      substr(start = 12, stop = 19)

    cat(paste(Sys.time(), "out reqstart\n"))
    valueBox(
      value = v,
      subtitle = "Start time",
      color = "yellow",
      icon = icon("log-out", lib = "glyphicon")
    )
  })
  
  output$reqend <- renderValueBox({
    cat(paste(Sys.time(), "in reqend\n"))
    
    req(input$dygraph_date_window[[2]])
    
    v <- gsub("T", " ", input$dygraph_date_window[[2]]) %>%
      as.POSIXct(tz = "UTC") %>%
      format(tz = "CET") %>%
      substr(start = 12, stop = 19)
    
    cat(paste(Sys.time(), "out reqend\n"))
    valueBox(
      value = v,
      subtitle = "End time",
      color = "yellow",
      icon = icon("log-in", lib = "glyphicon")
    )
  })
  
  output$min_cdh <- renderValueBox({
    cat(paste(Sys.time(), "in min\n"))
    
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)

    dt <- rv$cdh
    if (is.null(dt) || dt[, .N] == 0)
      v = 0
    else {
      from <- gsub("T", " ", input$dygraph_date_window[[1]]) %>%
        as.POSIXct(tz = "UTC")
      attr(from, "tzone") <- "CET"
      to <- gsub("T", " ", input$dygraph_date_window[[2]]) %>%
        as.POSIXct(tz = "UTC")
      attr(to, "tzone") <- "CET"
      
      dt <- dt[(ts >= from) & (ts <= to)]
      v = as.character(min(dt[, timetaken]))
    }
    cat(paste(Sys.time(), "out min\n"))
    
    valueBox(
      value = v,
      subtitle = "Minimum time",
      color = "green",
      icon = icon("time", lib = "glyphicon")
    )
  })
  
  output$max_cdh <- renderValueBox({
    cat(paste(Sys.time(), "in max\n"))

    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    dt <- rv$cdh
    if (is.null(dt) || dt[, .N] == 0)
      v = 0
    else {
      from <- gsub("T", " ", input$dygraph_date_window[[1]]) %>%
        as.POSIXct(tz = "UTC")
      attr(from, "tzone") <- "CET"
      to <- gsub("T", " ", input$dygraph_date_window[[2]]) %>%
        as.POSIXct(tz = "UTC")
      attr(to, "tzone") <- "CET"
      
      dt <- dt[(ts >= from) & (ts <= to), ]
      v = as.character(max(dt[, timetaken]))
    }
    
    cat(paste(Sys.time(), "out max\n"))
    
    valueBox(
      value = v,
      subtitle = "Maximum time",
      color = "red",
      icon = icon("time", lib = "glyphicon")
    )
  })

  output$mean_cdh <- renderValueBox({
    cat(paste(Sys.time(), "in mean\n"))
    
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    dt <- rv$cdh
    if (is.null(dt) || dt[, .N] == 0)
      v = 0
    else {
      from <- gsub("T", " ", input$dygraph_date_window[[1]]) %>%
        as.POSIXct(tz = "UTC")
      attr(from, "tzone") <- "CET"
      to <- gsub("T", " ", input$dygraph_date_window[[2]]) %>%
        as.POSIXct(tz = "UTC")
      attr(to, "tzone") <- "CET"
      
      dt <- dt[(ts >= from) & (ts <= to)]
      v = as.character(round(mean(dt[, timetaken]), 1))
    }

    cat(paste(Sys.time(), "out mean\n"))
    
    valueBox(
      value = v,
      subtitle = "Mean time",
      color = "blue",
      icon = icon("time", lib = "glyphicon")
    )
  })

  output$requests_cdh <- renderValueBox({
    cat(paste(Sys.time(), "in reqs\n"))
    
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    dt <- rv$cdh
    if (is.null(dt) || dt[, .N] == 0)
      v = 0
    else {
      from <- gsub("T", " ", input$dygraph_date_window[[1]]) %>%
        as.POSIXct(tz = "UTC")
      attr(from, "tzone") <- "CET"
      to <- gsub("T", " ", input$dygraph_date_window[[2]]) %>%
        as.POSIXct(tz = "UTC")
      attr(to, "tzone") <- "CET"
      
      dt <- dt[(ts >= from) & (ts <= to)]
      v = as.character(dt[, .N])
    }
    cat(paste(Sys.time(), "out reqs\n"))
    
    valueBox(
      value = v,
      subtitle = "Number of requests",
      color = "fuchsia",
      icon = icon("road", lib = "glyphicon")
    )
  })
  
  output$timetakenHist <- renderPlot({
    cat(paste(Sys.time(), "in hist\n"))
    
    req(input$dygraph_date_window[[1]])
    req(input$dygraph_date_window[[2]])
    req(rv)
    
    dt <- rv$cdh
    if (dt[, .N] > 0) {
      from <- gsub("T", " ", input$dygraph_date_window[[1]]) %>%
        as.POSIXct(tz = "UTC")
      attr(from, "tzone") <- "CET"
      to <- gsub("T", " ", input$dygraph_date_window[[2]]) %>%
        as.POSIXct(tz = "UTC")
      attr(to, "tzone") <- "CET"
      
      dt <- dt[(ts >= from) & (ts <= to)]
      p <- ggplot(dt, aes(x = timetaken, fill = "#75AADB")) +
        ggtitle("Distribution of time-taken") + 
        xlab("time-taken interval [secs]") +
        ylab("Number of requests per interval") +
        geom_histogram(bins = 10)
      cat(paste(Sys.time(), "out hist\n"))
      
      p
    }
  })
}

shinyApp(ui = ui, server = server)
