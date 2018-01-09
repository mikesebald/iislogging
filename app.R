library(shinydashboard)
library(readr)
library(shinyTime)
library(dplyr)
library(ggplot2)

options(shiny.maxRequestSize=30*1024^2)

ui <- dashboardPage(
  # skin = "red",
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
    ),
    fluidRow(
      box(width = 12,
        "Time selection",
        background = "teal",
        timeInput(
          "from",
          "From:",
          value = strptime("00:00:00", "%T")
        ),
        timeInput(
          "to",
          "To:",
          value = strptime("23:59:59", "%T")
        ),
        actionButton(
          "gobutton",
          "Go!"
        )
      )
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Requests", tabName = "requests")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        "dashboard",
        fluidRow(
          valueBoxOutput("start"),
          valueBoxOutput("end"),
          valueBoxOutput("requests")
        ),
        fluidRow(
          valueBoxOutput("min"),
          valueBoxOutput("max"),
          valueBoxOutput("mean")
        ),
        fluidRow(
          plotOutput("timetakenHist")
        ),
        br(),
        fluidRow(
          plotOutput("reqtypePlot")
        ),
        br(),
        fluidRow(
          plotOutput("reqperfPlot")
        )
      # not working with second tabItem. why???  
      # ),
      # tabItem("requests",
      #   fluidRow(
      #     valueBoxOutput("requests2"),
      #     valueBoxOutput("start2"),
      #     valueBoxOutput("start2")
      #   )
      )
    )
  )
)

server <- function(input, output, session) {
  dataInput <- reactive({
    cat("Reading file...")
    
    # if (is.null(input$file))
    #   return(NULL)
    # iis <- read_delim(input$file$datapath, " ",
    iis <- read_delim("../iislogging_data/iis.log", " ",
                      escape_double = FALSE, col_names = FALSE, trim_ws = TRUE,
                      col_types = paste0(rep("c", 15), collapse = ""),
                      na = "-", comment = "#")

    colnames(iis) <- c("date",
                       "time",
                       "sip",
                       "csmethod",
                       "csuristem",
                       "csuriquery",
                       "sport",
                       "csusername",
                       "cip",
                       "csUserAgent",
                       "csReferer",
                       "scstatus",
                       "scsubstatus",
                       "scwin32status",
                       "timetaken")

    iis$time <- as.POSIXct(paste(iis$date, iis$time))

    iis$reqtype <- ""
    iis$reqtype[iis$csmethod == "GET" & !is.na(iis$csuriquery)] <- "SEARCH"
    iis$reqtype[iis$csmethod == "GET" & is.na(iis$csuriquery)] <- "GET"
    iis$reqtype[iis$csmethod == "PUT"] <- "PUT"
    iis$reqtype[iis$csmethod == "POST"] <- "POST"
    iis$reqtype[iis$csmethod == "DELETE"] <- "DELETE"

    colnames(iis)[2] <- "timestamp"
    iis$timetaken <- as.numeric(iis$timetaken)

    cat("done.\n")

    return(iis)
  })

  observeEvent(input$file, {
    dataInput()
  })
  
  datasetUpdate <- eventReactive(input$gobutton, {
    iis <- dataInput()
    if (is.null(iis))
      return(NULL)
    else {
      basedate <- as.POSIXct(as.character(iis[1, "date"]), format = "%Y-%m-%d")
      
      min_ts <- iis[[1, "timestamp"]]
      max_ts <- iis[[nrow(iis), "timestamp"]]

      from = as.POSIXct(paste(basedate, format(input$from, "%T")))
      if (from == basedate)
        from = min_ts
      to = as.POSIXct(paste(basedate, format(input$to, "%T")))
      if (to == basedate)
        to = max_ts

      selectedtimes <- iis[iis$timestamp >= from & iis$timestamp <= to, ]
      print(paste("from:", from, "to:", to))
      
      return(selectedtimes)
    }
  })

  output$start <- renderValueBox({
    x <- datasetUpdate()
    if (is.null(x) || nrow(x) == 0)
      v = 0
    else
      v = as.character(min(x$timestamp), format = "%H:%M:%S")
    
    valueBox(
      value = v,
      subtitle = "Start time",
      color = "yellow",
      icon = icon("log-out", lib = "glyphicon")
    )
  })

  output$end <- renderValueBox({
    x <- datasetUpdate()
    if (is.null(x) || nrow(x) == 0)
      v = 0
    else
      v = as.character(max(x$timestamp), format = "%H:%M:%S")

    valueBox(
      value = v,
      subtitle = "End time",
      color = "yellow",
      icon = icon("log-in", lib = "glyphicon")
    )
  })

  output$min <- renderValueBox({
    x <- datasetUpdate()
    if (is.null(x) || nrow(x) == 0)
      v = 0
    else
      v = as.character(min(x$timetaken))

    valueBox(
      value = v,
      subtitle = "Minimum time",
      color = "green",
      icon = icon("time", lib = "glyphicon")
    )
  })

  output$max <- renderValueBox({
    x <- datasetUpdate()
    if (is.null(x) || nrow(x) == 0)
      v = 0
    else
      v = as.character(max(x$timetaken))

    valueBox(
      value = v,
      subtitle = "Maximum time",
      color = "red",
      icon = icon("time", lib = "glyphicon")
    )
  })

  output$mean <- renderValueBox({
    x <- datasetUpdate()
    if (is.null(x) || nrow(x) == 0)
      v = 0
    else
      v = as.character(round(mean(x$timetaken), 1))

    valueBox(
      value = v,
      subtitle = "Mean time",
      color = "blue",
      icon = icon("time", lib = "glyphicon")
    )
  })

  output$requests <- renderValueBox({
    x <- datasetUpdate()
    if (is.null(x) || nrow(x) == 0)
      v = 0
    else
      v = as.character(nrow(x))

    valueBox(
      value = v,
      subtitle = "Number of requests",
      color = "fuchsia",
      icon = icon("road", lib = "glyphicon")
    )
  })

  output$timetakenHist <- renderPlot({
    x <- datasetUpdate()
    if (!is.null(x)) {
      p <- ggplot(x, aes(x = timetaken, fill = "#75AADB")) +
        ggtitle("Distribution of time-taken") + 
        xlab("time-taken interval [secs]") +
        ylab("Number of requests per interval") +
        geom_histogram()
      p
    }
  })

  output$reqtypePlot <- renderPlot({
    x <- datasetUpdate()
    if (!is.null(x)) {
      df <- as.data.frame(table(x$reqtype))
      colnames(df) <- c("Request type", "Frequency")
      p <- ggplot(df, aes(x = `Request type`, 
                          y = Frequency, 
                          fill = `Request type`)) +
        geom_bar(stat = "identity") +
        ggtitle("Number of requests per type") +
        xlab("Request type") +
        ylab("Number of requests per type")
      p
    }
  })
  
  output$reqperfPlot <- renderPlot({
    x <- datasetUpdate()
    if (!is.null(x)) {
      reqperf <- x %>%
        mutate(window = cut(timestamp, "min")) %>%
        group_by(window) %>%
        summarise(requests = n())

      reqperf$window <- as.POSIXct(reqperf$window)
      
      print(reqperf)
      p <- ggplot(reqperf, aes(x = reqperf$window, y = reqperf$requests)) +
        geom_line() +
        scale_x_datetime() +
        ggtitle("Requests per minute") +
        xlab("Time") + 
        ylab("Number of requests")
      p
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
