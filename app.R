library(shinydashboard)
library(readr)
library(hms)

options(shiny.maxRequestSize=30*1024^2)

ui <- dashboardPage(
  # skin = "red",
  dashboardHeader(title = "IIS Log Analysis"),
  dashboardSidebar(
    fluidRow(
      box(width = 12,
        "File upload",
        background = "green",
        fileInput("filename", "Choose file to upload:",
                  accept = c("text/csv",
                             "text/plain",
                             "text/comma-separated-values",
                             ".csv")
        )
      )        
    ),
    fluidRow(
      box(width = 12,
        "Time selection",
        background = "teal",
        textInput(
          "from",
          "From",
          value = "00:00:00"
        ),
        textInput(
          "to",
          "To",
          value = "23:59:59"
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
    # if (is.null(input$filename))
    #   return(NULL)
    
#     iis <- read_delim(input$filename$datapath, " ", 
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
    
    basedate <- as.POSIXct(as.character(iis[1, "date"]), format = "%Y-%m-%d")
    iis$time <- as.POSIXct(paste(iis$date, iis$time))
    iis <- iis[, -1]

    colnames(iis)[1] <- "timestamp" 
    min_ts <- iis[[1, "timestamp"]]
    max_ts <- iis[[nrow(iis), "timestamp"]]
    
    iis$timetaken <- as.numeric(iis$timetaken)
    
    # Updating input fields not working correctly
    from = as.POSIXct(paste(basedate, input$from))
    if (from == basedate) {
      from = min_ts
    #  updateTextInput(session, "from", value = paste(from))
    }
    to = as.POSIXct(paste(basedate, input$to))
    if (to == basedate) {
      to = max_ts
    #  updateTextInput(session, "to", value = paste(to))
    }

    selectedtimes <- iis[iis$timestamp >= from & iis$timestamp <= to, ]
    return(selectedtimes)
  })

  output$start <- renderValueBox({
    x <- dataInput()
    if (nrow(x) == 0)
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
    x <- dataInput()
    if (nrow(x) == 0)
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
    x <- dataInput()
    if (nrow(x) == 0)
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
    x <- dataInput()
    if (nrow(x) == 0)
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
    x <- dataInput()
    if (nrow(x) == 0)
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
    x <- dataInput()
    if (nrow(x) == 0)
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
    x <- dataInput()
    hist(
      as.numeric(x$timetaken),
      main = "Distribution of time-taken",
      xlab = "time-taken interval [secs]",
      ylab = "Number of requests per interval",
      col = "#75AADB", 
      border = "white"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
