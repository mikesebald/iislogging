library(shinydashboard)
library(readr)
library(hms)

options(shiny.maxRequestSize=30*1024^2)

ui <- dashboardPage(
  skin = "red",
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
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  
  dashboardBody(tabItems(
    tabItem(
      "dashboard",
      fluidRow(
        valueBoxOutput("start"),
        valueBoxOutput("end")
      ),
      fluidRow(
        valueBoxOutput("min"),
        valueBoxOutput("max")
      ),
      fluidRow(
        valueBoxOutput("mean"),
        valueBoxOutput("median")
      )
    )
  ))
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
                       "s-ip",
                       "cs-method",
                       "cs-uri-stem",
                       "cs-uri-query",
                       "s-port",
                       "cs-username",
                       "c-ip",
                       "cs(User-Agent)",
                       "cs(Referer)",
                       "sc-status",
                       "sc-substatus",
                       "sc-win32-status",
                       "timetaken")
    
    basedate <- as.POSIXct(as.character(iis[1, "date"]), format = "%Y-%m-%d")
    iis$time <- as.POSIXct(paste(iis$date, iis$time))
    iis <- iis[, -1]

    # still struggeling with dates...
    colnames(iis)[1] <- "timestamp" 
    min_ts <- format(iis[1, "timestamp"], "%Y-%m-%d %H:%M:%S")
    max_ts <- iis[nrow(iis), "timestamp"]
    
    iis$timetaken <- as.numeric(iis$timetaken)
    
    # still struggeling with dates...
    from = as.POSIXct(paste(basedate, input$from))
    if (is.na(from)) {
      from = format(min_ts, format = "%H:%M:%S")
      updateTextInput(session, "from", value = from)
    }
    to = as.POSIXct(paste(basedate, input$to))
    if (is.na(to))
      to = as.POSIXct(max_ts)
    
    cat("from: ", as.character(from), "\n")
    cat("to: ", as.character(to), "\n")
    
    # this is not working properly
    # if (is.na(from) || identical(from, ""))
    #    from = as.hms(min(iis$time))
    # if (is.na(to) || identical(to, ""))
    #   to = as.hms(max(iis$time))

    cat("from2: ", as.character(from), "\n")
    cat("to2: ", as.character(to), "\n")
    
    selectedtimes <- iis[iis$timestamp >= from & iis$timestamp <= to, ]
    
    return(selectedtimes)
  })

  output$start <- renderValueBox({
    x <- dataInput()
    if (is.null(x))
      v = 0
    else
      v = as.character(min(x$timestamp), format = "%H:%M:%S")
    valueBox(
      value = v,
      subtitle = "Start time",
      color = "yellow",
      icon = icon("hourglass-start")
    )
  })
  
  output$end <- renderValueBox({
    x <- dataInput()
    if (is.null(x))
      v = 0
    else
      v = as.character(max(x$timestamp), format = "%H:%M:%S")
    
    valueBox(
      value = v,
      subtitle = "End time",
      color = "yellow",
      icon = icon("hourglass-end")
    )
  })
  
  output$min <- renderValueBox({
    x <- dataInput()
    if (is.null(x))
      v = 0
    else
      v = as.character(min(x$timetaken))
    
    valueBox(
      value = v,
      subtitle = "Minimum time",
      color = "green",
      icon = icon("arrow-up")
    )
  })
  
  output$max <- renderValueBox({
    x <- dataInput()
    if (is.null(x))
      v = 0
    else
      v = as.character(max(x$timetaken))
    
    valueBox(
      value = v,
      subtitle = "Maximum time",
      color = "red",
      icon = icon("arrow-down")
    )
  })

  output$mean <- renderValueBox({
    x <- dataInput()
    if (is.null(x))
      v = 0
    else
      v = as.character(round(mean(x$timetaken), 1))
    
    valueBox(
      value = v,
      subtitle = "Mean time",
      color = "blue",
      icon = icon("arrow-left")
    )
  })

  output$median <- renderValueBox({
    x <- dataInput()
    if (is.null(x))
      v = 0
    else
      v = as.character(median(x$timetaken))
    
    valueBox(
      value = v,
      subtitle = "Median time",
      color = "navy",
      icon = icon("arrow-right")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
