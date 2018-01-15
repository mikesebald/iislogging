library(readr)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)

columnnames <- "date time s-sitename s-computername s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs-version cs(User-Agent) cs(Cookie) cs(Referer) cs-host sc-status sc-substatus sc-win32-status sc-bytes cs-bytes time-taken"
columnnames <- gsub("[-()]", "", columnnames)
columnnames <- unlist(strsplit(columnnames, " "))
n_columns <- length(columnnames)

iis <- read_delim("../iislogging_data/iis3.log", " ",
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

# restart from here

perfbreaks = "1 min"

reqperf <- mutate(df, twindow = cut(timestamp, breaks = perfbreaks)) %>%
  group_by(twindow, reqtype) %>%
  summarise(counter = n())

reqperf <- spread(reqperf, reqtype, counter)

reqperf_xts <- xts(reqperf[, -1], 
                   order.by = as.POSIXct(reqperf$twindow))
dygraph(data = reqperf_xts, main = paste0("Requests per ", perfbreaks)) %>% 
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE, 
              highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyRangeSelector()
