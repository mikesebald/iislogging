library(readr)
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)

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

df <- iis[, c("timestamp", "reqtype")]

reqperf <- mutate(df, twindow = cut(timestamp, breaks = "1 mins")) %>%
  group_by(twindow, reqtype)

reqperf <- spread(reqperf, reqtype, counter)

reqperf_xts <- xts(reqperf[, -1], 
                   order.by = as.POSIXct(reqperf$twindow))
dygraph(data = reqperf_xts, main = "Requests per minute") %>% 
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyRangeSelector()

