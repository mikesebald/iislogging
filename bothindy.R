library(readr)
library(stringr)
library(magrittr)
library(dygraphs)
library(xts)
library(dplyr)

# IIS log file

columnnames <- "date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) cs(Referer) sc-status sc-substatus sc-win32-status time-taken"
columnnames <- gsub("[-()]", "", columnnames)
columnnames <- unlist(strsplit(columnnames, " "))
n_columns <- length(columnnames)

f.tmp <- read_delim("../iislogging_data/u_ex180117.log", " ",
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

# CDH log file

f.tmp <- read_lines("../iislogging_data/server-2018-01-17.log")
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

both <- cbind(cdh_xts, iis_xts)

dy <- dygraph(data = both, main = "Request times taken") %>% 
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE, 
              highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyRangeSelector()
dy
