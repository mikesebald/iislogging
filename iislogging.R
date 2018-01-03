library(readr)
library(hms)

iis <- read_delim("E:/temp/iis.log", " ", 
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

iis$timetaken <- as.numeric(iis$timetaken)
iis$time <- as.hms(iis$time)

from <- as.hms("11:16:00")
to <- as.hms("11:16:09")

selectedtimes <- iis[iis$time >= from & iis$time <= to, "timetaken"]$timetaken
mean(selectedtimes)
max(selectedtimes)
min(selectedtimes)
median(selectedtimes)

hist(selectedtimes)
