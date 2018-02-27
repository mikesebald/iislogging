library(readr)
library(stringr)
library(ggplot2)
library(data.table)

f.tmp <- read_lines("e:/temp/server-2018-02-26.log")

# subsetting on rows only which have a time-taken timestamp
cleans <- grepl("No changes detected:|Record saved", f.tmp)
clean_rows <- f.tmp[cleans]

# extracting the timestamp and the time-taken 
timestamp <- str_extract(clean_rows, "\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2},\\d{3}")
timetaken <- str_extract(clean_rows, "\\[\\d+ms\\]$") %>%
  str_extract("\\d+")

dt <- as.data.table(cbind(timestamp, timetaken))
dt[, timetaken := as.numeric(timetaken)]
dt[, timestamp := gsub(",", ".", df[, timestamp])]
dt[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%OS")]

# View(dt)

dt2 <- dt[timestamp > as.POSIXct("2018-02-26 12:04:19")]

td_min <- min(dt2[, timetaken], na.rm = TRUE)
td_max <- max(dt2[, timetaken], na.rm = TRUE)
td_mean <- mean(dt2[, timetaken], na.rm = TRUE)

min(dt2[, timestamp])
max(dt2[, timestamp])

interval <- "1 min"
cutvar <- cut(dt2[, timestamp], interval)
# levels(cutvar)
dt2_split <- split(dt2, cutvar)
numputs <- unlist(lapply(dt2_split, nrow))
dt3 <- data.table("timeinterval" = names(dt2_split), numputs)

nputs_min <- min(dt3[numputs > 0, numputs])
nputs_max <- max(dt3[numputs > 0, numputs])
nputs_mean <- signif(mean(dt3[numputs > 0, numputs]))

p1 <- ggplot(dt2, aes(x = timestamp, y = timetaken)) +
  geom_line(size = 1, colour = "blue") +
  #  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  geom_hline(yintercept = td_max, size = 1, colour = "red") +
  geom_hline(yintercept = td_mean, size = 1, colour = "green") +
  geom_hline(yintercept = td_min, size = 1, colour = "pink") +
  ggtitle("Put time as reported in server.log") + 
  xlab("Time") +
  ylab("Duraction [ms]") +
  theme_minimal()
p1

dt2[timetaken > 2000, .N]

dt2a <- dt2[timetaken <= 2000, ]
td2_min <- min(dt2a[, timetaken], na.rm = TRUE)
td2_max <- max(dt2a[, timetaken], na.rm = TRUE)
td2_mean <- mean(dt2a[, timetaken], na.rm = TRUE)
p1a <- ggplot(dt2a, aes(x = timestamp, y = timetaken)) +
  geom_line(size = 1, colour = "blue") +
  #  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  geom_hline(yintercept = td2_max, size = 1, colour = "red") +
  geom_hline(yintercept = td2_mean, size = 1, colour = "green") +
  geom_hline(yintercept = td2_min, size = 1, colour = "pink") +
  ggtitle("Put time as reported in server.log") + 
  xlab("Time") +
  ylab("Duraction [ms]") +
  theme_minimal()
p1a

p2 <- ggplot(dt2, aes(x = timetaken)) +
  ggtitle("Distribution of put times") + 
  xlab("time-taken interval [secs]") +
  ylab("Number of puts per interval") +
  geom_histogram(bins = 50, fill = "blue") +
  theme_minimal()
p2

prop.table(table(cut(dt2[, timetaken], c(0, 50, 100, 250, 500, 1000, 2000)))) * 100

p3 <- ggplot(dt3, aes(x = timeinterval, y = numputs)) +
  geom_bar(size = 1, fill = "blue", stat = "identity") +
  geom_hline(yintercept = nputs_max, size = 1, colour = "red") +
#  annotate("text", x = 100, y = nputs_max + 1, label = nputs_max) +
  geom_hline(yintercept = nputs_mean, size = 1, colour = "green") +
#  annotate("text", x = 100, y = nputs_mean + 1, label = nputs_mean) +
  geom_hline(yintercept = nputs_min, size = 1, colour = "pink") +
#  annotate("text", x = 100, y = nputs_min + 1, label = nputs_min) +
  scale_x_discrete(breaks = NULL) +
  ggtitle(paste("Number of puts per interval:", interval)) + 
  xlab("Time") +
  ylab("Number of puts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p3
