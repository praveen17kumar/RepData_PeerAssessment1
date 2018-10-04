# reproducible peer assessment 1
library("ggplot2")
library("plyr")

activity <- read.csv("activity.csv")

activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
##pulling data without NA's
clean <- activity[!is.na(activity$steps),]

