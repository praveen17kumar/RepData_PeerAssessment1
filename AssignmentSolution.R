# reproducible peer assessment 1
library("ggplot2")
library("plyr")

activity <- read.csv("activity.csv")

activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
##pulling data without NA's
clean <- activity[!is.na(activity$steps),]

## summarizing total steps per date
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")

## Creating the historgram of total steps per day
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")

## Mean of Steps
as.integer(mean(sumTable$Steps))
## Median of Steps
as.integer(median(sumTable$Steps))
