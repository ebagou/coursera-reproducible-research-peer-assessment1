############## Reproducible Research - Peer Graded Assignment: Course Project 1 ##########################################################

setwd("C:/Users/G13519/Documents/EB/Data Specialization/V. Reproducible Research")
library(ggplot2)
library(plyr)
library(reshape2)
library(knitr)

if(!file.exists("./pro1")){dir.create("./pro1")} #Creates new folder if necessary 

unzip(zipfile="C:/Users/G13519/Downloads/repdata%2Fdata%2Factivity.zip",exdir="./pro1")

## Load the data into R
amd<- read.csv("./pro1/activity.csv")

## Q1: What is mean total number of steps taken per day? For this part of the assignment, you can ignore the missing values in the dataset.
stepsD<-split(amd$steps,amd$date)

 # 1.a) Calculate the total number of steps taken per day
  totalD<-as.numeric(sapply(stepsD,sum, na.rm=TRUE))

 # 1.b) If you do not understand the difference between a histogram and a barplot, research the difference between them.
 # Make a histogram of the total number of steps taken each day
 qplot(totalD, binwidth=1000, xlab = "Total steps per day")

 # 1.c) Calculate and report the mean and median of the total number of steps taken per day
 meanD<-mean(totalD)

 medianD<-median(totalD)

## Q2: What is the average daily activity pattern?

 # 2.a) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged 
 # across all days (y-axis)
 
 stepsInt<-split(amd$steps,amd$interval)
 meanInt<-as.numeric(sapply(stepsInt,mean, na.rm=TRUE))
 int<-as.numeric(levels(as.factor(amd$interval)))
 
 ggplot(data=NULL, aes(int, meanInt)) + geom_line()
     + xlab("Interval") + ylab("Average no. of steps")
 
 #2.b) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 xInt<-data.frame(cbind(int,meanInt))
maxInt<-xInt$int[[as.numeric(which.max(xInt$meanInt))]]

## Q3: Imputing missing values
 # 3.a) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
NAm<-na.omit(amd)
no.NA<-as.numeric(nrow(amd)-nrow(NAm))

 # 3.b) Devise a strategy for filling in all of the missing values in the dataset.
 # 3.c) Create a new dataset that is equal to the original dataset but with the missing data filled in.
 amd2<-amd
 for (i in 1: nrow(amd2)) {
     if (is.na(amd2$steps[[i]])){
         amd2$steps[[i]]<-xInt$meanInt[[match(amd2$interval[[i]],xInt$int)]]}
     else { amd2$steps[[i]]<-amd2$steps[[i]]}
 }

 #3.d) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps
 #taken per day. Do these values differ from the estimates from the first part of the assignment?
 #What is the impact of imputing missing data on the estimates of the total daily number of steps?

 stepsD2<-split(amd2$steps,amd2$date)
 totalD2<-as.numeric(sapply(stepsD2,sum, na.rm=TRUE))
 
 qplot(totalD, binwidth=1000, xlab = "Total steps per day")
 
 mean2<-mean(totalD2)
 medianD2<-median(totalD2)
 
 ##Q4: Are there differences in activity patterns between weekdays and weekends?
 
  # 4.a)  Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
 
 wd<-factor(c("weekday", "weekend"))
 
  # 4.b) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of
  #steps taken, averaged across all weekday days or weekend days (y-axis).
 
 wday.or.wend<-function(date){
     d<-weekdays(date)
 if (d %in% c("lunes","martes","miércoles","jueves","viernes"))
     return("weekday") else if (d %in% c("sábado","domingo"))
         return("weekend") else stop("invalid date")
 }
 
 amd2$weekday<-as.Date(amd2$date)
 amd2$d<-sapply(amd2$weekday,FUN=wday.or.wend)

meanIntWDays <- aggregate(steps ~ interval + d, data = amd2, mean)
 
 ggplot(meanIntWDays, aes(interval, steps)) + geom_line() + facet_grid(d~.)
    
