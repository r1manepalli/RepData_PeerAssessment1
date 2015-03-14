---
title: 'Title : Peer Assessment1 R Markdown file'
author: "r1"
date: "Saturday, March 14, 2015"

output:
html_document:
    keep_md: true
    
---

#This is an R Markdown document. For the Peer Assessment 1
#This document has both code and results



```r
library(knitr)
library(data.table)
library(ggplot2) 
library(plyr)

opts_chunk$set(echo = TRUE, results = 'hold')
```

#Loading and preprocessing the data
#cleaning the data
##Demostrating the sample of the data


```r
activitydata <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
activitydata$date <- as.Date(activitydata$date, format = "%Y-%m-%d")
activitydata$interval <- as.factor(activitydata$interval)
head(activitydata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
# The mean total number of steps taken per day
##1. the total number of steps taken per day without NA
### We format the data into a string and produce a mean to show the top few rows

```r
str(activitydata)
steps_per_day <- aggregate(steps ~ date, activitydata, sum, na.rm=TRUE)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```


##2. histogram of the total number of steps taken each day
### We are producing a ggplot for the activity file

```r
ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "red", binwidth = 1000) + theme_bw() +
        labs(title="Histogram for Activiy file without nulls", 
             x = "Steps per Day", y = "Count per day") 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

##3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean_of_steps   <- mean(steps_per_day$steps, na.rm=TRUE)
median_of_steps <- median(steps_per_day$steps, na.rm=TRUE)

mean_of_steps
median_of_steps
```

```
## [1] 10766.19
## [1] 10765
```



#the average daily activity pattern
##1. Making a time series line chart plot using geom_line of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)



```r
steps_interval <- aggregate(activitydata$steps, by = list(interval = activitydata$interval), FUN=mean, na.rm=TRUE)
steps_interval$interval <- 
        as.integer(levels(steps_interval$interval)[steps_interval$interval])
colnames(steps_interval) <- c("interval", "steps")
ggplot(steps_interval, aes(x=interval, y=steps)) +  theme_bw() + 
        geom_line(color="blue", size=2) +    geom_jitter() +
        labs(title="Daily Activity Pattern", x="Interval", y="Steps")  
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


#2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
 steps_int <-steps_interval[which.max(  
        steps_interval$steps),]
```

The step with the maximum step intervals is  835, 206.1698113

#Imputing missing values
##1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing_steps <- sum(is.na(activitydata$steps))
```

The total missing steps are 2304

##2. Devising a strategy for filling in all of the missing values in the dataset.

```r
na_fill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

activity_fill <- data.frame(  
        steps = na_fill(activitydata, steps_interval),  
        date = activitydata$date,  
        interval = activitydata$interval)
str(activity_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```
The total missing steps are 0


##3.Creating a new dataset that is equal to the original dataset but with the missing data filled in.

```r
all_steps_per_day <- aggregate(steps ~ date, activity_fill, sum)
colnames(all_steps_per_day) <- c("date","steps")

head(all_steps_per_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

##4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```r
ggplot(all_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "yellow", binwidth = 1000) + theme_bw() +
        labs(title="Histogram of All Steps Taken per Day after NA fill", 
             x = "All Steps per Day", y = "count") 
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


## Missing value analysis:

```r
All_mean_steps   <- mean(all_steps_per_day$steps)
All_median_steps <- median(all_steps_per_day$steps)

All_mean_steps
All_median_steps
```

```
## [1] 10766.19
## [1] 10766.19
```
There is a very small differnce between mean_of_steps and All_mean_step.
There is a very small differnce between median_of_steps and All_median_steps.

#THe differences in activity patterns between weekdays and weekends
##1. Create a new factor variable in the dataset with two levels ??? weekday and weekend indicating whether a given date is a weekday or weekend day.


```r
weekdaytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activitydata$weekdaytype <- as.factor(sapply(activitydata$date, weekdaytype))
```


##2. Making a panel plot containing a time series line plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days. 


```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    agg_weekdays <- aggregate(steps ~ interval, data = activitydata, subset = activitydata$weekdaytype == 
        type, FUN = mean)
    plot(agg_weekdays , type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

