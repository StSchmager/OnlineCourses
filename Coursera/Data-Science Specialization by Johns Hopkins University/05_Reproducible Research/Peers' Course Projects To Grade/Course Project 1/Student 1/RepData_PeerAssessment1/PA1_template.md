# Reproducible Research: Peer Assessment 1




```
## [1] "English_United States.1252"
```



## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


The data of the activity tracker can be loaded with this code:


```r
dat<- read.table("activity.csv", header=T, sep=",")
str(dat) #check correct import
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


2. Process/transform the data (if necessary) into a format suitable for your
analysis

The data are conveniently transformed into a zoo time-series object. This is best done via a function "get.time()":


```r
get.time <- function(activity){
#re-format data into time series
#--------------        
        
        
        #extracts POSIX time from activity.csv data frame
        time.character <- paste(activity$date, formatC(activity$interval, width = 4, 
                                                       format = "d", flag = "0"), sep="-")
        #returns "e.g. "2012-10-01-0000"
        time.POSIXct <- strptime(time.character, format="%Y-%m-%d-%H%M", tz="GMT")
        return(time.POSIXct)
}
```


This is used to import the data into the zoo() object, which facilitates times series manipulation and aggregation.


```r
time.vec        <- get.time(dat)
steps   <- zoo(dat$steps, order.by=time.vec)
```

They look like this. Periods with "NA" values are labelled red.

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 



## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

The individual took on average 9354 steps per day with a median of 10395 steps.

## What is the average daily activity pattern?

The average daily activity pattern is nicely illustrated with  a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The 5-minute interval at 835, contains the maximum number of steps (206.1698113) on average across all the days in the dataset.


## Imputing missing values

In total, the dataset contains 2304 missing values.


A simple strategy for filling in all of the missing values in the dataset is to use the mean for that 5-minute interval as the best estimate. This can be done in the following way:


```r
#get the IDs of the missing values

id.na <- 1:length(steps)
id.na <- id.na[is.na(steps)]

#look for the IDs of the matching interval
id.matching.interval <- match(dat$interval[id.na], index(m3))
```

Then, a new dataset "steps.clean" can be created that is equal to the original dataset but with the missing data filled in.


```r
steps.clean <- steps
steps.clean[id.na] <- as.numeric(m3[id.matching.interval])
```


A new histogram of the total number of steps taken each day reveals a change in patterns. 

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

 
Now, the mean is 10766 and median 11015. These values differ from the estimates from the first part of the assignment by 15 percent (6 for the median). As expected, imputing missing data increases the estimates of the total daily number of steps.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.


```r
dat$daytype <- ifelse(isWeekday(index(steps)), "Weekday", "Weekend")

id.weekday <- which(dat$daytype =="Weekday") #ID's of weekdays
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

Yes, on weekends there are generally fewer steps and the activity pattern shows less activity in the morning and late afternoon. Instead, activity increases in afternoon >15:00 and in the evening around 20:00.
