# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Import libraries we will use:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(lattice)
```

**Load the data:**

Load data directly from the zip file:


```r
con <- unz('activity.zip', 'activity.csv')
activity <- tbl_df(read.csv(con))
```

**Process/transform the data (if necessary) into a format suitable for your analysis:**

Convert date column:


```r
activity <- activity %>% mutate(date = ymd(date))
```

Display a summary:


```r
str(activity)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

**Calculate the total number of steps taken per day:**


```r
steps_per_day <- activity %>%
    group_by(date) %>%
    summarise(total = sum(steps))
```

**Make a histogram of the total number of steps taken each day:**


```r
hist(steps_per_day$total,
     main="Total Steps per Day",
     xlab="Steps",
     ylab="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

**Calculate and report the mean and median of the total number of steps taken per day:**

Mean:


```r
mean_steps_per_day <- mean(steps_per_day$total, na.rm=TRUE)
mean_steps_per_day
```

```
## [1] 10766.19
```

Median:


```r
median_steps_per_day <- median(steps_per_day$total, na.rm=TRUE)
median_steps_per_day
```

```
## [1] 10765
```

## What is the average daily activity pattern?

**Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis):**


```r
interval_summary <- activity %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarise(average = mean(steps))
plot(interval_summary,
     type="l",
     main="Average Steps by Interval",
     xlab="Interval",
     ylab="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

**Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?**


```r
row_index <- which(interval_summary$average == max(interval_summary$average))
interval_summary[row_index,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```

## Imputing missing values

**Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):**


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

**Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

The strategy is to use the mean for the 5-minute interval, since we've prepared that data earlier:


```r
activity_filled <- inner_join(activity, interval_summary, by = c("interval"))
activity_filled[is.na(activity_filled$steps),]$steps <-
    activity_filled[is.na(activity_filled$steps),]$average
```

**Create a new dataset that is equal to the original dataset but with the missing data filled in.**

The fill was completed above, now we just match structure to the original dataset:


```r
activity_filled <- activity_filled %>% select(-average)
str(activity_filled)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

**Make a histogram of the total number of steps taken each day:**


```r
steps_per_day_filled <- activity_filled %>%
    group_by(date) %>%
    summarise(total = sum(steps))
hist(steps_per_day_filled$total,
     main="Total Steps per Day",
     xlab="Steps",
     ylab="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

**Calculate and report the mean and median total number of steps taken per day:**


```r
mean_steps_per_day_filled <- mean(steps_per_day_filled$total)
mean_steps_per_day_filled
```

```
## [1] 10766.19
```

Median:


```r
median_steps_per_day_filled = median(steps_per_day_filled$total)
median_steps_per_day_filled
```

```
## [1] 10766.19
```

**Do these values differ from the estimates from the first part of the assignment?**

Not by much.  Mean difference:


```r
abs(mean_steps_per_day - mean_steps_per_day_filled)
```

```
## [1] 0
```

Median difference:


```r
abs(median_steps_per_day - median_steps_per_day_filled)
```

```
## [1] 1.188679
```

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**

While the mean and median stay quite close (as shown above), the total count will increase since missing values have been populated.  Here's the difference in the total count between the original and NAs-filled-in datasets:


```r
abs(sum(activity$steps, na.rm=TRUE) - sum(activity_filled$steps))
```

```
## [1] 86129.51
```

## Are there differences in activity patterns between weekdays and weekends?

**Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**


```r
activity_filled <- activity_filled %>%
    mutate(day_type = as.factor(
        ifelse(weekdays(date) %in% c("Saturday", "Sunday"),
               "weekend",
               "weekday"))
        )
```

**Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**


```r
interval_summary_filled <- activity_filled %>%
    group_by(day_type, interval) %>%
    summarise(average = mean(steps))
xyplot(average~interval|day_type,
       data=interval_summary_filled,
       layout=c(1,2),
       xlab="Interval",
       ylab="Number of steps",
       type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png) 
