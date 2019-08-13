---
title: "Reproducible Research: Peer Assessment 1"
author: RG Willhoft
date: 8/12/2019
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data

1. Load the data (`read.csv()`


```r
unzip("activity.zip")
step_data <- read.csv("activity.csv", stringsAsFactors = FALSE)
str(step_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
step_data$date <- as.Date(step_data$date, "%Y-%m-%d")
range(step_data$date)
```

```
## [1] "2012-10-01" "2012-11-30"
```


```r
summary(step_data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
summary(step_data$interval)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   588.8  1177.5  1177.5  1766.2  2355.0
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
steps_per_day <- with(step_data, tapply(steps, date, sum, na.rm = TRUE))
str(steps_per_day)
```

```
##  int [1:61(1d)] 0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(steps_per_day,
     xlab = "Steps per Day",
     main = "Histogram of Total Steps per Day"
)
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean_steps_per_day <- mean(steps_per_day)
median_steps_per_day <- median(steps_per_day)
```

The mean (average) number of steps per day is 9354 
and the median is 10395 steps.


## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_per_interval <- with(step_data, tapply(steps, interval, mean, na.rm = TRUE))
str(steps_per_interval)
```

```
##  num [1:288(1d)] 1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:288] "0" "5" "10" "15" ...
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
plot(x = names(steps_per_interval),
     y = steps_per_interval,
     xlab = "Interval",
     main = "Average Steps by 5 Minute Interval",
     type = "l"
)
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
na_values = sum(is.na(step_data$steps))
na_values
```

```
## [1] 2304
```

```r
na_percent = 100 * na_values / nrow(step_data)
na_percent
```

```
## [1] 13.11475
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

<!-- Note you can remove head from these to see all the data --->


```r
all(table(step_data$interval) == 61)
```

```
## [1] TRUE
```

```r
all(table(step_data$interval[is.na(step_data$steps)]) == 8)
```

```
## [1] TRUE
```


```r
all(table(step_data$date) == 288)
```

```
## [1] TRUE
```

```r
table(step_data$date[is.na(step_data$steps)])
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
step_data_imputed <- step_data
bad <- is.na(step_data$steps)
interval_index <- match(step_data$interval[bad],names(steps_per_interval))
step_data_imputed$steps[bad] <- steps_per_interval[interval_index] 
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
steps_per_day_imputed <- with(step_data_imputed, tapply(steps, date, sum, na.rm = TRUE))
par(mfrow = c(1,2))
hist(steps_per_day,
     xlab = "Steps per Day",
     main = "Histogram of Total Steps per Day"
)
hist(steps_per_day_imputed,
     xlab = "Steps per Day",
     main = "Histogram of Total Steps per Day"
)
```

![](PA1_template_files/figure-html/total_steps_imputed-1.png)<!-- -->


```r
steps_per_interval_imputed <- with(step_data_imputed, tapply(steps, interval, mean, na.rm = TRUE))
plot(x = names(steps_per_interval),
     y = steps_per_interval,
     xlab = "Interval",
     main = "Average Steps by 5 Minute Interval",
     type = "l",
     col = "blue"
)
lines(x = names(steps_per_interval_imputed),
     y = steps_per_interval,
     col = "red"
)
```

![](PA1_template_files/figure-html/per_interval_imputed-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekend <- weekdays(step_data_imputed$date) %in% c("Saturday", "Sunday")
step_data_imputed$weekday <- factor( weekdays(step_data$date) %in% c("Saturday", "Sunday") )
levels(step_data_imputed$weekday) <- c("weekday", "weekend")
```


2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(dplyr)
steps_by_weekday <- step_data_imputed %>%
                    group_by(interval,weekday) %>%
                    summarize(steps = mean(steps)
)
```


```r
library(lattice)
xyplot(steps ~ interval | weekday, 
       data = steps_by_weekday, 
       layout = c(1,2),
       type = "l"
)
```

![](PA1_template_files/figure-html/plot_weekday-1.png)<!-- -->

