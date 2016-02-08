# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load and Transform the data

```r
unzip(zipfile="data/activity.zip")
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
library(ggplot2)
totalSteps <- tapply(data$steps, data$date, FUN=sum, na.rm = TRUE)
```
2. histogram of the total number of steps taken each day

```r
qplot(totalSteps, binwidth = 1000, xlab = "total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

3. mean and median of the total number of steps taken per day

```r
mean(totalSteps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(totalSteps, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
ggplot(averages, aes(interval, steps)) + geom_line(lwd = 1) + xlab("5-minute interval") + ylab("average number of steps taken across all-days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averages$interval[which.max(averages$steps)]
```

```
## [1] 835
```
3. maximum number of steps across all intervals

```r
max(averages$steps)
```

```
## [1] 206.1698
```

## Inputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing.values <- is.na(data$steps)
table(missing.values)
```

```
## missing.values
## FALSE  TRUE 
## 15264  2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
steps.fill <- function(steps, interval) {
    steps.filled <- NA
    if (!is.na(steps))
        steps.filled <- c(steps)
    else
        steps.filled <- (averages[averages$interval==interval, "steps"])
    return(steps.filled)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data.filled <- data
data.filled$steps <- mapply(steps.fill, data.filled$steps, data.filled$interval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of inputing missing data on the estimates of the total daily number of steps?


```r
totalSteps <- tapply(data.filled$steps, data.filled$date, FUN=sum, na.rm = TRUE)
qplot(totalSteps, binwidth = 1000, xlab = "total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)

```r
mean(totalSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```
#### Mean and Median are higher when steps values are filled with 5-minute interval averages than when they are NA's.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data.filled$date <- as.Date(data.filled$date)
data.filled <- mutate(data.filled, day = weekdays(date))
data.filled[data.filled$day %in% c("Saturday","Sunday"),4] <- "Weekend"
data.filled[data.filled$day != "Weekend",4] <- "Weekday"
data.filled$day <- as.factor(data.filled$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
averages <- aggregate(steps ~ interval + day, data=data.filled, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)
