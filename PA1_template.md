---
title: "Reproducible Research Assignment 1"
author: "Derek Sharp"
date: "October 5, 2016"
output: html_document
---



## R Markdown

This is an R Markdown document for the coursera John Hopkins Data Science - Reproduciable Research Couse. 

For this assigment, we need to do the following analysis for an anonymous individual's step count from Oct to Nov.

1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputing missing data
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

# Reading the Data

```r
step_data_raw = read.csv("C:\\Users\\dsharp\\r_projects\\activity.csv", header=TRUE)
head(step_data_raw)
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
it seems that the data load was good.

for steps 2 - 5, we are ignoring missing data, so the below code removes the missing data.

```r
step_data = step_data_raw[complete.cases(step_data_raw),]
```

# Histogram Total Step Count by Day
First, we need to aggregate the data by date and then take the sum then we can plot the
histrogram

```r
total_step_count_by_date = aggregate(step_data$steps, by=list(step_data$date), sum)
names(total_step_count_by_date)  <- c("Date", "Total_Step_Count")
head(total_step_count_by_date)
```

```
##         Date Total_Step_Count
## 1 2012-10-02              126
## 2 2012-10-03            11352
## 3 2012-10-04            12116
## 4 2012-10-05            13294
## 5 2012-10-06            15420
## 6 2012-10-07            11015
```

```r
hist(total_step_count_by_date$Total_Step_Count,
     main="Histogram of Total Step Count by Day (missing values)",
     xlab="Total Step Count by Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

# Mean and Median for Total Step Count by Day

```r
paste("The mean of the Total Step Count is",
      mean(total_step_count_by_date$Total_Step_Count), 
      sep=" ")
```

```
## [1] "The mean of the Total Step Count is 10766.1886792453"
```

```r
paste("The median of the Total Step Count is",
      median(total_step_count_by_date$Total_Step_Count),
      sep=" ")
```

```
## [1] "The median of the Total Step Count is 10765"
```

# Time Series of Avg. Step Take per Day by Time Interval

```r
total_step_count_by_interval = aggregate(step_data$steps, by=list(step_data$interval), sum)
col_names = c("Time_Interval", "Total_Step_Count")
names(total_step_count_by_interval)  <- col_names
head(total_step_count_by_interval)
```

```
##   Time_Interval Total_Step_Count
## 1             0               91
## 2             5               18
## 3            10                7
## 4            15                8
## 5            20                4
## 6            25              111
```

```r
plot(total_step_count_by_interval$Time_Interval,
     total_step_count_by_interval$Total_Step_Count,
     type="l",
     main="Time Series of Avg. Step Take per Day",
     xlab="Time Interval (5 min intervals)",
     ylab="Avg Step Count per Day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)




From the plot, we can see the max step count happens somewhere between 800-900. Futhermore, the overtrend seems to be high activity from 800 - 900 and medium activity between 1000 to 2000.


```r
max_step = total_step_count_by_interval[max(total_step_count_by_interval$Total_Step_Count) ==
                               total_step_count_by_interval$Total_Step_Count,]
paste("The max step count is", 
      max_step$Total_Step_Count,
      "for time interval",
      max_step$Time_Interval,
      sep=" ")
```

```
## [1] "The max step count is 10927 for time interval 835"
```

# Analysis with missing data filled in
The strategy I choose to use to fill in the missing data is to use the average step count
for that time interval.

First, let's identify the number of missing data points

```r
sum(is.na(step_data_raw$steps))
```

```
## [1] 2304
```

Second, calculate the average step count by time interval using the cleaned data step from the earlier analysis

```r
avg_step_count_by_interval = aggregate(step_data$steps, by=list(step_data$interval), mean)
names(avg_step_count_by_interval) <- c("Interval", "Avg_Step_Count")
head(avg_step_count_by_interval)
```

```
##   Interval Avg_Step_Count
## 1        0      1.7169811
## 2        5      0.3396226
## 3       10      0.1320755
## 4       15      0.1509434
## 5       20      0.0754717
## 6       25      2.0943396
```
Third, take a copy of th raw data set and then fill in the missing values with th average step count by time interval

```r
step_data_filled = step_data_raw
for( i in unique(step_data_filled[is.na(step_data_filled$steps),'interval'])) {
  step_data_filled[is.na(step_data_filled$steps) & step_data_filled$interval == i,'steps'] =
    avg_step_count_by_interval[avg_step_count_by_interval$Interval == i,'Avg_Step_Count']
}
head(step_data_filled)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
Last, let's check to see if all the missing data points have been filled.

```r
sum(is.na(step_data_filled$steps))
```

```
## [1] 0
```
# Histogram Total Step Count by Day with the Missing Values Filled-in

```r
total_step_count = aggregate(step_data_filled$steps, by=list(step_data_filled$date), sum)
names(total_step_count) <- c("Date", "Total_Step_Count")
hist(total_step_count$Total_Step_Count,
     main="Histogram of Total Step Count by Date (filled-in values)",
     xlab="Total Step Count by Date")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

# Mean and Median for Total Step Count by Day

```r
paste("The mean of the Total Step Count is",
      mean(total_step_count$Total_Step_Count), 
      sep=" ")
```

```
## [1] "The mean of the Total Step Count is 10766.1886792453"
```

```r
paste("The median of the Total Step Count is",
      median(total_step_count$Total_Step_Count),
      sep=" ")
```

```
## [1] "The median of the Total Step Count is 10766.1886792453"
```
# Missing-DataFrame vs. Filled-in-Dataframe
With missing data, the mean and median were 10766.19 and 10765 respectivly. While for the data-filled-in dataset, the mean and median 10766.19 and 10766.19. As one might expect, filing in the missing values with averages by time interval causes the total step count to trend more towards the first mean and median. This can be seen in the histogram as well. The second plot is much more centered around the mean.

# Weekday vs. Weekend Analysis
Using the dataset with the filled in missing values, we will now compare the weekday step activity against weekend step activity

First, let's change the datatype of the date column to a data object

```r
step_data_filled$date = as.Date(step_data_filled$date, '%Y-%m-%d')
head(step_data_filled)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
Second, let's add a new column to the dataframe called weekday

```r
step_data_filled$is_weekend= factor(weekdays(step_data_filled$date) %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
       levels=c('TRUE', 'FALSE'), labels=c("Weekday", "Weekend"))
```
Third, aggregate the dataframe by interval then by weekday and take the mean.

```r
avg_stp_count_by_interval_weekday = aggregate(step_data_filled$steps,
              by=list(step_data_filled$interval,
                      step_data_filled$is_weekend), mean)
names(avg_stp_count_by_interval_weekday) <- c("interval", "weekday", "avg_step")
```

```r
par(mfrow=c(2,1))
plot(avg_stp_count_by_interval_weekday[avg_stp_count_by_interval_weekday$weekday ==
                                         'Weekday',c('interval','avg_step')], 
     type="l",
     main="Weekday Time Series",
     xlab="Time Interval (5 min)",
     ylab="Avg Step per Day")

plot(avg_stp_count_by_interval_weekday[avg_stp_count_by_interval_weekday$weekday ==
                                         'Weekend',c('interval','avg_step')],
     type="l",
     main="Weekend Time Series",
     xlab="Time Interval (5 min)",
     ylab="Avg Step per Day")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

From the plot, one can see that the general trend of activity is the same versus weekday and weekend, but the intensity for any time after the 1000 time interval seems to be noticable higher for the weekend.
