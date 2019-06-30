---
title: "Course Project 1"
author: "Charles Vuono"
date: "June 29, 2019"
output: 
  html_document:
    keep_md: yes
---



###Introduction

We investigate various conclusions on data as provided in the project assignment. The assignment described the data as follows:

>This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

(Source: Course Project assignement)

###Loading and preprocessing the data

We load the data from the file *activity.csv* using the R code:


```r
df<-read.csv("activity.csv", header=TRUE)
```

The data contains many NA values in the field *steps* which we will address later in this report. 

###What is mean total number of steps taken per day?

First we transform the data to find the total number of steps per day (leaving any day with a NA observation as NA so as not to skew the data). We accomplish this with the R code:


```r
dailyDF <- aggregate(df$steps, list(date=df$date), sum)
colnames(dailyDF)<-c("date", "daily.steps")
```

Before calculating the mean and median of steps per day, we can investigate the distribution of steps per day by considering the frequency histogram of the daily steps data.


```r
hist(dailyDF$daily.steps, col="blue", xlab="Steps per Day", main="Frequency Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

To find the mean and median steps per day, we must ignore the NA values in the data and calculate the mean and median as:


```r
mean(dailyDF$daily.steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(dailyDF$daily.steps, na.rm=TRUE)
```

```
## [1] 10765
```

###What is the average daily activity pattern?

In order to see the average daily activity pattern, we transform the data to calculate the average number of steps for each interval using the R code


```r
averagedailyDF<-aggregate(df$steps, list(interval=df$interval), mean, na.rm=TRUE)
colnames(averagedailyDF)<-c("interval", "average.steps")
```

Having aggregated the interval data accross days, we can plot an average daily pattern:


```r
plot(averagedailyDF$average.steps~averagedailyDF$interval, 
     type="l", col="blue",
     main="Average Daily Activity", xlab="Time of Day", ylab="Steps per 5 minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The maximum activity occurs in the interval determined by the following R code:


```r
maxinterval<-averagedailyDF$interval[which.max(averagedailyDF$average.steps)]
```
The **maximum** average steps of **206** occurs at the interval **835**.


### Imputing missing values

As mentioned above, many of the intervals in our data have no data reported (i.e., the *steps* variable is NA). 

The number of such occurences is calculated as follows:


```r
nacount<-sum(complete.cases(df))
percentna<-nacount/nrow(df)
```

In total there are 15264 occurrences, which is 86% of the data.

We shall now transform our data so that the missing values are replaced with approximate values. We shall approximate the NA values using the mean for that five minute interval, which was already calculated and placed in *averagedailyDF$average.steps*. We accomplish this with the R code:


```r
augdf<-df
for(i in 1:nrow(augdf)) {
  if(is.na(augdf$steps[i])){
    subinterval<-augdf$interval[i]
    substeps<-mean(df[df$interval==subinterval,1], na.rm=TRUE)
    augdf$steps[i]<-substeps
  }
}
```


As above we can aggreagte our data to calculate the total steps per day from the augmented data:

```r
augdailyDF <- aggregate(augdf$steps, list(date=df$date), sum)
colnames(augdailyDF)<-c("date", "daily.steps")
```

This augmented version of average daily steps per day will be more accurate since days with a missing observation will not be excluded from out calculations. 

As before we can now create a frequency histogram of the augmented data set and calulate the mean and median as follows:


```r
hist(augdailyDF$daily.steps, col="blue", xlab="Steps per Day", main="Frequency Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean(augdailyDF$daily.steps)
```

```
## [1] 10766.19
```

```r
median(augdailyDF$daily.steps)
```

```
## [1] 10766.19
```

Note that for all calculations involing the augmented data set, we did not include `na.rm=TRUE` in any of the R code, since that was taken care of in the augmentation process. The equality of the mean and median in this case derives from the "median" day being a day in which all observations were NA. Similarly this augmentation method yields very similar results to the results where we ignored all days with a missing interval in the data. 

###Are there differences in activity patterns between weekdays and weekends?
To determine any differences between weekdays and weekends, we first add  a weekday/weekend indiicator variable to the augmented data and then aggregate the data by that indicator accross intervals as follows:


```r
library(dplyr)
library(lattice)
```


```r
dowdf<-augdf %>% mutate(DOW=weekdays(as.Date(date))) %>% 
              mutate( DOWtype=ifelse(DOW=="Saturday" | DOW=="Sunday","Weekend","Weekday"))
dowavedailyDF<-aggregate(dowdf$steps, list(interval=dowdf$interval, DOWtype=dowdf$DOWtype), mean)
colnames(dowavedailyDF)<-c("interval", "DOWtype", "average.steps")

xyplot(average.steps~ interval | DOWtype, data=dowavedailyDF, type="l", 
       main="Average steps by interval by Weekday/Weekend", 
       xlab="Interval", ylab="Average Steps per 5 minute interval",
       scales=list(alternating=FALSE))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

THe graph shows that overall weekend activity levels are higher, although the maximum activity occurs on weekday mornings. 

