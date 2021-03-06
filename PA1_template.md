# Reproducible Research: Peer Assessment 1



The following report is my submission to the first peer assessment of the
coursera course "Reproducible Research", which is part of the Data Science
specialization containing my analysis of movement data.

The analysis includes answering the questions for mean and median number of steps
and illustrates the overall distribution of the total mean number of steps per day for
the original dataset and a dataset created during the analysis where missing values
have been imputed. The strategy for imputing the missing values is described and
explained as well. Furthermore, the overall activity pattern of a day and the
activity patterns of weekdays and weekends were examined.

I expect the data to be present on the computer in a ZIP file as it is supplied with the assignment instructions and explicitly noted there. If you do not have it, you can download it from [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

In case you do not want to download it by hand, you can use the following R code, which will download and save the ZIP file to your current working directory.


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
```

## Loading and preprocessing the data

The process of loading and preprocessing the data is straightforward. At first,
I unzip the ZIP file that contains the data. Unzipping the file creates a new file,
"activity.csv", which contains the data that is used for this assignment. Now I read
this newly created file and store its content. As the file is a CSV file, it is
stored in a data.frame. 

After loading the data into a data.frame, I process it slightly: I change the
class of the date column to Date so that I can calculate the day of the week
for the dates, which will be necessary for the last question of this
assignment.


```r
unzip('activity.zip')
data <- read.csv('activity.csv')
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

To answer this question, I first aggregate steps as a function of the date applying
the sum function. Afterwards I create a histogram using the aggregated dataset.


```r
stepsPerDay <- aggregate(steps ~ date, data, sum, na.action = na.omit)
stepsPerDayPlot <- ggplot(stepsPerDay, aes(x=steps)) +
    geom_histogram(binwidth=(max(stepsPerDay$steps)-min(stepsPerDay$steps)) / 25) +
    labs(title='Mean total number of steps per day',
         x='Number of steps',
         y='Frequency') 
stepsPerDayPlot
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Based on the data which was created by aggregation in the last section, I calculate
the mean and the median number of steps taken per day as follows:


```r
stepsMean <- mean(stepsPerDay$steps, na.rm=TRUE)
stepsMedian <- median(stepsPerDay$steps, na.rm=TRUE)
```

Due to these calculations the mean number of steps taken each day is around **10766.19**, the
median is at **10765**.

## What is the average daily activity pattern?

At first I aggregate the steps as a function of the interval applying the mean
function. As in the last section, I now use this aggregate dataset to create a plot.
In this case, it is a line plot.


```r
stepsPerInterval <- aggregate(steps ~ interval, data, mean, na.action = na.omit)
ggplot(stepsPerInterval, aes(x=interval, y=steps)) +
    geom_line(size=0.725) +
    labs(title='Average daily activity pattern',
         x='Interval',
         y='Number of steps')
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

A glance at the figure above suggests that the 5-minute interval with the maximum 
number of steps, averaged across all days, is the peek between 750 and 1000.
The following code based on the aggregated data from the last section computes that
interval.


```r
stepsMax <- stepsPerInterval[which.max(stepsPerInterval$steps),]
print(stepsMax)
```

```
##     interval    steps
## 104      835 206.1698
```

Therefore, it is the 835^th^ interval which contains with about
206.17 (on average) the maximum number of steps.

## Imputing missing values

A lot of observations in the dataset are incomplete, which means that value for 
the steps is missing. I calculated the amount of those incomplete observations as
follows:


```r
table(is.na(data$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

Obviously, there are 2304 observations, where there is no
value present for steps, while 15264 do have one. Thus,
13.11% 
of the observations do not contain any value for steps. 

The amount is not too high, though it still may introduce some bias into some
calculations as stated in the assessment instructions. To overcome that problem
I developed a strategy for imputing the missing values.

I therefore consider two different options:

1. Replace a missing value with the mean or the median of the day.
2. Replace a missing value with the mean or the median of the specific interval.

To decide between one of those strategies I make a little experiment. I examine
the distribution of the missing values.

Looking at the missing values, I discover the following: Every interval contains
the exact same amount of missing values.


```r
NAs <- tapply(data$steps, data$interval, function(dat) {sum(is.na(dat))})
summary(NAs)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       8       8       8       8       8       8
```

This suggests that there are 8 complete days where there are no values at all.
To proof this, I make the following attempt:

1. Create a list with vectors for all intervals containing a
logical value if steps is missing for that interval on a day or not
2. Comparing the days where intervals have missing values by using the logical
OR-operator on the whole list of vectors. The resulting logical vector identifies
days where at least one interval contains a missing value.
3. Summing up the days with missing values.


```r
isNA <- tapply(data$steps, data$interval, is.na)
result <- rep(FALSE, length(isNA[[1]]))

for(i in seq_along(isNA)) {
    result <- result | isNA[[i]]
}

sum(result)
```

```
## [1] 8
```

The result is 8. When every interval has exactly 8 missing values
and there are just 8 days that contain missing values at all, there
have to be exactly 8 days that have missing values for every interval.
The following calculation shows this even better. Every day is divided into 5-minute
intervals, so there should be eight days that have 288 NA values for steps:


```r
tapply(data$steps, data$date, function(dat) {sum(is.na(dat))})
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0        288          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0        288          0          0        288          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0        288        288          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0        288          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##        288
```

As the table shows that is true. There are eight days that do not have any values
at all.

Thus, the first strategy does not seem to be promising, so I applied the 
second one using the mean to impute the values.


```r
means <- tapply(data$steps, data$interval, mean, na.rm=T, simplify=F)
imputed.data <- data
imputed.data$steps <- mapply(function(step, interval) {
    if(is.na(step))
        return (means[[as.character(interval)]])
    else
        return (step)
}, imputed.data$steps, imputed.data$interval)
```

To further improve the strategy, one could look at the dates of the days whose
values are missing. If they belong to just certain parts of the week (e.g. weekends),
it would improve the imputing by using only the values of the days that belong to
the same part of the week.

Having the dataset with imputed values, it is of interest to re-answer the first
question asked to compare the results and to figure out, whether the missing values
led to any bias or not.


```r
imputed.stepsPerDay <- aggregate(steps ~ date, imputed.data, sum, na.action = na.omit)
imputed.plot <- ggplot(imputed.stepsPerDay, aes(x=steps)) +
    geom_histogram(binwidth=(max(imputed.stepsPerDay$steps)-min(imputed.stepsPerDay$steps)) / 25) +
    labs(title='Mean total number of steps per day (imputed missing values)',
         x='Number of steps',
         y='Frequency') 
imputed.plot
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Median and mean are computed as before.


```r
imputed.stepsMean <- mean(imputed.stepsPerDay$steps, na.rm=TRUE)
imputed.stepsMedian <- median(imputed.stepsPerDay$steps, na.rm=TRUE)
```

With the imputed values the mean is around **10766.19**
and the median is around **10766.19**.

### Comparison

To compare the both I first create a plot showing visualizing the imputed data
as well as the original data.


```r
stepsPerDay <- cbind(stepsPerDay, rep(FALSE, length(stepsPerDay$steps)))
imputed.stepsPerDay <- cbind(imputed.stepsPerDay, rep(TRUE, length(imputed.stepsPerDay$steps)))
colnames(stepsPerDay)[3] <- "Imputed"
colnames(imputed.stepsPerDay)[3] <- "Imputed"

comparison.stepsPerDay <- rbind(stepsPerDay, imputed.stepsPerDay)

ggplot(comparison.stepsPerDay, aes(x=steps)) +
    geom_histogram(binwidth=(max(comparison.stepsPerDay$steps)-min(comparison.stepsPerDay$steps)) / 25,
                   aes(fill=Imputed), position="dodge") +
    labs(title='Mean total number of steps per day',
         x='Number of steps',
         y='Frequency')
```

![](./PA1_template_files/figure-html/unnamed-chunk-15-1.png) 

As one can see imputing the missing values has not affected most parts of the distribution
at all, however, the center of the distribution, the peek, has doubled. This is due to
the fact that before imputing the missing values, there were exactly eight complete days
that lacked values. By using the interval mean to impute the missing values, the overall
number of steps for those days was naturally identical and very close to the mean and
the median of the other days. Thus, imputing the missing values has not changed very much.
The mean stayed with around 10766.19 the same. The median rose
slightly from 10765 to 10766.19, but not significantly.

To sum up, on can say that this particular strategy for imputing missing values has
no impact on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

To answer this last question of the assignment, I first extend the dataset with the
imputed values with another column "day" to divide the data into the two groups,
"Weekend" or "Weekday". To do so, I calculate the day of the week for the date values
of every row. If the day of the week is a day of the weekend, the row gets the annotation
"Weekend" as a value in the column "day", "Weekday" otherwise.


```r
imputed.data$day <- sapply(imputed.data$date, function(date) {
    if (weekdays(date) %in% c("Saturday", "Sunday"))
        "Weekend"
    else
        "Weekday"
})
```

Now that I have an extra column "day" to distinguish between weekend and weekday, I 
can aggregate the steps as a function of the interval *and* the "day". Afterwards
I use this aggregated data and plot it.


```r
imputed.stepsPerInterval <- aggregate(steps ~ interval + day, imputed.data, mean)

ggplot(imputed.stepsPerInterval, aes(interval, steps)) +
    geom_line() +
    facet_grid(day ~ .) +
    labs(title="Activity patterns on weekdays and weekends",
         x="Interval",
         y="Number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

As one can see, on weekdays, the activity pattern has essentially one peek: in
the morning when the person probably got up and got to work. After that peek, 
there is still some activity, however, it is rather low. In this period of time,
there are three smaller peeks. The two in the middle may refer to some breaks, the
last one is probably due to the way back home.

On weekends on the other hand, the activity pattern has a lot of peeks and the
overall activity appears to be higher, although all peeks are quite a bit lower 
than the highest peek of the weekday pattern. This can have a number of reasons.
The person might have an office job, where he could be sitting at a desk most of
time during the weekdays. On weekends he might do some sport or work in the
garden - if there is one - which could explain the higher activity on weekends.

Furthermore, the person seems to start off the day a bit later on weekends
than on weekdays as the number of steps rises earlier in the weekday pattern
than on the weekend pattern.
