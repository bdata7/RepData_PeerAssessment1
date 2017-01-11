---
title: "*Reproducible Research:  Project Assignment #1*"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Loading and Preprocessing the Data

Assuming the project file is available in your working directory, the 
following code will read the data into r and pre-process the data for plotting.

```{r loading the data}
# Read in the data

activity <- read.csv('activity.csv', stringsAsFactors = F)

## Remove NA values & sum the number of steps per day

activity[,2]<- as.POSIXct(activity[,2],format="%Y-%m-%d") 
activity.na.omit <- na.omit(activity)
plot1.data <- aggregate(activity.na.omit$steps,by=list(activity.na.omit$date),
                        FUN=sum)
names(plot1.data) = c('date','steps')
```

## Plot 1:  Histogram of Total Number of Steps Taken Per Day

```{r Plot 1, echo=TRUE}
require(ggplot2)
ggplot(plot1.data, aes(steps)) + 
geom_histogram(color='black',fill='red', binwidth = 5000) + 
theme_bw() + ggtitle('Number of Steps Per Day') +
theme(plot.title = element_text(hjust = 0.5)) +
labs(x='Steps Per Day',y='Frequency')

options(scipen = 999)
mean.plot1 <- round(mean(plot1.data$steps))
median.plot1 <- round(median(plot1.data$steps))

```

The mean is `r mean.plot1`.  
The median is `r median.plot1`.

As shown above, the mean and median are about the same. 

## Plot 2: Determination of Average Daily Activity Pattern

```{r Plot 2, echo=TRUE}
plot2.data <- aggregate(activity.na.omit$steps,
    by=list(activity.na.omit$interval),FUN=mean)
names(plot2.data) = c('Interval','Steps')
plot(plot2.data, type='l',xlab='Interval',ylab='Avg. Number of Steps',
     main='Time Series Plot of Steps Averaged by Interval')
grid()

a <- max(plot2.data$Steps)
b <- round(a)
plot2.interval <- plot2.data[plot2.data$Steps==a,]

```

The average maximum number of steps is `r b`, and the interval with the 
average maximum number of steps is `r plot2.interval$Interval`.

## Plot 3: Imputing Missing Values

The number of complete cases and missing values in the dataset 'activity'
can be determined as follows:

```{r complete cases and missing values}

complete.cases.data <- as.numeric(sum(complete.cases(activity)))
missing.values.data <- as.numeric(sum(!complete.cases(activity)))
```

Thus, the number of complete cases is `r complete.cases.data`, and the 
number of missing values is `r missing.values.data`.

A simple method of replacing the missing values is to replace each missing
value with the mean of the entire dataset. 

```{r replacing missing values, echo = TRUE}
activity2 = activity
activity2$steps[is.na(activity2$steps)] = mean(activity2$steps, na.rm=TRUE)

activity2[,2]<- as.POSIXct(activity2[,2],format="%Y-%m-%d")

plot3.data <- aggregate(activity2$steps,by=list(activity2$date),FUN=sum)
names(plot3.data) = c('date','steps')

ggplot(plot3.data, aes(steps)) + 
    geom_histogram(color='black',fill='red', binwidth = 5000) + 
    theme_bw() + ggtitle('Number of Steps Per Day') +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x='Steps Per Day',y='Frequency')

mean3 <- round(mean(plot3.data$steps))
median3 <- round(median(plot3.data$steps))

```

Since we used the mean of the dataset to substitute for the missing values, the mean of the data in Plot 3 (`r mean3`) is the same as the mean of the data in Plot 1 (`r mean.plot1`).  However, the medians are different, with the median of Plot 3 (`r median3`) being slightly higher than the median from Plot 1 (`r median.plot1`).

## Plot 4:  Differences in Weekdays vs Weekends

The plot below shows the differences in the dataset when collected on a weekday versus a weekend.  Missing values were filled in with the mean of the dataset.

```{r weekdays vs weekends, echo = TRUE}
require(dplyr)
activity2$days <- weekdays(activity2$date)

# Determine weekdays and weekends; label accordingly

weekdays.data <- filter(activity2, days == 'Monday' | days == 'Tuesday' 
                        | days == 'Wednesday' | days == 'Thursday' | days == 'Friday')
weekend.data <- filter(activity2, days == 'Saturday' | days == 'Sunday')

weekdays <- aggregate(weekdays.data$steps,
            by=list(weekdays.data$interval),FUN=mean)
weekend <- aggregate(weekend.data$steps,
           by=list(weekend.data$interval),FUN=mean)

# Make a panel plot in lattice

require(lattice)
xyplot(x~Group.1|which, make.groups(weekdays, weekend),
       xlab = "Interval", ylab = "Number of steps", type = "l", layout = c(1,2)) 

```
