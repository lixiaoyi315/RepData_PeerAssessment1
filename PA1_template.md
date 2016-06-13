

#Loading and preprocessing the data
```{r simulation,echo=TRUE,results="hide"}
#1.Code for reading in the dataset and/or processing the data
setwd("d:/test data/repdata-data-activity")
activity<-read.csv("./activity.csv")
summary(activity)

```

#What is mean total number of steps taken per day?
```{r,results="hide"}
#2.Histogram of the total number of steps taken each day
#Calculate the total number of steps taken per day,ignore the missing values in the dataset.
activity1<-na.omit(activity)
dayactivity<-aggregate(activity1$steps,activity1[2],sum)
png("plot1.png")
barplot(dayactivity$x,names.arg = dayactivity$date,main = "Total Number of Steps per day",xlab = "Date",ylab = "Total Number Steps")
dev.off()
```
#What is the average daily activity pattern?
```{r,results="hide"}
#3.Mean and median number of steps taken each day
meansteps<-round(mean(dayactivity$x),digits = 2)
mediansteps<-median(dayactivity$x)
```

meansteps is `r meansteps`, mediansteps is `r mediansteps`

```{r,results="hide"}
#4.Time series plot of the average number of steps taken
averagesteps<-aggregate(activity1$steps,activity1[3],mean)
png("plot2.png")
plot(averagesteps,type="l",ylab = "average steps")
dev.off()

#5.The 5-minute interval that, on average, contains the maximum number of steps
averagesteps[which.max(averagesteps$x),"interval"]
#835
```

#Imputing missing values
```{r,results="hide"}
#6.Code to describe and show a strategy for imputing missing data
##compute the total number of missing data
sum(is.na(activity$steps))
##fill the missing data with the average steps per 5m interval
activity2<-activity
for (i in 1:nrow(activity2)){
    if (is.na(activity2[i,"steps"])){
        activity2[i, "steps"] <- averagesteps[averagesteps$interval == activity2[i,"interval"], "x"]
    }
}

#7.Histogram of the total number of steps taken each day after missing values are imputed
dayactivityNew<-aggregate(activity2$steps,activity2[2],sum)
png("plot3.png")
barplot(dayactivityNew$x,names.arg = dayactivityNew$date,main = "Total Number of Steps per day",xlab = "Date",ylab = "Total Number Steps")
dev.off()
meanstepsNew<-mean(dayactivityNew$x)
medianstepsNew<-median(dayactivityNew$x)
```
meansteps is `r meanstepsNew`, mediansteps is `r medianstepsNew`

#Are there differences in activity patterns between weekdays and weekends?
```{r,results="hide"}
#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
c<-Sys.setlocale("LC_TIME","C")
activity3<-activity2
activity3$date<-as.Date(activity3$date)
activity3$wday<-(weekdays(activity3$date)=='Saturday')|(weekdays(activity3$date)=='Sunday')
activity3$wday<-factor(activity3$wday,labels=c("weekday","weekend"))
activity3<-aggregate(steps~interval*wday,data = activity3,mean)
png("plot4.png")
library(ggplot2)
 qplot(interval,steps,data=activity3,facets =wday~.,geom = "line")
dev.off()

```




