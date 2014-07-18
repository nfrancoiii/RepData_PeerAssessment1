# Reproducible Research: Peer Assessment 1
The goal of this document is to process fitness data related to steps taken. THe data is measured in five minute intervals.

Begin by pointing to the directory where the CSV file is saved

```r
setwd("C:\\Users\\Owner\\Google Drive\\PROGRAMMING_SKILLS\\R\\Coursera\\ReproducibleResearch\\repdata-data-activity")
```
## Loading and preprocessing the data
We load the data file and store the data frame as a variable 'data'

```r
data <- read.csv("activity.csv")
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```


## What is mean total number of steps taken per day?
Use 'data' to create a data set 'sumData' that sums the number of steps taken each day.
Create a histogram of the steps taken daily.
Store the mean and median number of steps taken daily as variables 'meanSteps' and 'medianSteps'.

```r
sumData <- aggregate(data$steps, by=list(Date=data$date),FUN = sum, na.rm = TRUE)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
hist(sumData$x, breaks = nrow(sumData))
```

```
## Error: object 'sumData' not found
```

```r
meanSteps <- mean(sumData[[2]])
```

```
## Error: object 'sumData' not found
```

```r
medianSteps <- median(sumData[[2]])
```

```
## Error: object 'sumData' not found
```

## What is the average daily activity pattern?
Use 'data' to create a data set 'intsData' that averages the number of steps taken in each five minute interval across all days.
Plot the data as a time series.
Find the interval in which the maximum number of steps is taken on average daily and store as variable 'maxInterval'.

```r
intsData <- aggregate(data$steps, by=list(Interval=data$interval),FUN = mean,na.rm=TRUE)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
plot(intsData$Interval, intsData$x, type = "l")
```

```
## Error: object 'intsData' not found
```

```r
maxInterval <- intsData[which.max(intsData$x),1]
```

```
## Error: object 'intsData' not found
```


## Imputing missing values
Find the total number of missing entries and store as variable 'totalNAs'.
Replace missing values with average value for that five minute interval.
Loop through the rows of 'data' to populate missing values in 'completeData'.
Create a histogram of the total number of steps taken each day.


```r
totalNAs <- colSums(is.na(data))[1]
```

```
## Warning: is.na() applied to non-(list or vector) of type 'closure'
```

```
## Error: 'x' must be an array of at least two dimensions
```

```r
completeData <- data
for (i in 1:nrow(completeData)) {
        if (is.na(completeData[i,1])==TRUE) {
                missingInt <- completeData[i,3]
                j <- (missingInt/5 + 1)
                completeData[i,1]<- intsData[j,2]
        }
        
}
```

```
## Error: argument of length 0
```

```r
sumCompleteData <- aggregate(completeData$steps, by=list(Date=completeData$date),FUN = sum, na.rm = TRUE)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
hist(sumCompleteData$x, breaks = nrow(sumCompleteData))
```

```
## Error: object 'sumCompleteData' not found
```

```r
meanCompleteSteps <- mean(sumCompleteData[[2]])
```

```
## Error: object 'sumCompleteData' not found
```

```r
medianCompleteSteps <- median(sumCompleteData[[2]])
```

```
## Error: object 'sumCompleteData' not found
```



## Are there differences in activity patterns between weekdays and weekends?
Append a column of weekdays to 'completeData'.
Initialize a column for weekday/weekend binary.
Loop through the rows to define 'Weekends'

```r
completeData$day <- weekdays(as.Date(completeData$date))
```

```
## Error: object of type 'closure' is not subsettable
```

```r
completeData$weekDays <- "Weekdays"
```

```
## Error: object of type 'closure' is not subsettable
```

```r
for (i in 1:nrow(completeData))
        if(completeData[i,4]=="Saturday"|completeData[i,4]=="Sunday"){
                completeData[i,5] <- 'Weekends'
        }
```

```
## Error: argument of length 0
```

```r
weekDayData <- aggregate(completeData$steps, by=list(Interval=completeData$interval,Weekdays=completeData$weekDays),FUN = mean,na.rm=TRUE)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
#plot(intsData$Interval, intsData$x, type = "l")
```
                
