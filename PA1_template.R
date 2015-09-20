#Load the data
dataset <- read.csv("activity\\activity.csv", sep=",", header=TRUE)

#What is mean total number of steps taken per day?
#Make a histogram of the total number of steps taken each day
library(ggplot2)
total_steps <- tapply(dataset$steps, dataset$date, FUN = sum, na.rm = TRUE)
qplot(total_steps, binwidth = 1000, xlab = "Total Number of Steps Taken Per Day", 
      ylab = "Count",geom="histogram",alpha=I(.2),main = "Histogram for Total Number of Steps",fill=I("blue"), 
      col=I("red"))

#Calculate and report the mean and median total number of steps taken per day
mean(total_steps, na.rm = TRUE)
median(total_steps, na.rm = TRUE)

#What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all days (y-axis)
library(ggplot2)
averages <- aggregate(x = list(steps = dataset$steps), by = list(interval = dataset$interval), 
                      FUN = mean, na.rm = TRUE)
ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
  ylab("Average Number of Steps Taken")

#Which 5-minute interval, on average across all the days in the dataset,
#contains the maximum number of steps?
averages[which.max(averages$steps), ]

#Imputing missing values
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

missing_value <- is.na(dataset$steps)
# How many missing
table(missing_value)

# Replace each missing value with the mean value of its 5-minute interval
fill_value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else 
    filled <- (averages[averages$interval == interval, "steps"])
  return(filled)
}
filled_data <- dataset
filled_data$steps <- mapply(fill_value, filled_data$steps, filled_data$interval)

#Make a histogram of the total number of steps taken each day and Calculate
#and report the mean and median total number of steps taken per day.
total_steps <- tapply(filled_data$steps, filled_data$date, FUN = sum)
qplot(total_steps, binwidth = 1000, xlab = "Total Number of Steps Taken Per Day", 
      ylab = "Count",geom="histogram",alpha=I(.2),main = "Histogram for Total Number of Steps",fill=I("blue"), 
      col=I("red"))

mean(total_steps)
median(total_steps)

#Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels - "weekday"
#and "weekend" indicating whether a given date is a weekday or weekend day.
weekday_or_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
    return("weekday") 
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else 
    stop("invalid date")
}
filled_data$date <- as.Date(filled_data$date)
filled_data$day <- sapply(filled_data$date, FUN = weekday_or_weekend)

#Make a panel plot
averages <- aggregate(steps ~ interval + day, data = filled_data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
  xlab("5-minute interval") + ylab("Number of Steps")


