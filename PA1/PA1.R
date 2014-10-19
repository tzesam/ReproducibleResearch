setwd("C:/Users/tzesam/Desktop/Coursera/ReproducibleResearch/Data")

library(ggplot2)
activity <- read.csv("activity.csv",header=T,colClasses=c("numeric", "character", "numeric"))
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)

str(activity)
    
daysteps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=T)
hist(daysteps,main="Number of steps taken per day", xlab="Steps", col="blue",breaks=10)
mean(daysteps, na.rm=T)
median(daysteps, na.rm=T)

daysteps_mean <- aggregate(x=list(steps=activity$steps), by = list(interval=activity$interval),
                           FUN=mean, na.rm=T)
daysteps_mean$interval <- as.integer(levels(daysteps_mean$interval)[daysteps_mean$interval])
ggplot(daysteps_mean, aes(x=interval, y=steps)) +   
    geom_line(color="blue", size=1) +  
    labs(title="Average Daily Activity Pattern", x="Interval", y="Steps")
daysteps_mean[which.max(daysteps_mean$steps),]

miss <- is.na(activity$steps)
table(miss)
fillstep <- function(steps, int) {
    temp = 0
    if (!is.na(steps)) temp = steps
    else temp = daysteps_mean[daysteps_mean$interval == int, "steps"]
    return(temp)
}

activity_fill <- activity
activity_fill$steps <- mapply(fillstep,activity_fill$steps,activity_fill$interval)
sum(is.na(activity_fill$steps))
daysteps_fill <- tapply(activity_fill$steps, activity_fill$date, FUN=sum, na.rm=T)
hist(daysteps_fill, main="Number of steps taken per day", xlab="Steps",col="green",breaks=10)
mean(daysteps_fill)
median(daysteps_fill)

weekdaytype <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") 
    else if (day %in% c("Saturday", "Sunday")) 
        return("weekend")
    else stop("invalid date")
}
activity_fill$day <- sapply(activity_fill$date, FUN = weekdaytype)
daytype_mean <- aggregate(steps ~ interval + day, data = activity_fill, mean)
ggplot(daytype_mean, aes(as.numeric(interval), steps)) +
    geom_line(col="orange") + facet_grid(day ~ .) + 
    labs(title="Average Daily Activity Pattern", x="Interval", y="Steps") +
    theme_bw()