# Check data Dir if exists
if(!file.exists("./data")){dir.create("./data")}
# download and unzip data
dataUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile<-"/Users/Quito/OneDrive/Coursera/ReproductibleResearch/ReproducibleResearchPeerAssessment-1/data/repdata-data-activity.zip"
download.file(dataUrl,destfile)

unzip("/Users/Quito/OneDrive/Coursera/ReproductibleResearch/ReproducibleResearchPeerAssessment-1/data/repdata-data-activity.zip",exdir="data") #unzip
# store data in new variable
activityData <- read.csv("./data/activity.csv")

# show summary
summary(activityData)

# show names and head of data
names(activityData)
head(activityData)

# plot graphics from activity data
pairs(activityData)

# 2. Histogram of the total number of steps taken each day
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
hist(stepsPerDay$steps)

# 3. Calculate and report the mean and median of the total number of steps taken per day
#mean Steps per Day
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay

# median Steps per Day
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay

# 4. Time series plot of the average numbers of steps taken
# make time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis) 
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")

# 5. The 5-minute interval that, on average, contains the maximum number of steps
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps


# 6. Code to describe and show a strategy for imputing missing data
# calculate the total number of missing values in data and store it to var=totalValuesMissing
totalValuesMissings <- sum(is.na(activityData$steps))
totalValuesMissings

# fill in all missing values in the dataset with the mean per interval. 
getMeanStepsPerInterval<-function(interval){
  stepsPerInterval[stepsPerInterval$interval==interval,]$steps}
# Create a new dataset (var=activityDataNoNA) that is equal to the original include the mean per interval
activityDataNoNA<-activityData
for(i in 1:nrow(activityDataNoNA)){
  if(is.na(activityDataNoNA[i,]$steps)){
    activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
  }
}

# 7. Histogram of the total number of steps taken each day after missing values are imputed
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)

# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# create var with two criteria – weekday and weekend 

activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date, abbreviate = FALSE)
for (i in 1:nrow(activityDataNoNA)) {
  if (activityDataNoNA[i,]$day %in% c("Samstag","Sontag")) {
    activityDataNoNA[i,]$day<-"weekend"
  }
  else{
    activityDataNoNA[i,]$day<-"weekday"
  }
}

stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)

# aggregate steps+interval+day bay mean
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
names(stepsByDay) <- c("interval", "day", "steps")

library(lattice)
# panel plot the results
xyplot(steps ~ interval | day, stepsByDay, type = "l", col="darkgreen", layout = c(1, 2), 
       xlab = "Interval", ylab = "No. of Steps", main ="Interval across weekdays and weekends")





