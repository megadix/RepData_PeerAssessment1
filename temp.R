rawData <- read.csv("data/activity.csv")

avgStepsPerInterval <- xtabs(steps ~ interval, rawData)

# work on a copy of raw data
filledData <- rawData
# find indexes of NA values
naIndexes <- which(is.na(filledData$steps))
# replace missing values with average value for that interval
for (i in naIndexes) {
  interval <- filledData[i, 3]
  # use previously calculated contingency table
  replacement <- avgStepsPerInterval[[as.character(interval)]]
  filledData[i, 1] <- replacement
}
