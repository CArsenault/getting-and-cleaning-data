#Getting and Cleaning Data Course Project

##############################################################################
#
# FILE
#   run_analysis.R
#
# OVERVIEW 
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
#
#   Set Working Directory, if not done already

library(dplyr)

###Start by retrieving data

# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

# read features
features <- read.delim("features.txt", header = FALSE, sep = "")

# read training data
trainingSubjects <- read.delim("train/subject_train.txt", header = FALSE, sep = "")
trainingActivity <- read.delim("train/y_train.txt", header = FALSE, sep = "")
trainingValues <- read.delim("train/X_train.txt", header = FALSE, sep = "")

#add column names; training data
colnames(trainingSubjects) <- "subjectId"
colnames(trainingActivity) <- "activityId"
colnames(trainingValues) <- features[, 2]

# read test data
testSubjects <- read.delim("test/subject_test.txt", header = FALSE, sep = "")
testValues <- read.delim("test/X_test.txt", header = FALSE, sep = "")
testActivity <- read.delim("test/y_test.txt", header = FALSE, sep ="")

#add column names; test data
colnames(testSubjects) <- "subjectId"
colnames(testActivity) <- "activityId"
colnames(testValues) <- features[, 2]

#create data sets
train_final <- cbind(trainingSubjects, trainingActivity, trainingValues)
test_final  <- cbind(testSubjects, testActivity, testValues)

data_final  <- rbind(train_final, test_final)


# remove individual data tables 
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# Extract measurements on the mean and standard deviation
colNames <- colnames(data_final)
colstokeep = (grepl("activity..", colNames) | grepl("subject..",colNames) | grepl("-mean()..", colNames) | grepl("-std()..-", colNames))

### Use descriptive activity names to name the activities in the data set
# First read activity types
activityType <- read.table('./activity_labels.txt',header=FALSE)
colnames(activityType)  <- c('activityId','activityType')
#then merge final data set with the activity type
data_final              <- merge(data_final, activityType, by='activityId', all.x=TRUE)
#update the column names
colNames                <- colnames(data_final)

###Appropriately label the data set with descriptive variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(data_final) = colNames

### Create a second, independent tidy data set with the average of each variable 
# for each activity and each subject.

#create table without activity type
data_final_no_activity_type <- data_final[, names(data_final) != 'activityType']

#summarize, include mean
data_tidy <- aggregate(data_final_no_activity_type[, names(data_final_no_activity_type) != c('activityId', 'subjectId')], 
                       by = list(activityId = data_final_no_activity_type$activityId, 
                                 subjectId = data_final_no_activity_type$subjectId), mean)
#merge to include activity names
data_tidy <- merge(data_tidy, activityType, by='activityId', all.x=TRUE)

###finally, export the data
write.table(data_tidy, './data_tidy.txt',row.names=FALSE)   
