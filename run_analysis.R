# setting wd
setwd("H:\\MyDocs\\Coursera course\\Data Science Specialization\\3. Getting and Cleaning Data\\Week 4\\Getting-and-cleaning-data-assignment")

#Read in label files
activity <- read.table('./UCI HAR Dataset/activity_labels.txt', 
                     col.names = c('activityLabels', 'activityName'), quote = "")

features <- read.table('./UCI HAR Dataset/features.txt', 
                       col.names = c('featureLabels', 'featureName'), quote = "")

## Merges the training and the test sets to create one data set.

#Read in test data
subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt', col.names = c('subjectId'))
x_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')

#Combine all test data and give column names
colnames(x_test) <- features$featureName
colnames(y_test) <- c('activityLabels')
testData <- cbind(subject_test, x_test, y_test)

#Read in training data
subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt', col.names = c('subjectId'))
x_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')

#Combine all training data and give column names
colnames(x_train) <- features$featureName
colnames(y_train) <- c('activityLabels')
trainData <- cbind(subject_train, x_train, y_train)

#Combine test and training data
mergedData <- rbind(trainData, testData)


## Extracts only the measurements on the mean and standard deviation for each measurement.
mean_and_std <- mergedData[, c(1, grep(pattern = 'mean\\(\\)|std\\(\\)', x = names(mergedData)), 563)]


## Uses descriptive activity names to name the activities in the data set.
mean_and_std$subjectId <- as.factor(mean_and_std$subjectId)
mean_and_std$activity <- factor(mean_and_std$activityLabels,
                              levels = activity$activityLabels,
                              labels = activity$activityName)
mean_and_std <- mean_and_std[, -68]
names(mean_and_std)


## Appropriately labels the data set with descriptive variable names.
colnames(mean_and_std) <- gsub(pattern = '\\(\\)', replacement = "", x = names(mean_and_std))
mean_and_std <- mean_and_std[, c(1, 68, 2:67)]
write.table(mean_and_std, file = 'tidyData.txt', row.names = F, quote = F, sep = "\t")


## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
mean_and_std_byIdAct <- group_by(mean_and_std, subjectId, activity) %>% summarise_all(list(mean))
write.table(mean_and_std_byIdAct, file = 'tidyDataMean.txt', row.names = F, quote = F, sep = "\t")