## Step 1: Merges the training and the test sets to create one data set.

# Merge train data
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
trainData <- cbind(X_train, y_train, subject_train)

# Merge test data
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")
testData <- cbind(X_test, y_test, subject_test)

# Merge train and test data
mergedData <- rbind(trainData, testData)


## Step 2: Extracts only the measurements on the mean and standard 
## deviation for each measurement

# Get the names of the features and select only those that has 
# mean() and std() has part of the label names
features <- read.table("features.txt")
grep_MeanStd <- grep("mean\\(\\)|std\\(\\)", features[,2])
data <- mergedData[,c(grep_MeanStd, 562,563)] # Include y label and subject data


## Step 3: Uses descriptive activity names to name the activities in 
## the data set
## Step 4: Appropriately labels the data set with descriptive variable
## names 
## *** Since merging the data with the activity labels will reorder the data,
## I chose the label the data set first as to avoid confusion after merging

featureNames <- as.character(features[grep_MeanStd, 2])
editedFeatureNames <- gsub("-", "_", featureNames)
editedFeatureNames <- gsub("std\\(\\)", "std", editedFeatureNames)
editedFeatureNames <- gsub("mean\\(\\)", "mean", editedFeatureNames)
names(data) <- c(editedFeatureNames, "numActivity", "subject")

# Get activity labels
activity_labels <- read.table("activity_labels.txt")
names(activity_labels) <- c("numActivity", "activity")

# Replace the number for descriptive names for the activity column
data <- merge(data, activity_labels, by="numActivity")
# Remove numActivity column
data <- data[,-1]


## Step 5: From the data set in step 4, creates a second, independent
## tidy data set with the average of each variable for each activity 
## and each subject

library(reshape2)
library(dplyr)
meltdata <- melt(data, id=c("subject", "activity"))
tidyData <- dcast(meltdata, subject + activity ~ variable, mean)

# Write data to file
write.table(tidyData, "tidyData.txt", row.name=FALSE)
