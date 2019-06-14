# Coursera : Getting and Cleaning Data - Week 4 Peer graded assignment
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.

# load library
library(dplyr)

# set working directory
setwd("C:/ADAPT/GettingCleaningData/Week4_Assignment/UCI HAR Dataset")

# read features 
features <- read.table("./features.txt") 

# read activity labels 
activity_labels <- read.table("./activity_labels.txt") 

# read train data 
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/Y_train.txt") 
subject_train <- read.table("./train/subject_train.txt")

# read test data 
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/Y_test.txt") 
subject_test <- read.table("./test/subject_test.txt")

# merge train and test sets
merge_x <- rbind(x_train, x_test)
merge_y <- rbind(y_train, y_test) 
merge_subject <- rbind(subject_train, subject_test) 

# Extracts only the measurements on the mean and standard deviation for each measurement.
selected_features <- features[grep(".*mean\\(\\)|std\\(\\)", features[,2], ignore.case = FALSE),]
merge_x <- merge_x[,selected_features[,1]]

# name columns
colnames(merge_x)   <- selected_features[,2]
colnames(merge_y)   <- "activity"
colnames(merge_subject) <- "subject"

# Merges the training and the test sets to create one data set.
merge_set <- cbind(merge_subject,merge_y,merge_x)

# Uses descriptive activity names to name the activities in the data set
# and appropriately label the data set with descriptive variable names.
merge_set$activity <- factor(merge_set$activity, levels = activity_labels[,1], labels = activity_labels[,2]) 
merge_set$subject  <- as.factor(merge_set$subject) 

# create a second, independent tidy data set with the average of each variable 
# for each activity and each subject.
merge_mean <- merge_set %>% group_by(activity, subject) %>% summarize_all(funs(mean)) 

# export tidy dataset
write.table(merge_mean, file = "./tidy_data.txt", row.names = FALSE, col.names = TRUE) 