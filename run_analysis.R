#1. Merges the training and the test sets to create one data set.

library(tidyverse)
file_X_train <- "./UCI HAR Dataset/train/X_train.txt"
file_y_train <- "./UCI HAR Dataset/train/y_train.txt"
file_subject_train <- "./UCI HAR Dataset/train/subject_train.txt"
x_train <- read.table(file_X_train, header = FALSE)
y_train <- read.table(file_y_train, header = FALSE)
subject_train <- read.table(file_subject_train, header = FALSE)

train <- cbind(y_train, subject_train, x_train)


file_X_test <- "./UCI HAR Dataset/test/X_test.txt"
file_y_test <- "./UCI HAR Dataset/test/y_test.txt"
file_subject_test <- "./UCI HAR Dataset/test/subject_test.txt"
x_test <- read.table(file_X_test, header = FALSE)
y_test <- read.table(file_y_test, header = FALSE)
subject_test <- read.table(file_subject_test, header = FALSE)

test <- cbind(y_test, subject_test, x_test)

all_data <- rbind(train, test)



features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

names(all_data) <- c("activity", "subject", features$V2)


#2 Extracts only the measurements on the mean and standard deviation for each measurement.

mean_std<- all_data %>%
  select(activity, subject, contains("mean"), contains("std"))

#3 Uses descriptive activity names to name the activities in the data set
activity_labels <- as.character(activity_labels[,2])
mean_std$activity <- activity_labels[mean_std$activity]


#4 Appropriately labels the data set with descriptive variable names.
descriptive_name <- names(mean_std)
descriptive_name <- gsub("Acc", "Accelerometer", descriptive_name)
descriptive_name <- gsub("Gyro", "Gyroscope", descriptive_name)
descriptive_name <- gsub("Mag", "Magnitude", descriptive_name)  
descriptive_name <- gsub("^t", "Time", descriptive_name)         
descriptive_name <- gsub("^f", "Frequency", descriptive_name)    
names(mean_std) <- descriptive_name

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- mean_std %>%
  group_by(activity, subject) %>%
  summarise_all(mean)


#6 Write table
write.table(tidy_data, "./tidy_data.txt", row.name = FALSE)
                         
                         
                         
                         