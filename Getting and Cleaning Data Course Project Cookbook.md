# Getting and Cleaning Data Course Project_Cookbook

#### The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

#### One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
  
#### http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#### Here are the data for the project:
  
#### https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#### You should create one R script called run_analysis.R that does the following.

#### 1. Merges the training and the test sets to create one data set.
#### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#### 3. Uses descriptive activity names to name the activities in the data set
#### 4. Appropriately labels the data set with descriptive variable names.
#### 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## 1. Merges the training and the test sets to create one data set. 
#### Data was download and unzip in the working directory then read into R.

#### 1.1 Load package fot this analysis
library(tidyverse)

#### 1.2 Read train file and merge all colume into one dataframe.
file_X_train <- "./UCI HAR Dataset/train/X_train.txt"
file_y_train <- "./UCI HAR Dataset/train/y_train.txt"
file_subject_train <- "./UCI HAR Dataset/train/subject_train.txt"
x_train <- read.table(file_X_train, header = FALSE)
y_train <- read.table(file_y_train, header = FALSE)
subject_train <- read.table(file_subject_train, header = FALSE)

train <- cbind(y_train, subject_train, x_train)

#### 1.3 Read test file and merge all colume into one dataframe.
file_X_test <- "./UCI HAR Dataset/test/X_test.txt"
file_y_test <- "./UCI HAR Dataset/test/y_test.txt"
file_subject_test <- "./UCI HAR Dataset/test/subject_test.txt"
x_test <- read.table(file_X_test, header = FALSE)
y_test <- read.table(file_y_test, header = FALSE)
subject_test <- read.table(file_subject_test, header = FALSE)

test <- cbind(y_test, subject_test, x_test)

#### 1.4 Merge train and test dateframe into one dataframe
all_data <- rbind(train, test)


#### 1.5 Read variable name from features file and name all variables.
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
names(all_data) <- c("activity", "subject", features$V2)


## 2 Extracts only the measurements on the mean and standard deviation for each measurement.

mean_std<- all_data %>%
  select(activity, subject, contains("mean"), contains("std"))

## 3 Uses descriptive activity names to name the activities in the data set
#### Inport lable information from activity_labels file, then change all acticity in dataframe base on its value.

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
activity_labels <- as.character(activity_labels[,2])
mean_std$activity <- activity_labels[mean_std$activity]


## 4 Appropriately labels the data set with descriptive variable names.
#### Names with abbrevation changed to descriptive name. 

descriptive_name <- names(mean_std)
descriptive_name <- gsub("Acc", "Accelerometer", descriptive_name)
descriptive_name <- gsub("Gyro", "Gyroscope", descriptive_name)
descriptive_name <- gsub("Mag", "Magnitude", descriptive_name)  
descriptive_name <- gsub("^t", "Time", descriptive_name)         
descriptive_name <- gsub("^f", "Frequency", descriptive_name)    
names(mean_std) <- descriptive_name

## 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- mean_std %>%
  group_by(activity, subject) %>%
  summarise_all(mean)
  
## 6 Write tidy_data to file
#### Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() using row.name=FALSE (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).
