# run_analysis.R
# author: tmarkam, for Coursera Data Science: Getting and Cleaning Data Course Project
# date: Sep 4th, 2016
# function: perform an analysis of the UCI HAR Dataset

# Exercise:
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# 0) This script will use the readr and dplyr libraries 
library(readr)
library(dplyr)

# read and process the test and train datasets, and create a summary by activity/subject
create_summary <- function() {
  # read the activity labels into a table - 2-columns: id, name
  actlabels <- read.table("UCI HAR Dataset\\activity_labels.txt",col.names=c("activity_id", "activity"))

  # read column names for the main data file from "features.txt"
  # note: this file contains some duplicate column names
  features <- read.table("UCI HAR Dataset\\features.txt")
  # the second column contains names.  
  # note: use "as.vector" to prevent being returned as a factor variable
  colNames<-as.vector(features[,2])
  # delete parentheses
  colNames <- gsub("[()]","",colNames)
  # replace comma with hyphen
  colNames <- gsub("[,]","-",colNames)
  
  # the raw data is contained in the "X_test.txt" file and consists of a table of 561 columns.  each value is exactly 16 characters wide 
  X_widths <- rep.int(16,561)
  # read the test data file
  testdf <- read_fwf("UCI HAR Dataset\\test\\X_test.txt", 
                     fwf_widths(X_widths), 
                     col_types = cols(.default=col_double()))
  # add the variable names
  names(testdf)<-colNames
  
  # reduce the columns of the dataframe, to "mean" and "std" values - this also eliminates the duplicate names
  testdf<-testdf[,grep("-mean-|-mean$|-std-|-std$",names(testdf))]
  
  # turn it into a tbl
  testtbl <- tbl_df(testdf)
  
  # read the test activity file - a single-column of data
  testact <- read.table("UCI HAR Dataset\\test\\y_test.txt", col.names = c("activity_id"))
  # read the subject file
  testsubj <- read.table("UCI HAR Dataset\\test\\subject_test.txt", col.names = c("subject"))
  # add the activity_id and subject columns to the main data tbl
  testtbl <- testtbl %>% mutate(activity_id=testact$activity_id, subject=testsubj$subject)
  
  # for testing, comment out the following two lines - "train" is a bigger dataset
  # read the train data file
  traindf <- read_fwf("UCI HAR Dataset\\train\\X_train.txt", 
                     fwf_widths(X_widths), 
                     col_types = cols(.default=col_double()))
  # add the variable names
  names(traindf)<-colNames
  
  # reduce the columns of the dataframe, to "mean" and "std" values - this also eliminates the duplicate names
  traindf<-traindf[,grep("-mean-|-mean$|-std-|-std$",names(traindf))]
  
  # turn it into a tbl
  traintbl <- tbl_df(traindf)
  
  # read the train activity file - a single-column of data
  trainactdf <- read.table("UCI HAR Dataset\\train\\y_train.txt",col.names = c("activity_id"))
  # read the train subject file - a single column
  trainsubj <- read.table("UCI HAR Dataset\\train\\subject_train.txt", col.names = c("subject"))
  # add the activity_id and subject columns to the main data tbl
  traintbl <- traintbl %>% mutate(activity_id=trainactdf$activity_id, subject=trainsubj$subject)

  
  # combine the test and train datasets
  combined <- rbind(testtbl, traintbl)
   
  # merge with the activity labels
  combined <- combined %>% merge(actlabels,by.x="activity_id",by.y="activity_id")
  # drop the activity_id column
  combined <- combined %>% select(-activity_id)
  
  # group by activity and subject, for the following summarization
  combined <- combined %>% group_by(activity,subject)
  
  # create the new dataset by summarizing every variable except the group_by vars
  newtbl <- summarize_each(combined, funs(mean(., rm.na=TRUE)))
  
  write.table(newtbl,"UCI HAR Dataset\\summary_by_activity_subject2.txt",row.name=FALSE)

  return("Success!")
}
