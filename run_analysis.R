##
## 1.  Merges the training and the test sets to create one data set.
## 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.  Uses descriptive activity names to name the activities in the data set
## 4.  Appropriately labels the data set with descriptive variable names. 
## 5.  From the data set in step 4, creates a second, independent tidy data set with 
##     the average of each variable for each activity and each subject.

runAnalysis <- function(){

  # ensure the correct libraries are loaded
  require("data.table")
  require("reshape2")
  require("dplyr")
  
  #download the file to be used
  if(!file.exists("./Dataset.zip")){
    fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = "./Dataset.zip")
  }
  unzip("./Dataset.zip")
  
  #load meta data
  features <- read.table("UCI HAR Dataset/features.txt")
  activities <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
  
  #load training data set
  trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
  trainActivity <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
  trainFeatures <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
  
  #load test data set
  testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
  testActivity <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
  testFeatures <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
  
  #combine the training and test data to create one data set
  dfSubject <- rbind(trainSubject, testSubject)
  dfActivity <- rbind(trainActivity, testActivity)
  dfFeatures <- rbind(trainFeatures, testFeatures)
  
  # human readable activity labels
  dfActivity <- merge(dfActivity, activities, by.x="V1", by.y = "V1")
  
  #human readable column names
  colnames(dfSubject) <- "subject"
  colnames(dfActivity) <- c("activityID", "activity")
  colnames(dfFeatures) <- features$V2
  
  dfAll <- cbind(dfSubject, dfActivity, dfFeatures)
  
  #extract columns with mean or std deviation data
  dfMeanStd <- dfAll[, c(1,3,grep(".*mean.*|.*std.*", names(dfAll), ignore.case=TRUE))]
  measureLabels = setdiff(names(dfMeanStd), c("subject", "activity"))
  dfMeanStd <- melt(dfMeanStd, id = c("subject", "activity"), measure.vars = measureLabels)
  
  #apply mean to dataset, order, and write file
  tidyData <- dcast(dfMeanStd, subject + activity ~ variable, mean)
  tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
  write.table(tidyData, file = "./tidyData.txt", row.names = FALSE)
  
}