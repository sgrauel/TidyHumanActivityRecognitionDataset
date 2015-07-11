# TASK: to process the Human Activity Recognition Dataset, http://bit.ly/1mEvWTG
# INPUT : raw HAR dataset in directory 'UCI HAR'
# OUTPUT : a tidy datatable stacked wide written to a file named 'tidyHAR.txt' where every observation corresponding to a 
# row is a subject and activity factor group and every independent variable corresponding to a collumn is either a measurement
# filtered by mean or standard deviation. In addition, the table describes *ONE* experiment object, 30 individuals wearing a 
# smartphone embedded with an accelerometer and gyroscope measuring linear acceleration and angular velocity respectively. For
# more details on the experiement consult the following reference, , and the link to the UCI Machine Learning Repo page above.

# USAGE INSTRUCTIONS: 
#   - first set your present working directory to the directory that contains the 'UCI HAR Dataset' directory
#   - now load and call the 'processHAR' function
# tip : to load a function in RStudio highlight the function and press CTRL-ENTER

processHAR <- function() {

  # read in the training set and test set into data frames
  trainingSet <- read.table("./UCI HAR Dataset/train/X_train.txt")
  testSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
  
  # label 561 attributes for each data frame
  colAttrs <- read.table("./UCI HAR Dataset/features.txt")
  colAttrs <- as.character(colAttrs[,c(2)])
  colnames(trainingSet) <- colAttrs
  colnames(testSet) <- colAttrs
  
  library(data.table)
  
  # cast data frames to data tables
  # NOTE : alt: tbl_df
  trainingTbl <- data.table(trainingSet)
  testTbl <- data.table(testSet)
  
  # read in activity labels for train and test sets
  activityTrainLbl <- read.table("./UCI HAR Dataset/train/y_train.txt")
  activityTestLbl <- read.table("./UCI HAR Dataset/test/y_test.txt")
  
  # read in subject labels for train and test sets
  subjectTrainLbl <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  subjectTestLbl <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  # collumn bind activity labels and subject labels to train and test sets respectively
  trainingTbl[,activity:=activityTrainLbl]
  trainingTbl[,subject:=subjectTrainLbl]
  
  testTbl[,activity:=activityTestLbl]
  testTbl[,subject:=subjectTestLbl]
  
  
  # STEP 1: Merges the training table and the test table into one by row
  # NOTE: test table followed by training table
  
  mergedTbl <- rbind(testTbl,trainingTbl)
  
  # STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement.
  # => select only collumns with mean and standard deviation related filters with activty and subject collumn binded
  
  library(dplyr)
  
  mergedTbl <- mergedTbl %>% 
    select(which(grepl("mean()",names(mergedTbl)) | 
                   grepl("std()",names(mergedTbl)) | 
                   grepl("activity",names(mergedTbl)) |
                   grepl("subject",names(mergedTbl))))
  
  
  # STEP 3: Use descriptive activity names to name the activities in the data set
  
  # task: convert integer labels to equivalent string labels
  intToStrLabels  <- function(label) {
    if (label == 1) "WALKING"
    else if (label == 2) "WALKING_UPSTAIRS"
    else if (label == 3) "WALKING_DOWNSTAIRS"
    else if (label == 4) "SITTING"
    else if (label == 5) "STANDING"
    else "LAYING"
  }
  
  # mutate the activity collumn in arranged mean and sd tables sorted by subject and activity to new string labels
  strLabels <- sapply(mergedTbl[["activity"]],intToStrLabels)
  mergedTbl[,activity:=factor(strLabels)]
  
  # STEP 4 : Appropriately label the data set with descriptive variable names
  
  attrNames <- names(mergedTbl)
  res1 <- gsub("tBodyAcc","LinAccBodyOverTime",attrNames)
  res2 <- gsub("tGravityAcc","LinAccGravityOverTime",res1)
  res3 <- gsub("Jerk","_JerkSignal",res2)
  res4 <- gsub("Mag","_Magnitude",res3)
  res5 <- gsub("tBodyGyro","AngVelBodyOverTime",res4)
  res6 <- gsub("fBodyAcc","FreqLinAccBodyOverTime",res5)
  res7 <- gsub("fBodyGyro","FreqAngVelBodyOverTime",res6)
  res8 <- gsub("fBodyBodyAcc","BodyFreqLinAccBodyOverTime",res7)
  res9 <- gsub("fBodyBodyGyro","BodyFreqAngVelBodyOverTime",res8)
  
  setnames(mergedTbl,seq(1,length(names(mergedTbl))),res9)
  
  
  #  STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average
  # of each variable for each activity and each subject. =>
  # split the data into factor groups by subject and activity labels 1.1 1.2 1.3 1.4 1.5 ... 30.1 30.2 30.3 30.4 30.5
  # i.e. group by subject and activity labels and apply mean over the groups
  
  # group by activity and subject
  grpSubActTbl <- mergedTbl %>% group_by(subject,activity)
  
  # summarize each factor group applying the mean over the groups
  meanGrpSubActTbl <- grpSubActTbl %>% summarise_each(funs(mean))
  
  # sort by subject and activity for mean on each attribute
  arrangedMeanGrpSubActTbl <- meanGrpSubActTbl %>% arrange(subject,activity)
  
  # separate data into two tables linked by foreign key field 'group_id'
  stdArrangedMeanGrpSubActTbl <- arrangedMeanGrpSubActTbl %>% select(which(grepl("std()",names(arrangedMeanGrpSubActTbl)) |
                                                                             grepl("activity",names(arrangedMeanGrpSubActTbl)) |
                                                                             grepl("subject",names(arrangedMeanGrpSubActTbl))))
  
  meanArrangedMeanGrpSubActTbl <- arrangedMeanGrpSubActTbl %>% select(which(grepl("mean()",names(arrangedMeanGrpSubActTbl)) |
                                                                              grepl("activity",names(arrangedMeanGrpSubActTbl)) |
                                                                              grepl("subject",names(arrangedMeanGrpSubActTbl))))
  
  # left collumn bind a foreign key field named 'group_id' of 1 ... 180 integer values to both tables
  group_id <- 1:180
  stdArrangedMeanGrpSubActTbl <- cbind(group_id,stdArrangedMeanGrpSubActTbl)
  meanArrangedMeanGrpSubActTbl <- cbind(group_id,meanArrangedMeanGrpSubActTbl)
  
  
  # 'tidyHARmean.txt or 'tidyHARsd.txt' exists => overwrite them
  # 'tidyHARmean.txt' or 'tidyHARsd.txt' dne => create and write to them 
  if (!file.exists("./tidyHARmean.txt") ||  !file.exists("./tidyHARmeansd.txt")) {
    file.create("./tidyHARmean.txt")
    file.create("./tidyHARsd.txt")
    write.table(meanArrangedMeanGrpSubActTbl, file = "./tidyHARmean.txt", row.names = FALSE, quote = FALSE)
    write.table(stdArrangedMeanGrpSubActTbl, file = "./tidyHARsd.txt", row.names = FALSE, quote = FALSE)
  } else {
    write.table(meanArrangedMeanGrpSubActTbl, file = "./tidyHARmean.txt", row.names = FALSE, quote = FALSE)
    write.table(stdArrangedMeanGrpSubActTbl, file = "./tidyHARsd.txt", row.names = FALSE, quote = FALSE)
  }
}
































