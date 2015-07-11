---
title: "README"
author: "sgrauel"
date: "July 10, 2015"
output:
  md_document:
    variant: markdown_github
    toc: true
    toc_depth: 2
---

# Getting and Cleaning Data Course Project

## OVERVIEW:
**TASK:** to process [the Human Activity Recognition Dataset](http://bit.ly/1mEvWTG)  

**INPUT:** raw HAR dataset in directory 'UCI HAR'  

**OUTPUT:** two tidy datatables stacked wide written in a one to one correspondance to files 'tidyHARMean.txt' and 'tidyHARsd.txt' 
respectively where every observation corresponding to a row is a subject and activity factor group and every independent variable 
corresponding to a collumn is either a measurement estimated by mean or standard deviation by the data collectors. 

## TABLE STRUCTURE AND SEMANTICS:
the tables themeselves describe *ONE* experiment object of high dimensionality, however its attributes are selected into two subsets 
thereby described by the collumns of *TWO* tables to denote the nuance of **the experiment's estimation on mean and standard deviation** 
when processing the 3-axial raw signals of linear acceleration and angle velocity utilizing experimental filters.  

## THE EXPERIMENT: 
The general experiment is as follows: 30 individuals wearing a smartphone strapped to the belt embedded with an accelerometer and 
gyroscope measuring linear acceleration and angular velocity respectively when performing 6 distinct tasks. See *activity* collumn 
for task values.  

> For more info visit: 
>   - [UCI Machine Learning Repo page](http://bit.ly/1mEvWTG)
>   - features_info.txt, README.txt in raw in 'UCI HAR' provide details recorded by the collector

## USAGE INSTRUCTIONS: 
  - first *set your present working directory* to the directory that contains the 'UCI HAR Dataset' directory
  - now *load and call* the 'processHAR' function
  
## DATA PROCESSING PROCEDURE: 

read in the training set and test set into data frames  


```r
  trainingSet <- read.table("./UCI HAR Dataset/train/X_train.txt")
  testSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
```

label 561 attributes for each data frame  


```r
  colAttrs <- read.table("./UCI HAR Dataset/features.txt")
  colAttrs <- as.character(colAttrs[,c(2)])
  colnames(trainingSet) <- colAttrs
  colnames(testSet) <- colAttrs
```

cast data frames into data tables  


```r
  library(data.table)
  trainingTbl <- data.table(trainingSet)
  testTbl <- data.table(testSet)
```

read in activity labels for train and test sets  


```r
  activityTrainLbl <- read.table("./UCI HAR Dataset/train/y_train.txt")
  activityTestLbl <- read.table("./UCI HAR Dataset/test/y_test.txt")
```

read in subject labels for train and test sets  

```r
  subjectTrainLbl <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  subjectTestLbl <- read.table("./UCI HAR Dataset/test/subject_test.txt")
```

collumn bind activity labels and subject labels to train and test sets respectively  

```r
  trainingTbl[,activity:=activityTrainLbl]
  trainingTbl[,subject:=subjectTrainLbl]
  
  testTbl[,activity:=activityTestLbl]
  testTbl[,subject:=subjectTestLbl]
```
  
**STEP 1:** Merges the training table and the test table into one by row 
*NOTE:* test table followed by training table  


```r
  mergedTbl <- rbind(testTbl,trainingTbl)
```

**STEP 2:** Extracts only the measurements on the mean and standard deviation for each 
measurement => select only collumns with mean and standard deviation related filters 
with activty and subject collumn binded  


```r
  library(dplyr)
  
  mergedTbl <- mergedTbl %>% 
    select(which(grepl("mean()",names(mergedTbl)) | 
                   grepl("std()",names(mergedTbl)) | 
                   grepl("activity",names(mergedTbl)) |
                   grepl("subject",names(mergedTbl))))
```

**STEP 3:** Use descriptive activity names to name the activities in the data set


```r
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
```

**STEP 4:** Appropriately label the data set with descriptive variable names  


```r
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
```

**STEP 5:** From the data set in step 4, creates a second, independent tidy data set with the average 
of each variable for each activity and each subject. => split the data into factor groups by subject 
and activity labels 1.1 1.2 1.3 1.4 1.5 ... 30.1 30.2 30.3 30.4 30.5 i.e. group by subject and activity 
labels and apply mean over the groups. Then sort by activity and subject.  


```r
  # group by activity and subject
  grpSubActTbl <- mergedTbl %>% group_by(subject,activity)
  
  # summarize each factor group applying the mean over the groups
  meanGrpSubActTbl <- grpSubActTbl %>% summarise_each(funs(mean))
  
  # sort by subject and activity for mean on each attribute
  arrangedMeanGrpSubActTbl <- meanGrpSubActTbl %>% arrange(subject,activity)
```

Tidying up let us classify the attributes into those whose signals are estimated for the mean and standard deviation respectively to obtain two datatables linked by a foreign key field of unique integers 1, 2 ... 
180  


```r
  # separate data into two tables linked by foreign key field 'group_id'
  
  stdArrangedMeanGrpSubActTbl <- arrangedMeanGrpSubActTbl %>%           select(which(grepl("std()",names(arrangedMeanGrpSubActTbl)) |                                                                  grepl("activity",names(arrangedMeanGrpSubActTbl)) |                                                              grepl("subject",names(arrangedMeanGrpSubActTbl))))
  
  meanArrangedMeanGrpSubActTbl <- arrangedMeanGrpSubActTbl %>% select(which(grepl("mean()",names(arrangedMeanGrpSubActTbl)) |                                                                 grepl("activity",names(arrangedMeanGrpSubActTbl)) |
              grepl("subject",names(arrangedMeanGrpSubActTbl))))
  
  # left collumn bind a foreign key field named 'group_id' of 1 ... 180 integer values to both tables
  group_id <- 1:180
  stdArrangedMeanGrpSubActTbl <- cbind(group_id,stdArrangedMeanGrpSubActTbl)
  meanArrangedMeanGrpSubActTbl <- cbind(group_id,meanArrangedMeanGrpSubActTbl)
```
write our tables in a one to one correspondance to two files according to the following rule:  

> if 'tidyHARmean.txt or 'tidyHARsd.txt' exists, then overwrite them
> otherwise 'tidyHARmean.txt' or 'tidyHARsd.txt' does not exist, then create and write to them  


```r
  if (!file.exists("./tidyHARmean.txt") ||  !file.exists("./tidyHARmeansd.txt")) {
    file.create("./tidyHARmean.txt")
    file.create("./tidyHARsd.txt")
    write.table(meanArrangedMeanGrpSubActTbl, file = "./tidyHARmean.txt", row.names = FALSE, quote = FALSE)
    write.table(stdArrangedMeanGrpSubActTbl, file = "./tidyHARsd.txt", row.names = FALSE, quote = FALSE)
  } else {
    write.table(meanArrangedMeanGrpSubActTbl, file = "./tidyHARmean.txt", row.names = FALSE, quote = FALSE)
    write.table(stdArrangedMeanGrpSubActTbl, file = "./tidyHARsd.txt", row.names = FALSE, quote = FALSE)
  }
```

## CODE BOOK:

**description:** there are two tidy datasets stacked wide, the first classifies those variables for mean value estimates during the experiment, the other classifies variables for standard deviation estimated attributes. 
Both tables are unified by a foreign key whose values uniquely identify each observation. In addition, the observations were grouped by subject and activity and a mean was applies over the distict groups to summarize.

### mean value estimated attributes:

**subject :** integer value concerning any of thirty individuals who participated in the study  

**activity :** string value indicating one of six different acitivities being performed by the individuals in the study  

**LinAccBodyOverTime-mean()-X  
LinAccBodyOverTime-mean()-Y  
LinAccBodyOverTime-mean()-Z :** the *estimated means* of the X-Y-Z directions regarding the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. The acceleration signal was then separated into a *body acceleration signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz.  

**LinAccGravityOverTime-mean()-X  
LinAccGravityOverTime-mean()-Y  
LinAccGravityOverTime-mean()-Z :** the *estimated means* of the X-Y-Z directions regarding the 3 axial raw signals describing *linear acceleration of gravity with respect to time* captured at a contant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. The acceleration signal was then separated into a *gravity acceleration signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

**LinAccBodyOverTime_JerkSignal-mean()-X  
LinAccBodyOverTime_JerkSignal-mean()-Y  
LinAccBodyOverTime_JerkSignal-mean()-Z :** the *estimated means* of the X-Y-Z directions regarding the *jerk signals* derived from the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz. First the signals were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Then, the acceleration signal was then separated into a *body acceleration signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

**AngVelBodyOverTime-mean()-X  
AngVelBodyOverTime-mean()-Y  
AngVelBodyOverTime-mean()-Z :** the *estimated means* of the X-Y-Z directions regarding the 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz. First the signals were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Then, the velocity signal was then separated into a *body velocity signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

**AngVelBodyOverTime_JerkSignal-mean()-X  
AngVelBodyOverTime_JerkSignal-mean()-Y  
AngVelBodyOverTime_JerkSignal-mean()-Z :** the *estimated means* of the X-Y-Z directions regarding the *jerk signals* derived from 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz. First the signals were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Then, the velocity signal was then separated into a *body velocity signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

**LinAccBodyOverTime_Magnitude-mean() :** the *estimated means* of calculating *magnitude* of X-Y-Z linear acceleration signals *regarding a body* using a Euclidian norm

**LinAccGravityOverTime_Magnitude-mean() :** the *estimated means* of calculating *magnitude* of X-Y-Z linear acceleration signals *regarding gravity* using a Euclidian norm

**AngVelBodyOverTime_JerkSignal_Magnitude-mean() :** the *estimated means* of calculating *magnitude* of *jerk signals* derived from X-Y-Z angular velocity signals *regarding a body* using a Euclidian norm

**LinAccBodyOverTime_JerkSignal_Magnitude-mean() :** the *estimated means* of calculating *magnitude* of *jerk signals* derived from X-Y-Z linear acceleration signals *regarding a body* using a Euclidian norm

**AngVelBodyOverTime_Magnitude-mean() :** the *estimated means* of calculating *magnitude* of X-Y-Z angular velocity signals *regarding body* using a Euclidian norm

**FreqLinAccBodyOverTime-mean()-X  
FreqLinAccBodyOverTime-mean()-Y  
FreqLinAccBodyOverTime-mean()-Z :** the *estimated means* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.

**FreqLinAccBodyOverTime_JerkSignal-mean()-X  
FreqLinAccBodyOverTime_JerkSignal-mean()-Y  
FreqLinAccBodyOverTime_JerkSignal-mean()-Z :** the *estimated mean* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signal* derived from the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.


**FreqLinAccBodyOverTime_JerkSignal-meanFreq()-X  
FreqLinAccBodyOverTime_JerkSignal-meanFreq()-Y  
FreqLinAccBodyOverTime_JerkSignal-meanFreq()-Z :** the *estimated mean frequency* i.e. the weighted average of frequency components of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signal* derived from 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.

**FreqAngVelBodyOverTime-mean()-X  
FreqAngVelBodyOverTime-mean()-Y  
FreqAngVelBodyOverTime-mean()-Z :** the *estimated means* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.

**FreqLinAccBodyOverTime-meanFreq()-X  
FreqLinAccBodyOverTime-meanFreq()-Y  
FreqLinAccBodyOverTime-meanFreq()-Z :** the *estimated mean frequency* i.e. the weighted average of frequency components of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.

**FreqAngVelBodyOverTime-meanFreq()-X  
FreqAngVelBodyOverTime-meanFreq()-Y  
FreqAngVelBodyOverTime-meanFreq()-Z :** the *estimated mean frequency* i.e. the weighted average of frequency components of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the X-Y-Z 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.

**FreqLinAccBodyOverTime_Magnitude-mean() :** the *estimated means* of the *magnitude* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.  

**FreqLinAccBodyOverTime_Magnitude-meanFreq() :** the *estimated mean frequency* i.e. the weighted average of frequency components of the *magnitude* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.  

**BodyFreqLinAccBodyOverTime_JerkSignal_Magnitude-mean() :** the *estimated means* of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signals* derived from 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.  
**BodyFreqLinAccBodyOverTime_JerkSignal_Magnitude-meanFreq() :** the *estimated mean frequency* i.e. weighted average of frequency components of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signals* derived from 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.

**BodyFreqAngVelBodyOverTime_Magnitude-mean() :** the *estimated means* of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.  

**BodyFreqAngVelBodyOverTime_Magnitude-meanFreq() :** the *estimated mean frequency* i.e. the weighted average of the frequency components of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.  

**BodyFreqAngVelBodyOverTime_JerkSignal_Magnitude-mean() :** the *estimated means* of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signals* derived from 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.  

**BodyFreqAngVelBodyOverTime_JerkSignal_Magnitude-meanFreq() :** the *estimated mean frequency* i.e. the weighted average of the frequency components of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signals* derived from 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.

### standard deviation value estimated attributes:

**LinAccBodyOverTime-std()-X  
LinAccBodyOverTime-std()-Y  
LinAccBodyOverTime-std()-Z :** the *estimated standard deviations* of the X-Y-Z directions regarding the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. The acceleration signal was then separated into a *body acceleration signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz.  

**LinAccGravityOverTime-std()-X  
LinAccGravityOverTime-std()-Y  
LinAccGravityOverTime-std()-Z :** the *estimated standard deviations* of the X-Y-Z directions regarding the 3 axial raw signals describing *linear acceleration of gravity with respect to time* captured at a contant rate of 50 Hz. First, The signals were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Then, the acceleration signal was then separated into a *gravity acceleration signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz.  

 **LinAccBodyOverTime_JerkSignal-std()-X  
LinAccBodyOverTime_JerkSignal-std()-Y  
LinAccBodyOverTime_JerkSignal-std()-Z :** the *estimated standard deviations* of the X-Y-Z directions regarding the *jerk signals* derived from the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz. First the signals were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Then, the acceleration signal was then separated into a *body acceleration signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz.  

**AngVelBodyOverTime-std()-X  
AngVelBodyOverTime-std()-Y  
AngVelBodyOverTime-std()-Z :** the *estimated standard deviations* of the X-Y-Z directions regarding the 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz. First the signals were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Then, the velocity signal was then separated into a *body velocity signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz.  

 **AngVelBodyOverTime_JerkSignal-std()-X  
AngVelBodyOverTime_JerkSignal-std()-Y  
AngVelBodyOverTime_JerkSignal-std()-Z :** the *estimated standard deviations* of the X-Y-Z directions regarding the *jerk signals* derived from 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz. First the signals were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Then, the velocity signal was then separated into a *body velocity signal* using another low pass Butterworth filter with a corner frequency of 0.3 Hz.  

**LinAccBodyOverTime_Magnitude-std() :** the *estimated standard deviations* of calculating *magnitude* of X-Y-Z linear acceleration signals *regarding a body* using a Euclidian norm  

**LinAccGravityOverTime_Magnitude-std() :** the *estimated standard deviations* of calculating *magnitude* of X-Y-Z linear acceleration signals *regarding gravity* using a Euclidian norm  

 **LinAccBodyOverTime_JerkSignal_Magnitude-std() :** the *estimated standard deviations* of calculating *magnitude* of *jerk signals* derived from X-Y-Z linear acceleration signals *regarding a body* using a Euclidian norm  
 
 **AngVelBodyOverTime_Magnitude-std() :** the *estimated standard deviations* of calculating *magnitude* of X-Y-Z angular velocity signals *regarding body* using a Euclidian norm  
 
 **AngVelBodyOverTime_JerkSignal_Magnitude-std() :** the *estimated standard deviations* of calculating *magnitude* of *jerk signals* derived from X-Y-Z angular velocity signals *regarding a body* using a Euclidian norm  
 
 **FreqLinAccBodyOverTime-std()-X  
FreqLinAccBodyOverTime-std()-Y  
FreqLinAccBodyOverTime-std()-Z :** the *estimated standard deviations* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.  

**FreqLinAccBodyOverTime_JerkSignal-std()-X  
FreqLinAccBodyOverTime_JerkSignal-std()-Y  
FreqLinAccBodyOverTime_JerkSignal-std()-Z :** the *estimated standard deviation* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signal* derived from the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.  

 **FreqAngVelBodyOverTime-std()-X  
FreqAngVelBodyOverTime-std()-Y  
FreqAngVelBodyOverTime-std()-Z :** the *estimated standard deviations* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.  

**FreqLinAccBodyOverTime_Magnitude-std() :** the *estimated standard deviations* of the *magnitude* of a *frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.  

 **BodyFreqLinAccBodyOverTime_JerkSignal_Magnitude-std() :** the *estimated standard deviations* of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signals* derived from 3 axial raw signals describing *linear acceleration of a body with respect to time* captured at a contant rate of 50 Hz.  
 
 **BodyFreqAngVelBodyOverTime_Magnitude-std() :** the *estimated standard deviations* of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a contant rate of 50 Hz.   
 
 **BodyFreqAngVelBodyOverTime_JerkSignal_Magnitude-std() :** the *estimated standard deviations* of the *magnitude* of a *body frequency domain* obtained from a Fast Fourier Transform (FFT) applied to the *jerk signals* derived from 3 axial raw signals describing *angular velocity of a body with respect to time* captured at a constant rate of 50 Hz.
