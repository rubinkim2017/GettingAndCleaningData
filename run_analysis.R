##Getting and Cleaning Data Course Project

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 

# Organize memory space and set the working directory
rm(list=ls())
setwd("C:/Users/user/Desktop/HAR Dataset/UCI HAR Dataset/")

# Load data.table,dplyr and plyr package
library(data.table)
library(dplyr)
library(plyr)

# Import activity_labels,features
activity<-read.table("activity_labels.txt",header=F)
features<-read.table("features.txt",header=F)

# Import train data set,y_train and subject_train
trainData<-read.table("train/X_train.txt",header=F)
trainLabel<-read.table("train/y_train.txt",header=F)
trainSubject<-read.table("train/subject_train.txt",header=F)

# Import test data set,y_test and subject_test
testData<-read.table("test/X_test.txt",header=F)
testLabel<-read.table("test/y_test.txt",header=F)
testSubject<-read.table("test/subject_test.txt",header=F)

# Add activity labels to trainLabel data frame and testLabel data frame
names(trainLabel)<-"activityLabel"
activityName<-character(7352)
trainLabel<-cbind(trainLabel,activityName)
trainLabel$activityName<-as.character(trainLabel$activityName)
for(i in seq_along(activity[,2])){
  trainLabel[trainLabel$activityLabel== i,2]<-as.character(activity[i,2])
}

names(testLabel)<-"activityLabel"
activityName<-character(2947)
testLabel<-cbind(testLabel,activityName)
testLabel$activityName<-as.character(testLabel$activityName)
for(i in seq_along(activity[,2])){
  testLabel[testLabel$activityLabel== i,2]<-as.character(activity[i,2])
}

# Change the column names of trainData and testData with features
names(trainData)<-features[,2]
names(testData)<-features[,2]

# Change the column name of trainSubject and testSubject data frame
names(trainSubject)<-"subject"
names(testSubject)<-"subject"

# Merge trainSubject,trainLabel and trainData into the data frame which 
# is named as humanBodyData
trainData<-cbind(trainSubject,trainLabel,trainData)
testData<-cbind(testSubject,testLabel,testData)
humanBodyData<-rbind(trainData,testData)
humanBodyData<-humanBodyData[order(humanBodyData$subject,humanBodyData$activityLabel),]

# Extract only the measurements on the mean and standard deviation for each measurement.
# Name that data frame as meanStdData
meancols<-grep("[Mm]ean",names(humanBodyData))
stdcols<-grep("[Ss]td",names(humanBodyData))
cols<-c(meancols,stdcols)
cols<-sort(cols)

meanStdData<-humanBodyData[,c(1,2,3,cols)]
meanStdData

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

meanStdData$subject<-as.factor(meanStdData$subject)
meanStdData$activityLabel<-as.factor(meanStdData$activityLabel)
subjectMeanStdData<-split(meanStdData,list(meanStdData$subject,meanStdData$activityLabel))

subjectActivityAverage<-ldply(subjectMeanStdData,function(x) colMeans(x[,-(1:3)],na.rm=TRUE))
names(subjectActivityAverage)[1]<-"ID"
subjectActivityAverage$ID<-as.numeric(subjectActivityAverage$ID)
subjectActivityAverage<-subjectActivityAverage[order(subjectActivityAverage$ID),]

subjectLabel<-data.frame(Subject=rep(1:30,each=6),Activity=rep(c("walking","walking_upstairs",
                                                                 "walking_downstairs","sitting","standing","laying"),30))
subjectActivityAverage<-cbind(subjectLabel,subjectActivityAverage)
subjectActivityAverage<-subjectActivityAverage[,c(3,1,2,4:ncol(subjectActivityAverage))]
subjectActivityAverage

write.table(subjectActivityAverage,"subjectActivityAverage.txt",row.names = FALSE)
