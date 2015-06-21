################################################################################
#         Data Science Specialization - Getting and Cleaning Data              #
#                            Course Project                                    #
################################################################################

# This script loads data from the internet, merges test and training files     #
# together with subject IDs, indexes activity type, labels the variables, and  #
# then selects only those variables that reflect means and SDs to create the   #
# dataset, data.txt. It then aggregates (mean) the variables across subject and#
# activity type to create agg_data.txt                                         #        

################################################################################
#                                Load Packages                                 #
################################################################################
library(downloader)
library(dplyr)

################################################################################
#                               Get Raw Data                                   #
################################################################################

# download zipped file + unzip

zipUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download(zipUrl,dest="dataset.zip", mode="wb")
unzip("dataset.zip", exdir="./")
setwd("~/UCI HAR Dataset")

# load label files

features<- read.table("features.txt")
head(features)

activity<-read.table("activity_labels.txt")
head(activity)

# load test files

setwd("~/UCI HAR Dataset/test")

sub_test<-read.table("subject_test.txt")
X_test<-read.table("X_test.txt")
Y_test<-read.table("Y_test.txt")

# load training files

setwd("~/UCI HAR Dataset/train")

sub_train<-read.table("subject_train.txt")
X_train<-read.table("X_train.txt")
Y_train<-read.table("Y_train.txt")

setwd("~/UCI HAR Dataset") # reset to save files to main directory
################################################################################
#                               Merge Files                                    #
################################################################################

# one row contains data (X) for one person (sub) for one activity (Y))
test<-cbind(sub_test,Y_test,X_test)
train<-cbind(sub_train,Y_train,X_train)

# because files contain the same variables (in the same order), use rbind
dim(test);dim(train)
data<-rbind(test,train)
dim(data)

# double check that the number of observations are as expected
2947+7352


################################################################################
#                              Name Variables                                  #
################################################################################

# get the variable names from the features table,
# convert to character so they can be used as names,
# and transpose the column into a row vector
feat<-t(as.character(features$V2))
str(feat)

# name the variables
colnames(data)<-c("subID","activity",feat)
head(data)
names(data)

################################################################################
#                       Select Mean and SD Variables                           #
################################################################################

# determine which variables contain the mean and sd
means<-grep("mean()",names(data),value=T)
stds<-grep("std()",names(data),value=T)
stds
means

# select only the mean and sd variables
data<-subset(data,select=c("subID","activity",means,stds))

################################################################################
#                    Create Descriptive Activity Variable                      #
################################################################################

# activity names come from the activity_labels doc

data$activityName<-as.factor(ifelse(data$activity==1,"walking",
                          ifelse(data$activity==2, "walking upstairs",
                                 ifelse(data$activity==3,"walking downstairs",
                                        ifelse(data$activity==4,"sitting",
                                               ifelse(data$activity==5, "standing",
                                                      ifelse(data$activity==6,
                                                             "laying","NA")))))))

# verify that it did what it should
str(data$activityName)
table(data$activityName)


################################################################################
#                           Write Dataset to Text File                         #
################################################################################
setwd("~/UCI HAR Dataset") # reset to save files to main directory
write.table(data,"data.txt", row.name=F)


################################################################################
#                          Generate Aggregated Dataset                         #
################################################################################

# compute mean for all variables across subject and activity
data_agg<-aggregate(data,list(subID=data$subID,activityName=data$activityName),mean)

# remove duplicate columns
data_agg<-data_agg[,!duplicated(colnames(data_agg))]


################################################################################
#                  Write Aggregated Dataset to Text File                       #
################################################################################

write.table(data_agg,"data_agg.txt", row.name=F)

