## Set Working Directory to where the data is stored
setwd("~/Documents/Coursera/Getting.Data/UCI HAR Dataset")

## Read in the data
train.x<-read.table("train/X_train.txt")
test.x<-read.table("test/X_test.txt")
train.y<-read.table("train/y_train.txt")
test.y<-read.table("test/y_test.txt")
Subject_train<-read.table("train/subject_train.txt")
Subject_test<-read.table("test/subject_test.txt")
activity_lables<-read.table("activity_labels.txt")
features<-read.table("features.txt")

## Merge the training and the test sets to create one data set
all.x<-rbind(train.x,test.x)
all.y<-rbind(train.y,test.y)

## Extracts only the variables on the mean and standard deviation
colnames(all.x) <- c(as.character(features[,2]))
Mean<-grep("mean()",colnames(all.x),fixed=TRUE)
SD<-grep("std()",colnames(all.x),fixed=TRUE)
MeanSD<-all.x[,c(Mean,SD)]

## Bind the data with the y-data
all.activity<-cbind(all.y,MeanSD)
colnames(all.activity)[1] <- "Activity"

## Appropriately label the data set with descriptive activity names
activity_lables[,2]<-as.character(activity_lables[,2])

for(i in 1:length(all.activity[,1]))
{all.activity[i,1]<-activity_lables[all.activity[i,1],2]}

## Creates a second tidy data set with the meand of each variable for each activity and subject.
Subject_all<-rbind(Subject_train,Subject_test)
all<-cbind(Subject_all,all.activity)
colnames(all)[1] <- "Subject"

## Use the aggregate function to find the means
Tidy <- aggregate( all[,3] ~ Subject+Activity, data = all, FUN= "mean" )

for(i in 4:ncol(all))
{Tidy[,i] <- aggregate( all[,i] ~ Subject+Activity, data = all, FUN= "mean" )[,3]}

colnames(Tidy)[3:ncol(Tidy)] <- colnames(MeanSD)

## Write the new tidy data set to the text file
write.table(Tidy, file = "Tidy.txt")


