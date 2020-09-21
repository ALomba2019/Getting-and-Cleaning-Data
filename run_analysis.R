###########Getting & Cleaning Data Course Project ###########
#############################################################

#AIM: Collect, work with and clean the UCI dataset, according
#to the tasks defined in the Assignment

#setting working directory

setwd('C:/Users/AngelaLomba/Desktop/Getting&CleaningData')

#creating the 'data' folder (if does not exists)
if(!file.exists("./data")){dir.create("./data")}

#download the dataset

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl,destfile="./data/dataset.zip")

#unzipping the dataset
unzip(zipfile="./data/dataset.zip",exdir="./data")

#Writing the R script called run_analysis.R (Task 1 to 5)

#T1.Merges the training and the test data sets to create one 
#data set.

#read training data
xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subjecttrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#read test data
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subjecttest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#readfeature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

#reading activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

#Assigning column names:

colnames(xtrain) <- features[,2]
colnames(ytrain) <-"activityId"
colnames(subjecttrain) <- "subjectId"

colnames(xtest) <- features[,2] 
colnames(ytest) <- "activityId"
colnames(subjecttest) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

#Merging training and test data in one data set

train <- cbind(ytrain, subjecttrain, xtrain)
test <- cbind(ytest, subjecttest, xtest)
fulldata <- rbind(train, test)

#dim(fulldata)
#[1] 10299   563

#T2.Extracting only measurements on the mean and standard deviation for each measurement.

#Reading column names:

colNames <- colnames(fulldata)

#Creating a vector for defining ID, mean and standard deviation:

MSD <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

#Subset the fulldata

fullMSD <- fulldata[ , MSD == TRUE]

dim(fullMSD)


#T3. Using descriptive activity names to name the activities in the data set

fulldataAN <- merge(fullMSD, activityLabels,
                              by='activityId',
                              all.x=TRUE)

#T4. Labeling the data set with descriptive variable names.

#get column names

fulldataANcol<- colnames(fulldataAN)

#remove special characters

fulldataANcol<-gsub("[\\(\\)-]", "", fulldataANcol)


#expand abbreviations and clean names of columns

fulldataANcol <- gsub("^f", "frequencyDomain", fulldataANcol)
fulldataANcol <- gsub("^t", "timeDomain", fulldataANcol)
fulldataANcol <- gsub("Acc", "Accelerometer", fulldataANcol)
fulldataANcol <- gsub("Gyro", "Gyroscope", fulldataANcol)
fulldataANcol <- gsub("Mag", "Magnitude", fulldataANcol)
fulldataANcol <- gsub("Freq", "Frequency", fulldataANcol)
fulldataANcol <- gsub("mean", "Mean", fulldataANcol)

# correct typo
fulldataANcol <- gsub("BodyBody", "Body", fulldataANcol)

# use new labels as column names
colnames(fulldataAN) <- fulldataANcol

#T5. From T4, creating a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

library (dplyr)

fulldataANMean<- fulldataAN %>%
  group_by(subjectId, activityId) %>%
  summarise_each(funs(mean))

write.table(fulldataANMean, "tidydata.txt", row.names = FALSE, 
            quote = FALSE)
