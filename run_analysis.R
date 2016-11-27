# Step 1 installing the libraries and reading the datasets
library(data.table)
library(dplyr)

#Read Supporting Metadata
featureNames <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/features.txt")
activityLabels <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/activity_labels.txt")

#Read training data
subjectTrain  <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/train/subject_train.txt")
activityTrain <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/train/y_train.txt")
featuresTrain  <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/train/X_train.txt")

#Read test data
subjectTest  <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/test/subject_test.txt")
activityTest <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/test/y_test.txt")
featuresTest  <- read.table("C:/users/Aashish/Desktop/Coursera DS/DataCleaning/week4/UCI HAR Dataset/test/X_test.txt")

#Merge the datasets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)


# Step 2 - Merging the datasets

# Naming the columns
colnames(features) <- t(featureNames[2])

#Merger the data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


#Setp 3 - Extracting only the measurements on mean and standard deviation
#Extract the column indices that have either mean or std in them.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)


#Add activity and subject columns to the list and look at the dimension of completeData
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

extractedData <- completeData[,requiredColumns]


# Step 4 Naming the activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)


# Replacing with the appropriate names 

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))



# Step 5  - creating the tidy dataset

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)








