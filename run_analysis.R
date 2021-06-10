library(data.table)
library(dplyr)


## Get the dataset
if (!dir.exists("./RawData")) 
        dir.create("./RawData")

rawDataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(rawDataURL, destfile = "./RawData/rawData.zip", method = "curl")
unzip("./RawData/rawData.zip", exdir = "./RawData")

list.files("./RawData/UCI HAR Dataset/test/")
list.files("./RawData/UCI HAR Dataset/train/")


###  Merges the training and the test sets to create one data set.

## Train data
X_train <- fread("./RawData/UCI HAR Dataset/train/X_train.txt")
s_train <- fread("./RawData/UCI HAR Dataset/train/subject_train.txt")
y_train <- fread("./RawData/UCI HAR Dataset/train/y_train.txt")

## Test data
X_test <- fread("./RawData/UCI HAR Dataset/test/X_test.txt")
s_test <- fread("./RawData/UCI HAR Dataset/test/subject_test.txt")
y_test <- fread("./RawData/UCI HAR Dataset/test/y_test.txt")

dim(X_train); dim(X_test);
dim(s_train); dim(s_test);
dim(y_train); dim(y_test);


## Bind data by rows
X_data <- X_train %>% bind_rows(X_test)
s_data <- s_train %>% bind_rows(s_test)
y_data <- y_train %>% bind_rows(y_test)

dim(X_data)
dim(s_data)
dim(y_data)


## Read features names and set names to variables
features_names <- fread("./RawData/UCI HAR Dataset/features.txt", header = FALSE)
names(X_data) <- features_names$V2
names(s_data) <- c("subject")
names(y_data) <- c("activity")


## Merge columns to get one data frame
Data <- cbind(X_data, s_data, y_data)


### Extracts only the measurements on the mean and standard deviation for each measurement

## Find the variables with "mean()" or "std()"
subFeatures <- grep(pattern = "mean\\(\\)|std\\(\\)", x = features_names$V2, value = TRUE)
## Subset just the corresponding Features
Data <- Data %>% select(all_of(subFeatures), subject, activity)
## Check the structures of the data frame 
str(Data)


### Uses descriptive activity names to name the activities in the data set
levels(as.factor(Data$activity))
activity_names <- fread("./RawData/UCI HAR Dataset/activity_labels.txt")
Data$activity <- factor(Data$activity, labels = activity_names$V2)


### Appropriately labels the data set with descriptive variable names. 

#* prefix t is replaced by time
#* Acc is replaced by Accelerometer
#* Gyro is replaced by Gyroscope
#* prefix f is replaced by frequency
#* Mag is replaced by Magnitude
#* BodyBody is replaced by Body

names(Data) <- gsub("^t", replacement = "time", x = names(Data))
names(Data) <- gsub("^f", replacement = "frequency", x = names(Data))
names(Data) <- gsub("Acc", replacement = "Accelerometer", x = names(Data))
names(Data) <- gsub("Gyro", replacement = "Gyroscope", x = names(Data))
names(Data) <- gsub("Mag", replacement = "Magnitude", x = names(Data))
names(Data) <- gsub("BodyBody", replacement = "Body", x = names(Data))

## Check
names(Data)


### From the data set in step 4, creates a second, 
### independent tidy data set with the average of each variable for each activity and each subject.
dim(Data)
tidyData <- Data %>%
                group_by(activity, subject) %>%
                summarize(across(.cols = 1:66, .fns = mean))

head(tidyData)
dim(tidyData)


### Save the tidy Data
if (!dir.exists("./TidyData")) {
        dir.create("./TidyData")
}

write.table(x = tidyData, file = "./TidyData/tidyDataset.txt", row.names = FALSE, quote = FALSE)



### ----------------------------------------------------------------------------------------------

## Useful Info: https://dplyr.tidyverse.org/articles/colwise.html

## Other ways to do this project:
##https://rstudio-pubs-static.s3.amazonaws.com/37290_8e5a126a3a044b95881ae8df530da583.htm

