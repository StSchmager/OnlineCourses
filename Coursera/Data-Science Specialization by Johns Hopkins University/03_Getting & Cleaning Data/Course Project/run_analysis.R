## 0. Reads the training and test data sets

# training
setwd("C:/Users/Stefan/Google Drive/Coursera/03_GettingData/UCI HAR Dataset/train")
train_subject <- read.table("subject_train.txt", col.names = "subject", colClasses = "factor")
train_y <- read.table("y_train.txt", col.names = "activity", colClasses = "factor")
train_x <- read.table("X_train.txt")
train <- cbind(set = "train", train_subject, train_y, train_x)

# test
setwd("C:/Users/Stefan/Google Drive/Coursera/03_GettingData/UCI HAR Dataset/test")
test_subject <- read.table("subject_test.txt", col.names = "subject", colClasses = "factor")
test_y <- read.table("y_test.txt", col.names = "activity", colClasses = "factor")
test_x <- read.table("X_test.txt")
test <- cbind(set = "test", test_subject, test_y, test_x)

## 1. Merges the training and the test sets to create one data set.

dat1 <- rbind(test, train)
remove(train_subject, train_y, train_x, test_subject, test_y, test_x, test, train)

# 2. Uses descriptive activity names to name the activities in the data set
setwd("C:/Users/Stefan/Google Drive/Coursera/03_GettingData/UCI HAR Dataset")
activity_labels <- read.table("activity_labels.txt")
dat1$activity <- factor(dat1$activity,
                          levels = as.character(activity_labels$V1),
                          labels = activity_labels$V2)
remove(activity_labels)

# 3. Appropriately labels the data set with descriptive variable names.
var_labels <- read.table("features.txt")
colnames(dat1)[4:564] <- as.character(var_labels$V2)
remove(var_labels)

# 4. Extracts only the measurements on the mean and standard deviation for each measurement.
dat1 <- dat1[,c(1, 2, 3, grep("mean()", colnames(dat1)), grep("std()", colnames(dat1)))]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
dat2 <- summarise_each(group_by(dat1, subject, activity), funs(mean))
names(dat2)[4:82] <- paste0("AvgOf_", names(dat2)[4:82])
dat2$set <- factor(dat2$set, levels = c(1, 2), labels = c("Test", "Train"))

setwd("C:/Users/Stefan/Google Drive/Coursera/03_GettingData")
write.table(dat2, "CourseProject_DataSet.txt",row.name=FALSE)