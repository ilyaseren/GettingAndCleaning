# Step 1
# Merge the training and test sets to create one data set
#########################################################

x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

# Combine features data 'X' (test and train) and set names to variables
x_data <- rbind(x_train, x_test)

dataFeaturesNames <- read.table(("features.txt"),head=FALSE)
names(x_data)<- dataFeaturesNames$V2

# Combine activity data 'Y' (test and train) and set names to variables
y_data <- rbind(y_train, y_test)

names(y_data)<- c("activity")

# Combine subject data (test and train) and set names to variables
subject_data <- rbind(subject_train, subject_test)

names(subject_data)<-c("subject")

# Merge to 1 dataset
datamerge <- cbind(subject_data, y_data)
data <- cbind(x_data, datamerge)

# Step 2
# Extract only the measurements on the mean and standard deviation for each measurement
#######################################################################################

# get only columns with mean() or std()
subdata <- dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

#subset the columns
selectedNames <- c(as.character(subdata), "subject", "activity" )
data <- subset(data,select=selectedNames)

# Step 3
# Use descriptive activity names to name the activities in the data set
#######################################################################

#read acitivity names from "acitivity_labels.txt"
activitylabel <- read.table(("activity_labels.txt"),header = FALSE)

#factorize variable activity in data frame data
data$activity <- factor(data$activity, levels = activitylabel[,1], labels = activitylabel[,2])

# Step 4
# Appropriately label the data set with descriptive variable names
##################################################################
names(data) <- gsub("^t", "time", names(data))
names(data) <- gsub("^f", "frequency", names(data))
names(data) <- gsub("Acc", "Accelerometer", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("BodyBody", "Body", names(data))

# Step 5
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
###############################################################################################################################################
library(plyr)
data2 <- aggregate(. ~subject + activity, data, mean)
data2 <- data2[order(data2$subject,data2$activity),]
write.table(data2, file = "tidydata.txt",row.name=FALSE)
