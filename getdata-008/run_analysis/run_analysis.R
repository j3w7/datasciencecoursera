##
# -1. Install and require package dependencies

# Require a package. Install package if not yet installed.
# (Adapted from BondedDust's answer on http://stackoverflow.com/a/19843463/2392982)
insist <- function(pkg){
         if ( !require(pkg, character.only=TRUE) ) {
              install.packages(as.character(pkg), dependencies=TRUE)
              require(pkg, character.only=TRUE) }}

# require (and maybe install) depencendy
insist ("reshape2")
insist ("plyr")

##
# 0. Download dataset and read relevant data

# location for local temporary file
dataZipFile <- "/tmp/FUCI_HAR_Dataset.zip"
# download data iff not done yet
if(!file.exists(dataZipFile)) {
  download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", dataZipFile)
}

# Unzip and read training data.

subject_train <- read.table(unz(dataZipFile, 'UCI HAR Dataset/train/subject_train.txt'))
x_train       <- read.table(unz(dataZipFile, 'UCI HAR Dataset/train/X_train.txt'))
y_train       <- read.table(unz(dataZipFile, 'UCI HAR Dataset/train/y_train.txt'))

# Unzip and read test data.

subject_test  <- read.table(unz(dataZipFile, 'UCI HAR Dataset/test/subject_test.txt'))
x_test        <- read.table(unz(dataZipFile, 'UCI HAR Dataset/test/X_test.txt'))
y_test        <- read.table(unz(dataZipFile, 'UCI HAR Dataset/test/y_test.txt'))

# Unzip and read features
features       <- read.table(unz(dataZipFile, "UCI HAR Dataset/features.txt"), header=F, colClasses="character")

# Unzip and read activities
activities <- read.table(unz(dataZipFile, "UCI HAR Dataset/activity_labels.txt"), header=F, colClasses="character")

##
# 1.  Merge the training and the test sets to create one data set.

subject_data <- rbind(subject_train, subject_test)
x_data       <- rbind(x_train, x_test)
y_data       <- rbind(y_train, y_test)

data <- list(x=x_data, y=y_data, subject=subject_data)

##
# 2. Extract the measurements on the mean and standard deviation for each measurement.

# identify feature entries for mean and standard deviation
is_mean_or_std     <- grepl("mean|std", features[,2])
extracted_features <- features[is_mean_or_std,2]

# extract features from dataset x
extracted_data           <- data$x[, is_mean_or_std]
colnames(extracted_data) <- extracted_features

# reduce data for x
data$x <- extracted_data

##
# 3. Use descriptive activity namesm and appropriate labels with descriptive variable names.

colnames(data$y) <- c("Activity")
data$y$Activity[data$y$Activity == 1] = "WALKING"
data$y$Activity[data$y$Activity == 2] = "WALKING_UPSTAIRS"
data$y$Activity[data$y$Activity == 3] = "WALKING_DOWNSTAIRS"
data$y$Activity[data$y$Activity == 4] = "SITTING"
data$y$Activity[data$y$Activity == 5] = "STANDING"
data$y$Activity[data$y$Activity == 6] = "LAYING"

colnames(data$subject) <- c("Subject")

##
# 4. Create the tidy data set with averages.

df <- cbind(data$x, data$y, data$subject)

tidy_data <- ddply(df, .(Subject, Activity), function(x) colMeans(x[,1:60]))

write.csv(tidy_data, "tidy_data.txt", row.names=FALSE)
