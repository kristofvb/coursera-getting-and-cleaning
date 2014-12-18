library(data.table)
library(plyr)
library(dplyr)

select_mean_std_columns <- function(df) {
        as.data.frame(df)[, grepl("subject", names(df)) | grepl("activity", names(df)) | grepl("mean", names(df)) | grepl("std", names(df))]
}

run_analysis <- function () {
        # read features
        features <- read.table("data/features.txt")
        feature_names <- as.character(features$V2)
        
        # read activity vector for test data
        test_activity <- read.table("data/test/y_test.txt")
        
        # read test subjects
        test_subjects <- read.table("data/test/subject_test.txt")
                
        # bind activity as first column to test data
        df_test <- cbind(test_subjects, test_activity, read.table("data/test/X_test.txt"))
        setnames(df_test, c("subject", "activity", feature_names))

        # select mean and std columns
        df_test <- select_mean_std_columns(df_test)
        
        # read activity vector for training data        
        train_activity <- read.table("data/train/y_train.txt")
        
        # read train subjects
        train_subjects <- read.table("data/train/subject_train.txt")

        # bind activity as first column to training data        
        df_train <- cbind(train_subjects, train_activity, read.table("data/train/X_train.txt"))
        setnames(df_train, c("subject", "activity", feature_names))

        # select mean and std columns
        df_train <- select_mean_std_columns(df_train)
        
        # combine test and training data
        df <- rbindlist(list(df_test, df_train))
        
        # read activity labels
        activity_labels <- read.table("data/activity_labels.txt")
        
        # replace activity indexes with activity labels        
        df$activity <- factor(df$activity, levels = activity_labels$V1, labels = activity_labels$V2)        
        
        # group by subject and activity
        df <- ddply(df, .(subject,activity), numcolwise(ave))
        
        # get rid of duplicate rows
        distinct_(df)
}