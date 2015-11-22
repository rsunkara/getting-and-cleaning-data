require(dplyr)
require(readr)
require(reshape2)

# Setup the data path to read files from the UCI HAR dataset
data_path = "/UCI HAR Dataset/"
test_dir_path ="test"
train_dir_path = "train"
test_fname = "X_test.txt"
train_fname = "X_train.txt"
test_act_fname = "y_test.txt"
train_act_fname = "y_train.txt"
test_sub_fname = "subject_test.txt"
train_sub_fname = "subject_train.txt"

#Save the current working directory
old_wd = getwd()
setwd(paste0(old_wd,data_path))

#Read the feature names to the row names, will rename the rows 
# One more time
feature_names <- read.csv("features.txt",sep=" ",header=FALSE)
setwd(paste0(old_wd,data_path,test_dir_path))

#read the test data and set colnames
test_df <- read_fwf(test_fname, fwf_widths(c(rep(16,561))))
test_activity <- read_fwf(test_act_fname, fwf_widths(c(1)))
test_subject <- read.table(test_sub_fname)
colnames(test_df) = feature_names[,2]

#Read teh training data and set col names
setwd(paste0(old_wd,data_path,train_dir_path))
train_df <- read_fwf(train_fname, fwf_widths(c(rep(16,561))))
train_activity <- read_fwf(train_act_fname, fwf_widths(c(1)))
train_subject <- read.table(train_sub_fname)
colnames(train_df) = feature_names[,2]

#Rowbind to merge both the datframes
final_df <- rbind(train_df, test_df)

#There are repeats in colnames, remove duplicates
final_df <- final_df[,unique(colnames(final_df))]

#update final_df only to include the mean() and standard deviation std()
final_df <- final_df[,grepl(".std\\(\\).",colnames(final_df)) | grepl(".mean\\(\\).",colnames(final_df))]
#copy the colnames to beautify
pre_col_names <- colnames(final_df)

#clean up the column names 
pre_col_names = gsub("*mean\\(\\)-X","Mean_X-axis",pre_col_names)
pre_col_names = gsub("*mean\\(\\)-Y","Mean_Y-axis",pre_col_names)
pre_col_names = gsub("*mean\\(\\)-Z","Mean_Z-axis",pre_col_names)
pre_col_names = gsub("*std\\(\\)-Z","Stdev_Z-axis",pre_col_names)
pre_col_names = gsub("*std\\(\\)-Y","Stdev_Y-axis",pre_col_names)
pre_col_names = gsub("*std\\(\\)-X","Stdev_X-axis",pre_col_names)
pre_col_names = gsub("^tBody","timeseriesBody",pre_col_names)
pre_col_names = gsub("^tGravity","timeseriesGravity",pre_col_names)
pre_col_names = gsub("^fBody","fourierseriesBody",pre_col_names)
colnames(final_df) = pre_col_names

# merge training and test activities and subjects
activities = rbind(test_activity, train_activity)
subjects   = rbind(test_subject, train_subject)


colnames(activities) = "activity_type"
colnames(subjects) = "subject_id"
#setup descriptive activity names
activities$activity_type  = factor(activities$activity_type ,labels=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

subjects$subject_id = as.factor(subjects$subject_id)

#add the activity type and subjects to the dataframe with means and stdev
final_df = cbind(activities,subjects, final_df)

# Create the tidy data set with mean per activitype per variable
tidy_data = melt(final_df, id=c("activity_type","subject_id"))
tidy_data = dcast(tidy_data, subject_id + activity_type ~variable, mean)

#write the data to file
setwd(paste0(old_wd,data_path))
write.csv(tidy_data, "tidy_data.csv")

