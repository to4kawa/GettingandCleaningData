### run_analysis.R

library(tidyverse)

### Premise:Unzip the file in the current directory
### read files
list_files <- list.files(,full.names=T,recursive=T)

### get file path
filepath <- list_files[grep("UCI",list_files)]

### test & train data read and create data_df
for (i in grep("X_",filepath,value=T)){
  assign(str_extract(i,"[^/]+(?=.txt)"),read.table(i,check.names=F))
}

data_df <- rbind(X_test,X_train)

### test & train label data read and create label_df
for (i in grep("/y_",filepath,value=T)){
  assign(str_extract(i,"[^/]+(?=.txt)"),read.table(i,check.names=F))
}

label_df <- rbind(y_test,y_train)

### test & train subject data read and create subject_df
for (i in grep("/subject_",filepath,value=T)){
  assign(str_extract(i,"[^/]+(?=.txt)"),read.table(i,check.names=F))
}

subject_df <- rbind(subject_test,subject_train)

### activity_labels data read and create activity_labels
for (i in grep("/activity_",filepath,value=T)){
  assign(str_extract(i,"[^/]+(?=.txt)"),read.table(i,check.names=F))
}

### features data read and create features_list
for (i in grep("/features.txt",filepath,value=T)){
  assign(str_extract(i,"[^/]+(?=.txt)"),read.table(i,check.names=F))
}

col_nums <- grep("mean\\(\\)-|std\\(\\)-",features[,2])
col_names <- gsub("[()]","",grep("mean\\(\\)-|std\\(\\)-",features[,2],value=T))
col_names <-  gsub("^f","frequency",col_names)
col_names <-  gsub("^t","time",col_names)

features_list <- list("col_numbers" = col_nums, "col_names"= col_names)

### Extracts only the measurements on the mean and standard deviation for each measurement.
std_mean_data_df <- data_df %>% select(features_list$col_numbers)
names(std_mean_data_df) <- features_list$col_names

### Uses descriptive activity names to name the activities in the data set
activity_labels_df <- inner_join(label_df,activity_labels,by="V1")
names(activity_labels_df) <- c("activity_labels","activity")

### Appropriately labels the data set with descriptive variable names.
names(subject_df) <-c("subject")
test_train_data_df <- cbind(subject_df, activity_labels_df, std_mean_data_df )

### creates a second, independent tidy data set with the average of each variable for each activity and each subject.
average_activity_subject_df <- test_train_data_df %>% 
  group_by(activity,subject) %>% 
  summarize_all(mean)
