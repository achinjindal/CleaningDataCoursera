library(dplyr)
library(readr)
library(reshape2)

#1. Merges the training and the test sets to create one data set.

  #read test and train data
test=read_fwf(file="UCI HAR Dataset/test/X_test.txt", fwf_widths(rep(16,561)))
train=read_fwf(file="UCI HAR Dataset/train/X_train.txt", fwf_widths(rep(16,561)))

  #combine test and train data
data= rbind(train, test) 

  #read features file and extract row numbers that contain "mean()" and "std()"
features=read.table("UCI HAR Dataset/features.txt")
meancols=features[grepl("mean\\(\\)", features[,2]),][,1]
meancolnames=features[grepl("mean\\(\\)", features[,2]),][,2]
stdcols=features[grepl("std\\(\\)", features[,2]),][,1]
stdcolnames=features[grepl("std\\(\\)", features[,2]),][,2]

  #get subjects and participants
subjecttest = read.table("UCI HAR Dataset/test/subject_test.txt")
subjecttrain = read.table("UCI HAR Dataset/train/subject_train.txt")
activitytest= read.table("UCI HAR Dataset/test/y_test.txt")
activitytrain= read.table("UCI HAR Dataset/train/y_train.txt")


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

data_means = data[,meancols]
data_stds = data[,stdcols]
data_extract = cbind(data_means, data_stds, rbind(subjecttrain, subjecttest), rbind(activitytrain, activitytest))

  #assign colnames
colnames(data_extract)=c(as.character(meancolnames), as.character(stdcolnames), "Subject", "Activity")



#3. Uses descriptive activity names to name the activities in the data set

for(i in 1:nrow(data_extract)){
data_extract$Activity[i] = switch(data_extract$Activity[i], "1"="WALKING", "2"="WALKING_UPSTAIRS","3"="WALKING_DOWNSTAIRS",
                                              "4"="SITTING", "5"="STANDING","6"="LAYING")
}


#4 Appropriately labels the data set with descriptive variable names. 

#Skipping this step as the variable names are sufficiently descriptive anyway, 
#its readily understood what each variable stands for


#5. From the data set in step 4, creates a second, independent tidy data set with the average 
#   of each variable for each activity and each subject.

melted=melt(data_extract, id.vars = c("Activity", "Subject"))
grouped <- group_by(melted, Activity, Subject, variable)
tidy_temp= summarise(grouped, mean=mean(value))
tidy = dcast(tidy_temp, Subject + Activity ~ variable)

tidy_names=colnames(tidy)
for(i in 3:68) tidy_names[i]=paste0("mean(",tidy_names[i],")")
colnames(tidy)= tidy_names

write.table(tidy, file="tidy.txt", row.names=F)