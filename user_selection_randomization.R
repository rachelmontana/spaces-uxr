#data <- read.csv(file="/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/Participants/NYC_Users_List.csv", header=TRUE)
android_data <- read.csv(file="/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/Contacting Users/Android_User_Emails.csv", header=TRUE)
android_existing_list <- read.csv(file="/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/Contacting Users/Android_User_Emails_Sample3.csv", header=TRUE)
ios_existing_list <- read.csv(file="/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/Contacting Users/iOS_User_Emails_Sample.csv", header=TRUE)
#ios.sub.nonengineers <- subset(ios_data, ios(data)in_engineering = FALSE)
#ios.sub.engineers <- subset(ios_data, in_engineering == "TRUE")

#user_list <- data[sample(nrow(data), 30), ]
#write.csv(user_list, "/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/NYC_User_Emails_Sample.csv")


#ios_user_list <- data[sample(nrow(data), 15), ]
#write.csv(ios_user_list, "/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/iOS_User_Emails_Sample2.csv")

android_user_list <- android_data[sample(nrow(android_data), 4000), ]
android_user_list_final <- android_user_list[!(android_user_list$Email %in% android_existing_list$Email),]
android_user_list_final <- android_user_list_final[!(android_user_list_final$Email %in% ios_existing_list$Email),]
write.csv(android_user_list_final, "/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/Contacting Users/Android_User_Emails_Sample4.csv")
