GettingNcleaningData_project
============================

*#This is a Project done under getting and Cleaning the data course of coursera.

*#Below is the R-script :

*#Remove the previously created objects and clean up the R working Space.:

rm(list=ls(all=TRUE)) 

# 1) Reading the text files :

*#Reading train text files:

setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train")

x_train<-read.table("X_train.txt")

*#Reading Features Text file:

setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")

feature<-read.table("features.txt")

*#Remove the first column of the feature data set and transposing the feature vector, so we are left with an array of 

*#feature names. There are 561 varaibles and they are named as 'v1','v2', and so on 'v561'.

feature<-feature[-c(1)]

featureT<-data.frame(t(feature))

row.names(featureT)=NULL

*# The below code re-names the features of train data w.r.to feature data set.

colnames(x_train)<-colnames(featureT)

x_train<-rbind(x_train,featureT)


colnames(x_train)<-x_train[7353,]

x_train<-x_train[-7353,]



*# The below code reads the vectors 'y_train.txt' and 'subject_train.txt' as 'Y_train' and 'Subjects_train' respectively

*# and adds the two vectors as columns for train data frame as 'Activity' and 'Subjects'

setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train")

Y_train<-read.table("y_train.txt")

subjects_train<-read.table("subject_train.txt")

train<-cbind(x_train,Y_train,subjects_train)

colnames(train)[562]<-"Activity"

colnames(train)[563]<-"Subjects"


*# The below code performs the same steps as we have performed to the train data.:

*# Now working on Test data :

*# Reading Test text files:

setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test")

x_test<-read.table("X_test.txt")


colnames(x_test)<-colnames(featureT)

x_test<-rbind(x_test,featureT)


colnames(x_test)<-x_test[2948,]

x_test<-x_test[-2948,]


*# Reading Test text file:

Y_test<-read.table("y_test.txt")

subjects_test<-read.table("subject_test.txt")

test<-cbind(x_test,Y_test,subjects_test)

colnames(test)[562]<-"Activity"

colnames(test)[563]<-"Subjects"

*# So, By this step our Train and Test data are read in R and Ready to use.

# 2) Merge the train and test set into one data set:

*# Now, Combining the two data sets, train and test into one independent data set namely 'samsungData'.

samsungData<-rbind(train,test)

*# Removing un-used objects :

rm(featureT,subjects_test,Y_test,x_test,x_train,Y_train,subjects_train,feature)

# 3) Working with column names of samsungData:

*# The below code re-names the column names into human readable format and creates unique column names by removing 

*#spaces,paranthesis ect:

colnames(samsungData)

for (n in 303:316) {
  colnames(samsungData)[n] <- paste(colnames(samsungData)[n],"X", sep="")
}

for (n in 317:330) {
  colnames(samsungData)[n] <- paste(colnames(samsungData)[n], "Y", sep="")
}

for (n in 331:344) {
  colnames(samsungData)[n] <- paste(colnames(samsungData)[n], "Z", sep="")
}

colnames(samsungData) <- gsub('\\(|\\)',"",names(samsungData), perl = TRUE)

colnames(samsungData) <- gsub('\\-',"",names(samsungData), perl = TRUE)

colnames(samsungData) <- gsub('\\,',"",names(samsungData), perl = TRUE)

# 4) Use Descriptive Activity Names to name the activities in the data set:

*# 1 Walking

*# 2 WALKING_UPSTAIRS

*# 3 WALKING_DOWNSTAIRS

*# 4 SITTING

*# 5 STANDING

*# 6 LAYING

samsungData$Activity <- ifelse(samsungData$Activity ==1,"Walking",ifelse(samsungData$Activity == 2,"WALKING_UPSTAIRS",ifelse(samsungData$Activity == 3,"WALKING_DOWNSTAIRS",ifelse(samsungData$Activity == 4,"SITTING",ifelse(samsungData$Activity == 5,"STANDING","LAYING")))))
tail(samsungData,n=2)

# 5) Exract mean and std deviation columns for each measurement :

*# The below code searches all the columns of the data frame and exracts only those columns after matching the required 

*#string and renaming the data frame as 'tidydata'

samsungData_mean<-samsungData[,grep('mean',names(samsungData))]

samsungData_std<-samsungData[,grep('std',names(samsungData))]

samsungData_activity<-data.frame(samsungData[,grep('Activity',names(samsungData))])

samsungData_sub<-data.frame(samsungData[,grep('Subjects',names(samsungData))])


tidydata<-cbind(samsungData_mean,samsungData_std,samsungData_activity,samsungData_sub)

names(tidydata)[80]<-"Activity"

names(tidydata)[81]<-"Subjects"

# 5) Create a second independent tidy data set with the avg of each variables for each activity and each subject:

library(reshape2)
molten<-melt(tidydata,id=c("Activity","Subjects"))

# Reshaping:
data<-dcast(molten,formula=Subjects+Activity~.)

colnames(data)[3]<-"Cummulative"

# library(sqldf)
# avg_subject<-sqldf("subjects,avg(Cummulative) as occurance FROM data GROUP BY Subjects")
# 
# write.csv(data,"data.csv")




