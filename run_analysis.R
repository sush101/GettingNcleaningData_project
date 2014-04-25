# Getting and cleaning the data:
# Coursera Project:

rm(list=ls(all=TRUE)) 

# Reading train text files:
setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train")
x_train<-read.table("X_train.txt")

# Reading Features Text file:
setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
feature<-read.table("features.txt")
feature<-feature[-c(1)]

featureT<-data.frame(t(feature))
row.names(featureT)=NULL

colnames(x_train)<-colnames(featureT)
x_train<-rbind(x_train,featureT)

colnames(x_train)<-x_train[7353,]
x_train<-x_train[-7353,]

# Now add two columns at the last:
# Reading Train text file :
setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train")
Y_train<-read.table("y_train.txt")
subjects_train<-read.table("subject_train.txt")

train<-cbind(x_train,Y_train,subjects_train)

colnames(train)[562]<-"Activity"
colnames(train)[563]<-"Subjects"

# Now working on Test data :
# Reading Test text files:
setwd("D:/sushma/GettingnCleaningTheData/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test")
x_test<-read.table("X_test.txt")

colnames(x_test)<-colnames(featureT)
x_test<-rbind(x_test,featureT)

colnames(x_test)<-x_test[2948,]
x_test<-x_test[-2948,]

# Reading Test text file
Y_test<-read.table("y_test.txt")
subjects_test<-read.table("subject_test.txt")

test<-cbind(x_test,Y_test,subjects_test)

colnames(test)[562]<-"Activity"
colnames(test)[563]<-"Subjects"

# tail(test,n=2)

samsungData<-rbind(train,test)
rm(featureT,subjects_test,Y_test,x_test,x_train,Y_train,subjects_train,feature)


# Working with column names of samsungData:
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

# 3) Use Descriptive Activity Names to name the activities in the data set:
samsungData$Activity <- ifelse(samsungData$Activity ==1,"Walking",ifelse(samsungData$Activity == 2,"WALKING_UPSTAIRS",ifelse(samsungData$Activity == 3,"WALKING_DOWNSTAIRS",ifelse(samsungData$Activity == 4,"SITTING",ifelse(samsungData$Activity == 5,"STANDING","LAYING")))))
tail(samsungData,n=2)

# Step 2: Exract mean and std columns :

samsungData_mean<-samsungData[,grep('mean()',names(samsungData))]
samsungData_std<-samsungData[,grep('std()',names(samsungData))]
samsungData_activity<-data.frame(samsungData[,grep('Activity',names(samsungData))])
samsungData_sub<-data.frame(samsungData[,grep('Subjects',names(samsungData))])

tidydata<-cbind(samsungData_mean,samsungData_std,samsungData_activity,samsungData_sub)
names(tidydata)[80]<-"Activity"
names(tidydata)[81]<-"Subjects"

rm(samsungData_mean,samsungData_std,samsungData_activity,samsungData_sub)

# 5) Below code creates a second independent tidy data set with avgerege of variables for each activity and each subject.
# It uses reshape2 package.
library(reshape2)
molten<-melt(tidydata,id=c("Activity","Subjects"))

# Reshaping:
data<-dcast(molten,formula=Subjects+Activity~.)

colnames(data)[3]<-"Cummulative"

write.csv(tidydata,"TidyData.csv")

