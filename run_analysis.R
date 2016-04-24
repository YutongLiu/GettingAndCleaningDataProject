##Getting and Cleaning data

##The Input
#Download and unzip the raw data
Url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url, destfile = "Human_Activity_Recognition.zip")
unzip("Human_Activity_Recognition.zip")
list.dirs("./")

#Get the file name
trainfile<-list.files("./UCI HAR Dataset/train",full.names = T)
testfile<-list.files("./UCI HAR Dataset/test",full.names = T)

#Read and save the train and test set
trainset<-read.table(trainfile[3])
testset<-read.table(testfile[3])

##Merge vertically the training and the test sets 
##to create one dataset called"totalset"
##(already known that the train and test set have the same variables)
totalset<-rbind(trainset,testset)

##Extracts only the measurements on the mean and 
##standard deviation for each measurement
feature<-read.table("./UCI HAR Dataset/features.txt")
f_position<-as.integer(feature$V2)
f_name<-levels(feature$V2)

#Find the mean variable column and extract
meanposition<-grep("[Mm][Ee][Aa][Nn]\\(\\)",f_name)
m_position<-c(rep(NA,561))
for (i in 1:561) {
        for (j in 1:length(meanposition)) {
                if(f_position[i]==meanposition[j]){
                        m_position[i]<-i
                        next
                }
                
        }
        
}
bad<-is.na(m_position)
m_position<-m_position[!is.na(m_position)]
all_mean<-select(totalset,m_position)

#Find the standard deviation variable column and extract
stdposition<-grep("[Ss][Tt][Dd]\\(\\)",f_name)
std_position<-as.integer(c(rep(NA,561)))
for (i in 1:561) {
        for (j in 1:length(stdposition)) {
                if(f_position[i]==std[j]){
                        std_position[i]<-i
                        next
                }
                
        }
        
}
bad<-is.na(std_position)
std_position<-std_position[!is.na(std_position)]
all_std<-select(totalset,std_position)

##Appropriately labels the data set with descriptive variable names
#Get the feature name
feature<-read.table("./UCI HAR Dataset/features.txt")
featurename<-tolower(feature$V2)
featurename<-gsub("\\(\\)","",featurename)
#Rename the variables
mean_feature<-featurename[m_position]
names(all_mean)<-mean_feature
std_feature<-featurename[std_position]
names(all_std)<-std_feature

#Combine the measurements on the mean and standard deviation
mean_std<-cbind(all_mean,all_std)

##Uses descriptive activity names to name the activities in the data set
trainlabel<-read.table(trainfile[4])
testlabel<-read.table(testfile[4])
actnamefile<-read.table("./UCI HAR Dataset/activity_labels.txt")
actname<-character(6L)
n_position<-as.integer(actnamefile$V2)
n_name<-levels(actnamefile$V2)
for(i in 1:length(actnamefile$V2)){
        actname[i]=n_name[n_position[i]]
}
activity<-character(nrow(mean_std))
for (i in 1:nrow(trainlabel)) {
        activity[i]<-actname[trainlabel$V1[i]]
}
for (i in 1:nrow(testlabel)) {
        activity[i+nrow(trainlabel)]<-actname[testlabel$V1[i]]
}
mean_std<-cbind(mean_std,activity)

##Labels the subjects of each row
trainsubject<-read.table(trainfile[2])
testsubject<-read.table(testfile[2])
subject<-rbind(trainsubject,testsubject)
names(subject)<-"subject"
mean_std<-cbind(mean_std,subject)

##A new data set called "avg_set" with the average of each subject and activity
mean_std_group<-group_by(mean_std,subject,activity)
avg_set<-summarize_each(mean_std_group,funs(mean))##already checked is.na(mean_std_group)
avg_set<-avg_set[,1:66]

##The output
#Save the tidy data set in a txt file
write.table(avg_set,file = "./average set.txt")
