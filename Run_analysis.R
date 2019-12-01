### Coursera: Getting and cleaning data with R
### Peer reviewed assignement week 4

root_dir <- './your_directory' # select here the relevant directory name
main_dir <- './UCI HAR Dataset'
train_dir <- './train'
test_dir <-  './test'

setwd(root_dir) 

### 1. Merges the training and the test sets to create one data set

## read features names
setwd(main_dir) 
features <- read.table(file="features.txt")

## read training files 
setwd(train_dir) 
training_data <- read.table(file="X_train.txt",header=FALSE)
training_label <- read.table(file="y_train.txt")
training_subject <- read.table(file="subject_train.txt")
setwd(main_dir) 


# create empty table
training = data.frame(matrix("", ncol = 563, nrow = 7352) )

# add subject, features, activity information
training[,1] <- training_subject
names(training)[1]<-'subject'

training[,2:562] <- training_data
names(training)[2:562] <- features[,1]

training[,563] <- training_label
names(training)[563]<-'activity'

# add column indicating training/test set
training$set <- 'training'

## read test files 
setwd(test_dir) 
test_data <- read.table(file="X_test.txt",header=FALSE)
test_label <- read.table(file="y_test.txt")
test_subject <- read.table(file="subject_test.txt")
setwd(main_dir) 

# create empty table
test = data.frame(matrix("", ncol = 563, nrow = 2947) )

# add subject, features, activity information
test[,1] <- test_subject
names(test)[1]<-'subject'

test[,2:562] <- test_data
names(test)[2:562] <- features[,1]

test[,563] <- test_label
names(test)[563]<-'activity'

# add column indicating training/test set
test$set <- 'test'

## Merge the 2 datasets
data <- bind_rows(training,test)




### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
temp <- as.numeric(c(grep('mean\\()',features$V2),grep('std\\()',features$V2)))

feature_select <- temp[order(temp)]

data2 <- select(data,set,subject,activity,as.character(feature_select))



### 3. Uses descriptive activity names to name the activities in the data set
activity_label <- read.table(file="activity_labels.txt")
activity_label <- rename(activity_label,activity=V1,activity_name=V2)

tidy_data <- data2 %>% merge(activity_label,by="activity") %>% arrange(desc(set),subject,activity) %>%  select(set,subject,activity,activity_name,'1':'543')



### 4. Appropriately labels the data set with descriptive variable names () 

varnames <- gsub("-","_",features[feature_select,2]) #replace unwanted/untidy characters
varnames <- gsub("\\()","",varnames)

names(tidy_data )[5:70] <- varnames 



### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

by_activity <- group_by(tidy_data,set,subject,activity, activity_name) 
average_tidy_data <- summarize_all(by_activity,"mean",na.rm=TRUE)
average_tidy_data <- arrange(result,desc(set),subject,activity)

### Output

if (!file.exists("Output")) {dir.create("Output")}
setwd("./Output")

write.table(tidy_data,file="tidy_data.txt",sep=' ',quote=FALSE)
#write.table(average_tidy_data,file="tidy_data_average.txt",sep=' ',quote=FALSE)
write.table(average_tidy_data,file="tidy_data_average.txt",row.names = FALSE,quote=FALSE)



