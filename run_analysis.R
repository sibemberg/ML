#1. Merges the training and the test sets to create one data set.
db_labels <- read.table("./features.txt") # load variables labels names
db_labels[,2] <- paste(db_labels[,1],db_labels[,2],sep="-") # I insert it to avoid duplicated colum name
db_labels <- gsub('[()-,]','',db_labels[,2]) # I insert it no remove unwanted characters

db_test <- read.table("./test/X_test.txt") # load testing database
db_train <- read.table("./train/X_train.txt") # load trainning database
mdb <- rbind(db_test, db_train) # merge both databases into mdb
names(mdb) <- db_labels # insert variables name into mdb

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

library(dplyr) # load dplyr
mdb <- mdb %>% select(matches("mean|std")) # select only the columns that have "mean" or "std" on their names

#3. Uses descriptive activity names to name the activities in the data set
db_activity_test <- read.table("./test/y_test.txt") # load activity test database
db_activity_train <- read.table("./train/y_train.txt") # load activity train database
mdb_activity <- rbind(db_activity_test, db_activity_train) # merge activity databases

activity_labels <- read.table("./activity_labels.txt") # load dictionay of activities levels and labels
mdb_activity[,2] <- factor(mdb_activity[,1], levels = activity_labels[,1], labels = activity_labels[,2] ) # insert the names on column 2

mdb <- mdb %>% mutate(Activity = mdb_activity[,2]) # merge activity into master database


#4. Appropriately labels the data set with descriptive variable names.

mdb <- mdb
names(mdb) <- sub('^[0-9]*-','',names(mdb)) # remove numbers before the first "-"
names(mdb) <- sub("^f","Frequency", names(mdb)) # replace "f" with "Frequency
names(mdb) <- sub("^t","Time", names(mdb)) # replace "t" with "Time"



#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

db_sub_test <- read.table("./test/subject_test.txt") # load subject test IDs
db_sub_train <- read.table("./train/subject_train.txt") # load subject train IDs
mdb_sub <- rbind(db_sub_test, db_sub_train) # merge IDs databases

mdb <- mdb %>% mutate(Subject = as.factor(mdb_sub[,1])) # insert subjects IDs into master database (as factor)
mdb <- mdb %>% select(Subject, Activity, everything()) # order Subject and Activity as first columns

#library(data.table) # load data.table library
#mdb <- data.table(mdb) # set mdb as data table format

summary <- aggregate(. ~ Subject + Activity, mdb, mean) # create the 2nd dataset with the averages
summary <- summary %>% arrange(Subject) # sort by subject first


#6.  Please upload your data set as a txt file created with write.table() using row.name=FALSE
write.table(summary, file="summary.txt", row.names = F)



