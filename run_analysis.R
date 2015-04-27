run_analysis <- function () {
        ## check if the file has been download and unziped
        if (!file.exists("./UCI HAR Dataset")) {
                url1 <- "https://d396qusza40orc.cloudfront.net/
                getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                download.file(url1, destfile="proj1.zip")
                unzip("proj1.zip")
        }
        
        ## read training and test files
        # 1. labels
        actLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
        cnamesList <- read.table("./UCI HAR Dataset/features.txt")
        # 2. data
        fileTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
        fileTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
        # 3. subjects
        subTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        subTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        # 4. activities
        actTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
        actTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
        
        ## Merges the training and the test sets to create one data set
        fullData <- rbind(fileTrain, fileTest)
        fullAct <- rbind(actTrain, actTest)
        totalSub <- rbind(subTrain, subTest)
        
        ## Extracts only the measurements on the mean and 
        ## standard deviation for each measurement.
        # 1. clean "mean" data
        meanList1 <- grep("meanFreq", as.character(cnamesList[,2]))
        meanList2 <- grep( "mean", as.character( cnamesList[,2] ) )
        for (i in seq_along(meanList1)) {
                j <- which(meanList2==meanList1[i])
                meanList2 <- meanList2[-j]
        }
        # 2. clean "standard deviation" data
        stdList <- grep( "std", as.character( cnamesList[,2]) )
        totalList <- sort( c(meanList2, stdList) )
        meanstdData <- fullData[,totalList]
        # 3. data after clean
        meanstdData1 <- cbind(meanstdData, totalSub, fullAct)
        
        ## Uses descriptive activity names to name the activities in the data set
        for (k in 1:length(fullAct[,1])) {
                meanstdData1[k,"fullAct"] <- as.character(actLabels[fullAct[k,1],2])
        }

        ## Appropriately labels the data set with descriptive variable names.
        listNames <- c(as.character(cnamesList[totalList,2]),"subject","activities")
        colnames(meanstdData1) <- listNames
        
        ## Creates a second, independent tidy data set with the average of
        ## each variable for each activity and each subject
        tidyData <- aggregate(meanstdData1, by=list(subject=meanstdData1$subject, activities=meanstdData1$activities), mean)
        tidyData[,68] <- NULL
        tidyData[,69] <- NULL
        write.table(tidyData, "tidyData.txt", sep="\t")        
}
