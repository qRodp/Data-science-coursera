#Rod Paris
#Launch your career in data science - Air Pollution
#Computing in R
#2016

#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of the file 
#and the second column is the number of complete cases.
complete <- function(directory, id = 1:332){
        files_list <- list.files(directory, full.names=TRUE)[id]
        dat <- data.frame() #create empty data frame
        for (i in seq_along(files_list)) {
                test <- read.csv(files_list[i]) #read each file and retrieve complete rows
                good <- complete.cases(test)
                ncomplete <- nrow(test[good,])
                
                dat <- rbind(dat, data.frame(id=i,nobs=ncomplete)) #merge all complete rows
        }
        dat
}