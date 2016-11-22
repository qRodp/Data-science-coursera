#Rod Paris
#Launch yout career in data science - Air Pollution
#Computing in R
#2016

#Write a function named 'pollutantmean' that calculates the mean of 
#a pollutant (sulfate or nitrate) across a specified list of monitors.
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant',
#and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that
#monitors' particulate matter data from the directory specified in the 'directory' 
#argument and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332){
        files_list <- list.files(directory, full.names=TRUE)[id]
        dat <- data.frame()
        for(i in seq_along(files_list)) {
                test <- read.csv(files_list[i])
                dat <- rbind(dat, test)
        }
        ds <- mean(dat[, pollutant], na.rm = TRUE) #return the mean of the pollutant
        ds
}