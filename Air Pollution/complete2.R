#Rod Paris
#Launch your career in data science - Air Pollution
#Computing in R
#2016

complete2 <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        nobs <- function(id) {
                path <- file.path(directory, paste(sprintf("%03d", as.numeric(id)), ".csv", sep=""))
                return (sum(complete.cases(read.csv(path))))
        }
        return (data.frame(id=id, nobs=sapply(id, nobs))) #complete.R with sapply function
}