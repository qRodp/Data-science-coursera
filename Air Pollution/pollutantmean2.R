#Rod Paris
#Launch yout career in data science - Air Pollution
#Computing in R
#2016

pollutantmean2 <- function(directory, pollutant, id = 1:332) {
        filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        values <- numeric()
        
        for (i in id) {
                data <- read.csv(filelist[i])
                values <- c(values, data[[pollutant]])
        }
        mean(values, na.rm = TRUE)
        
}