#Rod Paris
#Launch yout career in data science - Hospital Quality
#Computing in R
#2016

#Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
#outcome name. The function reads the outcome-of-care-measures.csv and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specifed outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of heart attack, heart failure, or pneumonia. Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
best <- function(state, outcome) {
        # state argument indicates the two letter state abbreviation you would like to gather data for
        # outcome argument indicates either heart attack, heart failure, or pneumonia 
        # function returns the hospital name with the best (lowest) 30-day mortality rate 
        # for the specified outcome in the given state
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        options(warn=-1) # sets R warnings off for NA data
        
        # make sure state entered is valid
        if(!(state %in% data$State)) { 
                stop("invalid state entered")
        }
        
        # check that outcome is valid and do appropriate calculations
        # found outcome numbers using provided informational PDF
        if(outcome == "heart attack") {
                findBest(11, state, data)
        }
        else if (outcome == "heart failure") {
                findBest(17, state, data)
        }
        else if(outcome == "pneumonia") {
                findBest(23, state, data)
        }
        else {
                stop("invalid outcome entered")
        }  
        
}

# helper function that finds the best hospiral for the given outcome number
findBest <- function(outcomeNumber, state, data) {
        data[, outcomeNumber] <- as.numeric(data[, outcomeNumber])
        data<-subset(data,data$State==state) # only need info for given state
        valMin<-min(data[[outcomeNumber]],na.rm=TRUE) # best in this case = minimum
        data<-subset(data,data[[outcomeNumber]]==valMin)
        data<-data[order(data[["Hospital.Name"]]),]
        return(data[1,"Hospital.Name"]) # return best hospital name
}