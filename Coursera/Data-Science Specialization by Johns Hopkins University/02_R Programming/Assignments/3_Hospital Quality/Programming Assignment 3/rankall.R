rankall <- function(outcome = as.character(), num = "best") {
     ## Read outcome data 
     # hospital name is set as character var. by default
     setwd("~/R/Coursera/R Programming/Assignments/3_Hospital Quality/rprog-data-ProgAssignment3-data")
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     # state is set as factor var.
     data$State <- as.factor(data$State)
     
     # outcome vars. are set as numeric (warning about NAs turned off and on again later)
     options(warn=-1)
     data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
     data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
     data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
     options(warn=0)
     
     # Maximum amount of hospitals per state
     max <- max(table(data$State))
     
     ## Check that outcome input is valid
     Outcome_lev <- c("heart attack", "heart failure", "pneumonia")
     if (outcome %in% Outcome_lev == F) stop("invalid outcome")

     ## For each state, find the hospital of the given rank
     
     if (outcome == "heart attack") {
          data_sub <- subset(data, select = c(
               State,
               Hospital.Name,
               Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
          data_sub <- data_sub[order(data_sub$State,
                                     data_sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                     data_sub$Hospital.Name, na.last = NA),]
     } else {
          
     if (outcome == "heart failure") {
          data_sub <- subset(data, select = c(
               State,
               Hospital.Name,
               Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
          data_sub <- data_sub[order(data_sub$State,
                                     data_sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                     data_sub$Hospital.Name, na.last = NA),]
     } else {
     
     if (outcome == "pneumonia") {
          data_sub <- subset(data, select = c(
               State,
               Hospital.Name,
               Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
          data_sub <- data_sub[order(data_sub$State,
                                     data_sub$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                     data_sub$Hospital.Name, na.last = NA),]
          }     
          
     }}
     
     if (num == "best") {
          hospital <- tapply(data_sub$Hospital.Name,
                             data_sub$State,
                             FUN = function(x) {x[1]}, simplify = F)
          
     } else {
          
          if (num == "worst") {
          hospital <- tapply(data_sub$Hospital.Name,
                 data_sub$State,
                 FUN = function(x) {x[length(x)]}, simplify = F)               
     
     } else {
               
               if (num > max) {
                    print(NA)
                    
               } else {
                         hospital <- tapply(data_sub$Hospital.Name,
                                data_sub$State,
                                FUN = function(x) {x[num]},
                                simplify = F)               
               }
     }}     
          
     ## Return a data frame with the hospital names and the (abbreviated) state name
     
     state <- dimnames(hospital)[[1]]
     hospital <- unlist(hospital)
     
     Result <- data.frame()
     Result <- cbind(hospital, state)
     Result <- as.data.frame(Result)
     Result
}