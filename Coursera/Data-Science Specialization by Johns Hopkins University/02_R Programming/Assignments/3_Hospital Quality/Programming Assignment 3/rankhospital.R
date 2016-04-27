rankhospital <- function(state = as.character(), outcome = as.character(), num = "best") {
     
     ## Read outcome data & Check that state and outcome are valid
     setwd("~/R/Coursera/R Programming/Assignments/3_Hospital Quality/rprog-data-ProgAssignment3-data")
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     data$State <- as.factor(data$State)
     State_lev <- levels(data$State)
     if (state %in% State_lev == F) stop("invalid state")
     
     Outcome_lev <- c("heart attack", "heart failure", "pneumonia")
     if (outcome %in% Outcome_lev == F) stop("invalid outcome")
     
     options(warn=-1)
     data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
     data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
     data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
     options(warn=0)
     
     
     if (outcome == "heart attack") {
          data_sub <- subset(data, State == state, select = c(
               Hospital.Name,
               Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
          data_sub <- data_sub[order(data_sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                     data_sub$Hospital.Name, na.last = NA),]
          
     } else {
          
     if (outcome == "heart failure") {
          data_sub <- subset(data, State == state, select = c(
               Hospital.Name,
               Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
          data_sub <- data_sub[order(data_sub$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                     data_sub$Hospital.Name, na.last = NA),]
          
     } else {
               
     if (outcome == "pneumonia") {
          data_sub <- subset(data, State == state, select = c(
               Hospital.Name,
               Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
          data_sub <- data_sub[order(data_sub$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                     data_sub$Hospital.Name, na.last = NA),]
     }}}     
          
     ## Return hospital name in that state with the given rank 30-day death rate

     if (num == "best") {
          data_sub[1, 1]
          
     } else {
          
     if (num == "worst") {
          data_sub[nrow(data_sub), 1]
     
     } else {
          
     if (num > nrow(data_sub)) {
          print(NA)
               
     } else {
          data_sub[num, 1]}
     
     }}

}