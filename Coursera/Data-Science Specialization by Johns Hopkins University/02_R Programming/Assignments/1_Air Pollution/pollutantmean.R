pollutantmean <- function(directory, pollutant, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'pollutant' is a character vector of length 1 indicating
     ## the name of the pollutant for which we will calculate the
     ## mean; either "sulfate" or "nitrate".
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return the mean of the pollutant across all monitors list
     ## in the 'id' vector (ignoring NA values)
     
     setwd("~/R/Coursera/R Programming/Assignments/1_Air_Pollution")
     
     files_list <- list.files(directory, full.names=TRUE)
     files_number <- length(files_list)
     dat <- data.frame()
     for (i in 1:files_number) {
          dat <- rbind(dat, read.csv(files_list[i]))
     }
     dat_subset <- subset(dat, ID %in% c( id ), select = pollutant)
     
     mean(dat_subset[,pollutant], na.rm = TRUE)                            
}