complete <- function(directory, id = 1:332) {
     
     setwd("~/R/Coursera/R Programming/Assignments/1_Air_Pollution")
     
     files_list <- list.files(directory, full.names=TRUE)
     files_number <- length(files_list)
     dat <- data.frame()
     for (i in 1:files_number) {
          dat <- rbind(dat, read.csv(files_list[i]))
     }
     
     nobs_s <- complete.cases(dat$sulfate)
     nobs_n <- complete.cases(dat$nitrate)
     nobs <- ifelse(nobs_s == T & nobs_n == T, 1, 0)
     nobs_sum <- nobs_s + nobs_n
     nobs_test <- ifelse(nobs_sum > 1, 1, 0)
     
     dat2 <- cbind (dat, nobs_s, nobs_n, nobs, nobs_test, nobs_sum)
     
     dat3 <-  subset(dat2, select = c(ID, nobs))
     
     dat4 <- melt(dat3, id.vars = "ID", measure.vars = "nobs")
     dat5 <- cast(dat4, ID~variable, sum)
     
     dat_result <- subset(dat5, ID %in% id)
     as.data.frame(dat_result, row.names = NULL)
}
