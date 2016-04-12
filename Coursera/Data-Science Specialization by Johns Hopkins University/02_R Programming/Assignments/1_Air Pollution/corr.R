corr <- function(directory, threshold = 0) {
     
          setwd("~/R/Coursera/R Programming/Assignments/1_Air_Pollution")
          
          files_list <- list.files(directory, full.names=TRUE)
          files_number <- length(files_list)
          dat <- data.frame()
          for (i in 1:files_number) {
               dat <- rbind(dat, read.csv(files_list[i]))
          }
          
          ### Copied & pasted from Part 2
          
          nobs_s <- complete.cases(dat$sulfate)
          nobs_n <- complete.cases(dat$nitrate)
          nobs <- ifelse(nobs_s == T & nobs_n == T, 1, 0)
          as.integer(nobs)
          nobs_sum <- nobs_s + nobs_n
          nobs_test <- ifelse(nobs_sum > 1, 1, 0)
          
          dat2 <- cbind (dat, nobs_s, nobs_n, nobs, nobs_test, nobs_sum)
          
          dat3 <-  subset(dat2, select = c(ID, nobs))
          
          dat4 <- melt(dat3, id.vars = "ID", measure.vars = "nobs")
          dat5 <- cast(dat4, ID~variable, sum)
          
          ###################################################################
          
          cr <- data.frame()
          for (i in 1:max(dat$ID)) {
               dat_sulfate <- unlist(subset(dat, ID == i, select = sulfate))
               dat_nitrate <- unlist(subset(dat, ID == i, select = nitrate))
               cr[i, 1] <- cor(x = dat_sulfate, y = dat_nitrate, use = "p")
          }
          colnames(cr) <- "Correlation"
          crnobs <- cbind(dat5, cr)
          
          result <- vector()
          
          for (i in 1:max(crnobs$ID)) {
               if (crnobs$nobs[i] > threshold) {result[i] <- crnobs$Correlation[i]} 
          }
          
          result[!is.na(result)]
     }