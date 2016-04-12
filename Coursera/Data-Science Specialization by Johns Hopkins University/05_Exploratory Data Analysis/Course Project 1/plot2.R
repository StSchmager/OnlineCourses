## Reading  & Processing Data ##########################################################################################

# Load package
library(dplyr)

# Set working directory, weblink of data set, zip-folder name, and data file name
setwd("C:/Users/Stefan/Google Drive/Coursera/05_Exploratory Data Analysis/ExData_Plotting1")
data.source <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
zip.folder  <- "household_power_consumption.zip"
data.file   <- "household_power_consumption.txt"

# Download zip file if not already done so
if (!file.exists(zip.folder)) {
      download.file(data.source, zip.folder)
}

# Read data file that is extracted from zip folder
data        <- read.table(unz(zip.folder, data.file),
                          header = T,
                          sep = ";",
                          colClasses = c(rep("character", 2),
                                         rep("numeric",   7)),
                          na.strings = "?") %>%
      
      # Convert dates to "Date" class
      mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
      
      # Filter rows with relevany dates
      filter(Date >= "2007-02-01" & Date <= "2007-02-02")

# Convert date times (that were concatenated from Date and Time variable) to "PISIXlt" class
data$DateTime     <- strptime(paste(as.character(data$Date), data$Time), "%Y-%m-%d %H:%M:%S")

## Plotting Graph  & saving/copying into standard 480x480 PNG file #####################################################

plot(data$DateTime, data$Global_active_power,
     type = "l",
     xlab = "Date Time",
     ylab = "Global Active Power (kilowatts)")
dev.copy(png, file = "plot2.png")
dev.off()