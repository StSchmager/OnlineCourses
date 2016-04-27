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

png(filename = "plot4.png")

par(mfrow = c(2, 2))

# top-left
plot(data$DateTime, data$Global_active_power,
     type = "l",
     xlab = "Date Time",
     ylab = "Global Active Power (kilowatts)")

# top-right
plot(data$DateTime, data$Voltage,
     type = "l",
     xlab = "Date Time",
     ylab = "Voltage (volt)")

# bottom-left
plot(data$DateTime, data$Sub_metering_1, col = "black",
     type = "l",
     xlab = "Date Time",
     ylab = "Energy Sub Metering (watt-hour)")
lines(data$DateTime, data$Sub_metering_2, col = "red")
lines(data$DateTime, data$Sub_metering_3, col = "blue")
legend("topright",
       bty = "n",
       lty = 1, 
       col    = c("black",          "red",            "blue"),
       legend = c("Sub metering 1", "Sub metering 2", "Sub metering 3"))

# bottom-right
plot(data$DateTime, data$Global_reactive_power,
     type = "l",
     xlab = "Date Time",
     ylab = "Global Reactive Power (kilowatts)")

dev.off()