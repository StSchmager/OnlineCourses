## Reading  & Processing Data ##########################################################################################

# Load package
library(plyr)
library(dplyr)
library(ggplot2)

# Set working directory, weblink of data set, zip-folder name, and data file name
setwd("C:/Users/Stefan/Google Drive/Coursera/05_Exploratory Data Analysis/ExData_Plotting2")
data.source <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zip.folder  <- "NEI_data.zip"
data.file1  <- "summarySCC_PM25.rds"
data.file2  <- "Source_Classification_Code.rds"

# Download zip file if not already done so
if (!file.exists(zip.folder)) {
      download.file(data.source, zip.folder)
}

# Extract files from zip file if not already done so
if (!file.exists(data.file1)) {
      unzip(zip.folder, c(data.file1,
                          data.file2))
}


# Read extracted data files
data0a <- readRDS(data.file1)
data0b <- readRDS(data.file2)

## Define variables
# A five-digit number (represented as a string) indicating the U.S. county
data0a$fips          <- factor(data0a$fips)
# The name of the source as indicated by a digit string (see source code classification table)
data0a$SCC           <- factor(data0a$SCC)
# A string indicating the pollutant
data0a$Pollutant     <- factor(data0a$Pollutant)
# Amount of PM2.5 emitted, in tons
data0a$Emissions     <- as.numeric(data0a$Emissions) 
# The type of source (point, non-point, on-road, or non-road)
data0a$type          <- factor(data0a$type)
# The year of emissions recorded
data0a$year          <- as.integer(data0a$year)

## Plotting/Saving Graphs ##############################################################################################

## Plot 1 ##############################################################################################################
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot
# showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

data1 <- group_by(data0a, year) %>% summarize(TotalEmissions = sum(Emissions))

png(filename = "plot1.png")
with(data1, {
      plot(year, TotalEmissions/1000000,
           type = "b",
           main = expression(paste("Total ", PM[2.5], " Emissions from 1999 to 2008 in All U.S. Counties")),
           ylab = expression(paste("Total Emissions from ", PM[2.5], " (in Million Tons)")),
           xlab = "Years",
           xaxt="n")
      axis(1, at = seq(1999, 2008, by = 1), las=2)
})
dev.off()