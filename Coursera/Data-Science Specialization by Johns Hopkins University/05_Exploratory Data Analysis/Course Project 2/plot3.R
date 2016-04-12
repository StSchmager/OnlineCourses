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

## Plot 3 ##############################################################################################################
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City?
# Which have seen increases in emissions from 1999-2008?
# Use the ggplot2 plotting system to make a plot answer this question.

data3 <- filter(data0a, fips == "24510") %>%
      mutate(fips = factor(fips, levels = c("24510"), labels = c("Baltimore City, Maryland"))) %>%
      group_by(fips, type, year) %>%
      summarize(TotalEmissions = sum(Emissions))

png(filename = "plot3.png", width = 600)
ggplot(data = data3,
       aes(as.factor(year), TotalEmissions/1000)) +
      geom_bar(stat = "identity") +
      facet_grid(fips ~ type) +
      labs(title = expression(paste("Total ", PM[2.5], " Emissions from 1999 to 2008 in Baltimore City by Source Type")),
           y     = expression(paste("Total Emissions from ", PM[2.5], " (in Thousand Tons)")),
           x     = "Years") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()