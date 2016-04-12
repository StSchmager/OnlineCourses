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

## Plot 4 ##############################################################################################################
# Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
# What are coal combustion-related sources?
# Coal Ash (Coal Combustion Residuals, or CCR):
# "Coal combustion residuals, commonly known as coal ash, are created when coal is burned by power plants
# to produce electricity. Coal ash is one of the largest types of industrial waste generated in the United States.
# In 2012, 470 coal-fired electric utilities generated about 110 million tons of coal ash."
# (Source: U.S. Environmental Protection Agency, http://www2.epa.gov/coalash)

levels(data0b$EI.Sector)
CoalCombSrc <-    levels(data0b$EI.Sector)[c(13, 18, 23)]

data4 <- join(data0a, data0b) %>%
      filter(EI.Sector %in% CoalCombSrc) %>%
      group_by(EI.Sector, year) %>%
      summarize(TotalEmissions = sum(Emissions)) %>%
      arrange(desc(TotalEmissions))

png(filename = "plot4.png", width = 800)
ggplot(data = data4,
       aes(x = as.factor(year), y = TotalEmissions/1000000)) +
      geom_bar(stat = "identity", aes(fill = EI.Sector), position = "stack") +
      labs(title = expression(paste("Total ", PM[2.5], " Emissions from 1999 to 2008 In All U.S. Counties by Coal Combustion-Related Source Type")),
           y     = expression(paste("Total Emissions from ", PM[2.5], " (in Million Tons)")),
           x     = "Years") + 
      theme(legend.position = "bottom", legend.direction = "vertical") +
      scale_fill_discrete(name = "Coal Combustion-Related Source Types",
                          labels = c("Commercial/Institutional",
                                     "Electric Generation",
                                     "Industrial Boilers, ICEs"))
dev.off()