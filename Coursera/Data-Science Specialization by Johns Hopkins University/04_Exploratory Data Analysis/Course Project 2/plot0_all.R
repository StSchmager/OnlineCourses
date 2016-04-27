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

## Plot 2 ##############################################################################################################
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

data2 <- filter(data0a, fips == "24510") %>%
      mutate(fips = factor(fips, levels = c("24510"), labels = c("Baltimore City, Maryland"))) %>%
      group_by(fips, year) %>%
      summarize(TotalEmissions = sum(Emissions))

png(filename = "plot2.png")
with(data2, {
      plot(year, TotalEmissions/1000,
           type = "b",
           main = expression(paste("Total ", PM[2.5], " Emissions from 1999 to 2008 in Baltimore City, Maryland")),
           ylab = expression(paste("Total Emissions from ", PM[2.5], " (in Thousand Tons)")),
           xlab = "Years",
           xaxt="n")
      axis(1, at = seq(1999, 2008, by = 1), las=2)
})
dev.off()

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



## Plot 5 ##############################################################################################################
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
levels(data0b$EI.Sector)
MotorVehicelSrc <-      levels(data0b$EI.Sector)[49:52]

data5 <- filter(data0a, fips == "24510") %>%
      mutate(fips = factor(fips, levels = c("24510"), labels = c("Baltimore City, Maryland"))) %>%
      join(data0b) %>%
      filter(EI.Sector %in% MotorVehicelSrc) %>%
      group_by(fips, EI.Sector, year) %>%
      summarize(TotalEmissions = sum(Emissions)) %>%
      arrange(desc(TotalEmissions))

png(filename = "plot5.png", width = 800)
ggplot(data = data5,
       aes(as.factor(year), TotalEmissions)) +
      geom_bar(stat = "identity", aes(fill = EI.Sector), position = "stack") +
      labs(title = expression(paste("Total ", PM[2.5], " Emissions from 1999 to 2008 in Baltimore City by Motor Vehicle-Related Source Type")),
           y     = expression(paste("Total Emissions from", PM[2.5], " (in Tons)")),
           x     = "Years") +
      theme(legend.position = "bottom", legend.direction = "vertical") +
      scale_fill_discrete(name = "Motor Vehicle-Related Source Types",
                          labels = c("Diesel Heavy-Duty Vehicles",
                                     "Diesel Light-Duty Vehicles",
                                     "Gasoline Heavy-Duty Vehicles",
                                     "Gasoline Light-Duty Vehicles"))
dev.off()

## Plot 6 ##############################################################################################################
# Compare
# emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

data6 <- filter(data0a, fips %in% c("24510", "06037")) %>%
      mutate(fips = factor(fips, levels = c("24510", "06037"), labels = c("Baltimore City, Maryland", "Los Angeles County, California"))) %>%
      join(data0b) %>%
      filter(EI.Sector %in% MotorVehicelSrc) %>%
      group_by(fips, EI.Sector, year) %>%
      summarize(TotalEmissions = sum(Emissions)) %>%
      arrange(desc(TotalEmissions))

png(filename = "plot6.png", width = 800)
ggplot(data = data6,
       aes(as.factor(year), TotalEmissions/1000)) +
      geom_bar(stat = "identity", aes(fill = EI.Sector), position = "stack") +
      labs(title = expression(paste("Total ", PM[2.5], " Emissions from 1999 to 2008 in Specific U.S. Counties by Motor Vehicle-Related Source Type")),
           y     = expression(paste("Total Emissions from ", PM[2.5], " (in Thousand Tons)")),
           x     = "Years") +
      facet_grid(fips~., scales = "free_y") +
      theme(legend.position = "bottom", legend.direction = "vertical") +
      scale_fill_discrete(name = "Motor Vehicle-Related Source Types",
                          labels = c("Diesel Heavy-Duty Vehicles",
                                     "Diesel Light-Duty Vehicles",
                                     "Gasoline Heavy-Duty Vehicles",
                                     "Gasoline Light-Duty Vehicles"))
dev.off()