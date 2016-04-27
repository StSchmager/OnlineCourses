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