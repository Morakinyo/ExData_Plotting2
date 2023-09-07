library(ggplot2)
library(magrittr)
library(tidyverse)
library(RColorBrewer)

filename <- "exdata-data-NEI_data.zip"

if (!file.exists(exdata_filename)) {
  download_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(download_url, destfile = filename)
  unzip (zipfile = filename)
}

if (!exists("NEI")) {
  # print("Loading NEI Data, please wait.")
  NEI <- readRDS("summarySCC_PM25.rds") 
}

if (!exists("SCC")) {
  # print("Loading SCC Data.")
  SCC <- readRDS("Source_Classification_Code.rds")
}



# Gather the subset of the NEI data which corresponds to vehicles
# vehiclesSCC <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
 # vehiclesNEI <- NEI[NEI[, SCC] %in% vehiclesSCC,]


# Filtering NEI dataset of Coal from a specific SCC list.
NEI5 <- subset(x = NEI, NEI$fips=="24510" &  NEI$type=="ON-ROAD")

# Subset the vehicles NEI data to Baltimore's fip
# baltimoreVehiclesNEI <- vehiclesNEI[fips=="24510",]

plot5 <- NEI5 %>%
  group_by(year) %>%
  summarise(total = sum(Emissions))

png("plot5.png")

ggplot(plot5, aes(x = year, y = total)) +
  geom_bar(position = "stack", stat="identity", fill ="#FF9999" ,width=1.3) +
  geom_text(data = plot5,
                     aes(x = year,
                         label = base::format(x = total,
                                              nsmall = 1,digits = 1), # Rounding the values.
                         y = total,
                         fill = NULL),
                     nudge_y = 10) + 
  scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
  labs(x="year", y=expression('Aggregate PM' [2.5] ~ 'Emission in tons')) + 
  labs(title=expression('Vehicle Emissions PM'[2.5] ~ ' in Baltimore')) +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()