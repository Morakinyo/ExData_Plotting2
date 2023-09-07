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


# Comparing Baltimore, MD-24510 and Los Angeles, CA-06037
baltimore <- summarise(group_by(filter(NEI, NEI$fips == "24510"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))
losAngeles <- summarise(group_by(filter(NEI, NEI$fips == "06037"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))

baltimore$County <- "Baltimore City, MD"
losAngeles$County <- "Los Angeles County, CA"

blEmissions <- rbind(baltimore, losAngeles)
# plot6.png
# Type: ON-ROAD, Fips = 24510 for Baltimore, MD Motor Vehicle PM[2.5]* Emissions Against Los Angeles, CA Fips = 06037  from 1999 to 2008
ggplot(blEmissions, aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions,2))) +
  geom_bar(stat="identity") + 
  facet_grid(County~., scales="free") +
  ylab(expression("Aggregate PM"[2.5]*" emissions in tons")) + 
  xlab("year") +
  ggtitle(expression("Baltimore City vs Los Angeles County Vehicle emissions in tons"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_label(aes(fill = County))

# Saving the plot to a PNG file
dev.copy(png,"plot6.png", width=480, height=480)
dev.off()