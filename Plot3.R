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

# Subsetting for Baltimore City.
NEI3 <- subset(x = NEI, NEI$fips == "24510")

# Summarizing the data set to calculate the total summation
options(dplyr.summarise.inform = FALSE) # Avoids the message `summarise()` 
#has grouped output by 'type'. You can override using the `.groups` argument.
plot3 <- NEI3 %>%
  group_by(type, year) %>%
  summarise(total = sum(Emissions))


  
  # Creating a PNG file.
  png(filename = "plot3.png", height = 480, width = 800) 
  
  ggplot(data = plot3,
         aes(x = year,
             y = total,
             label = format(x = total,
                            nsmall =1,
                            digits = 1))) +
    
  geom_line(aes(color=type), lwd =1) +
  
  geom_text(hjust = 0.5, vjust = 0.5) + 
    
    # Setting the years.
  scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
    
    # Editing the Graphic Tile.
  labs(title = base::expression('Emissions of PM'[2.5] ~ ' in Baltimore')) +
    
    # Adding x-axis label.
  xlab("Year") + 
    
    # Adding y-axis label.
  ylab(base::expression("Aggregate PM"[2.5] ~ "emission in tons")) +
    
    # Editing the legend position and tile position.
  theme(legend.position = "right",
                   legend.title.align = 0.5,
                   plot.title = ggplot2::element_text(hjust = 0.5))
  
  # Closing the device.
  grDevices::dev.off()
  
 