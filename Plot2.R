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
NEI2 <- subset(x = NEI, NEI$fips == "24510")

# Summarizing the data set to calculate the total summation
plot2 <- NEI2 %>%
  group_by(year) %>%
  summarise(total = sum(Emissions))




# Plotting the bar chart
coul <- brewer.pal(4, "Blues")
with(data = plot2, {
  
  # Creating a PNG file.
  png(filename = "plot2.png", height = 480, width = 800)  
  
  # Add a outer margin to the plot.
  par(oma = c(1,1,1,1))
  
  # Creating the barchart plotting using base graphic system.
  p <- barplot(height = total, name = year, # Re-scaling to million.
               
               # Adding colour
               col = coul,
               # Adding title.
               main = expression('Aggregate PM'[2.5] ~ ' in Baltimore City'),
               
               # Adding y-axis label.
               ylab = expression('PM'[2.5] ~ 'Emissions in tons'),
               
               # Adding x-axis label.
               xlab = "Year");
  
  # Adding text over the bars.
  text(x = p,
       y = total - 100,            # Re-scaling to million.
       label = format(total, # Re-scaling to million.
                      nsmall = 1,          # Rounding the number.
                      digits = 1))
  
  # Closing the device.
  dev.off()      
})