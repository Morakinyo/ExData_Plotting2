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


list_SCC <- SCC %>% 
  filter(grepl(x = EI.Sector,
                            pattern = "Coal|coal")) %>%
  select(SCC, EI.Sector)

# Filtering NEI dataset of Coal from a specific SCC list.
NEI4 <- base::subset(x = NEI, SCC %in% list_SCC$SCC)

# Merging list_SCC and NEI_q4 to insert a column of EI.Sector on NEI dataset.
NEI4_v2 <- base::merge(NEI4, list_SCC)

# Calculating the total summation and removing some patterns from EI.Sector column.
NEI4_v3 <- NEI4_v2 %>%
  group_by(year, EI.Sector) %>%                                    # Grouping to summarize it by year and EI.Sector.
  summarise(total = base::sum(Emissions)) %>%                      # Calculating the total column.
  mutate(EI.Sector = base::gsub(pattern = "Fuel Comb - | - Coal",  # Removing some patterns from EI.Sector
                                       replacement =  "",                 # Cleaning the info from EI.Sector column.
                                       x = EI.Sector))

# Auxiliary dataset to calculate the total of each bar.
NEI4_v3_total <- NEI4_v3 %>%
  summarise(total = sum(total)/1000)



# Exporting a PNG file. 
png(filename = "plot4.png", height = 480, width = 800)

# Creating a ggplot2 graph.
ggplot(data = NEI4_v3,
                aes(x = year,
                    y = total/1000,       # Re-scaling to thousands.
                    fill = EI.Sector)) +
  
  # Defining stacked bars.
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = c("steelblue", "cadetblue", "seagreen3")) +
  
  # Adding labels with value over the bars.
  geom_text(data = NEI4_v3_total,
                     aes(x = year,
                              label = format(x = total,
                                                    nsmall = 1, digits = 1), # Rounding the number.
                              y = total,
                                  fill = NULL),
                     nudge_y = 10) + # Distance to the point.
  
  # Setting the years.
  scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
  
  # Adding title
  labs(title = base::expression('Coal Combustion PM'[2.5] ~ ' in the United States')) + 
  
  # Adding x-axis label.
  xlab("Year") +
  
  # Adding y-axis label.
  ylab(expression('PM'[2.5] ~ 'Emissions (10' ^3 ~ 'tons)')) +
  
  # Editing the legend position and tile position.
  theme(legend.position = "bottom",
        legend.title.align = 0.5,
                 plot.title = element_text(hjust = 0.5)) +
  
  # Removing the legend title.
  guides(fill = guide_legend(title = ""))

# Closing the device.
dev.off()