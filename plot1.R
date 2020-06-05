library(tidyverse)
library(janitor)
library(lubridate)

# Import data. 

NEI <- readRDS("summarySCC_PM25.rds") %>% 
        as_tibble %>% 
        clean_names %>% 
        mutate(year = as.factor(year)) %>%
        mutate(type = as.factor(type))
NEI
SCC <- readRDS("Source_Classification_Code.rds") %>% as_tibble %>% clean_names

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008 

# create total emission column 
NEI_total<- NEI %>% 
        group_by(year) %>%
        summarize(sum(emissions)) 

names(NEI_total) <- c("year","emissions")

# change to numeric year for plot to work correctly. First from factor to char, then to number
NEI_total<- NEI_total %>% 
        mutate(year = parse_number(as.character(year)))

NEI_total
# plot
png(file = "plot1.png", width = 640, height = 480)
cols<-c("red", "green", "blue", "yellow")
with(data = NEI_total, plot(year, emissions, xlab = "Year", ylab = "Total PM2.5 Emissions (tons)", 
                            main = "Total PM2.5 Emission by Year", pch = 20, cex = 2, col = cols))
model<-lm(emissions~year, data = NEI_total)
abline(model, lwd = 1, col = "red")
with(data = NEI_total, text(year[1:3], emissions[1:3], labels = round(emissions[1:3], digits = 3), pos = 4, offset = 0.5))
with(data = NEI_total, text(year[4], emissions[4], labels = round(emissions[4], digits = 3), pos = 2, offset = 0.5))
legend("topright", pch = 20, col = c("red", "green", "blue", "yellow"), legend = c("1999", "2002","2005","2008"))
dev.off()
