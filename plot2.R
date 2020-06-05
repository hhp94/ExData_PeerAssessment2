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

# 2. Total emissions in Baltimore City (fips == "24510") from 1999 to whatever

# create total emission column for Baltimore City
NEI_balti<- NEI %>% filter(fips == "24510") %>%
        mutate(year = as.factor(year)) %>%
        group_by(year) %>%
        summarize(sum(emissions)) 

names(NEI_balti) <- c("year","emissions")

# change to numeric year for plot to work correctly
NEI_balti<- NEI_balti %>%
        mutate(year = parse_number(as.character(year)))

NEI_balti
# plot
cols<-c("red", "green", "blue", "yellow")

png(file = "plot2.png", width = 640, height = 480)
with(data = NEI_balti, plot(year, emissions, xlab = "Year", ylab = "Total PM2.5 Emissions in Baltimore City (tons)", 
                            main = "Total PM2.5 Emission by Year in Baltimore City", pch = 20, cex = 2, col = cols))
model_balti<-lm(emissions~year, data = NEI_balti)
abline(model_balti, lwd = 1, col = "red")
with(data = NEI_balti, text(year[1:3], emissions[1:3], labels = emissions[1:3], pos = 4, offset = 0.5))
with(data = NEI_balti, text(year[4], emissions[4], labels = emissions[4], pos = 2, offset = 0.5))
legend("topright", pch = 20, col = c("red", "green", "blue", "yellow"), legend = c("1999", "2002","2005","2008"))
dev.off()
