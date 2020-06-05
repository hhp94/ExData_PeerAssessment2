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

# 3. Of the 4 types (point, nonpoint, onroad, nonroad) which of these sources have seen decreases in emissions. 

# create total emission column for Baltimore City, based on groups
NEI_balti_type<- NEI %>% filter(fips == "24510") %>%
        group_by(year, type) %>%
        summarize(sum(emissions))

names(NEI_balti_type)<-c("year","type","emissions")

NEI_balti_type<- NEI_balti_type %>%
        mutate(year = parse_integer(as.character(year))) 

# plot
ggplot<-ggplot(data = NEI_balti_type, aes(year, emissions))+ 
        geom_point(color = as.factor(NEI_balti_type$year), size = 4)+
        geom_smooth(method = "lm", se = FALSE, size = 0.5)+
        geom_text(data = NULL, label = round(NEI_balti_type$emissions, digits = 2), nudge_y = c(55,100,45,100))+
        labs(x = "Year", y = expression(PM[2.5] * " Emissions (Tons)"), 
             title = "Total PM2.5 Emission in Baltimore City  by Year and Type")+
        facet_grid(NEI_balti_type$type~., scale = "free")+
        scale_x_continuous(breaks=c(1999:2008), labels=c(1999:2008))
png(file = "plot3.png", width = 640, height = 480)
ggplot
dev.off()
