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

# 6 Emissions from motor vehicle sources in Baltimore Ciy vs California

#merge 2 datasets
NEI_2<-left_join(NEI, SCC, by = "scc")

#plot
NEI_5 <- NEI_2 %>% filter(fips %in% c("24510","06037")) %>%
        filter(str_detect(scc_level_three, ("Motor|Vehicle"))) %>%
        mutate(county = factor(fips, levels =c("06037","24510"), labels = c("Los Angeles County, CA","Baltimore City, MD"))) %>%
        group_by(year, county) %>%
        summarize(sum(emissions)) %>% 
        mutate(year = parse_number(as.character(year)))
names(NEI_5)<- c("year", "county", "emissions")

ggplot_3<- ggplot(data = NEI_5, aes(year, emissions))+
        geom_point(color = NEI_5$year, size = 4)+
        facet_grid(NEI_5$county~., scale = "free")+
        geom_smooth(method = "lm", se = FALSE, size = 0.5)+
        labs(x = "Year", y = expression(PM[2.5] * " Emissions (Tons)"), 
             title = "Total PM2.5 Emission from motor vehicle sources in the US by Year")+
        geom_text(data = NULL, label = round(NEI_5$emissions, digits = 2), nudge_y = c(30,15)) +
        scale_x_continuous(breaks=c(1999:2008), labels=c(1999:2008))

png(file = "plot6.png", width = 640, height = 480)
ggplot_3
dev.off()

