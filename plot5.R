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

# 5 Emissions from motor vehicle sources in Baltimore City

#merge 2 datasets
NEI_2<-left_join(NEI, SCC, by = "scc")

#plot
NEI_4 <- NEI_2 %>% filter(fips == "24510") %>%
        filter(str_detect(scc_level_three, ("Motor|Vehicle"))) %>%
        group_by(year) %>%
        summarize(sum(emissions)) %>% 
        mutate(year = parse_number(as.character(year)))
names(NEI_4)<- c("year", "emissions")

ggplot_2<- ggplot(data = NEI_4, aes(year, emissions))+
        geom_point(color = NEI_4$year, size = 4)+
        geom_smooth(method = "lm", se = FALSE, size = 0.5)+
        labs(x = "Year", y = expression(PM[2.5] * " Emissions (Tons)"), 
             title = "Total PM2.5 Emission from motor vehicle sources in Baltimore City by Year")+
        geom_text(data = NULL, label = round(NEI_4$emissions, digits = 2), nudge_y = 10) +
        scale_x_continuous(breaks=c(1999:2008), labels=c(1999:2008))

png(file = "plot5.png", width = 640, height = 480)
ggplot_2
dev.off()
