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

# 4. How have emissions from coal sources changes from 1999 to 2008?

#merge 2 datasets
NEI_2<-left_join(NEI, SCC, by = "scc")

NEI_3<- NEI_2 %>% filter(str_detect(ei_sector, "Fuel Comb.*Coal")) %>%
        group_by(year) %>%
        summarize(sum(emissions)) %>% 
        mutate(year = parse_number(as.character(year)))

names(NEI_3) <- c("year","emissions")

ggplot_1<- ggplot(data = NEI_3, aes(year, emissions))+
        geom_point(color = NEI_3$year, size = 4)+
        geom_smooth(method = "lm", se = FALSE, size = 0.5)+
        labs(x = "Year", y = expression(PM[2.5] * " Emissions (Tons)"), 
             title = "Total Coal Combustion related PM2.5 Emission in the US by Year")+
        geom_text(data = NULL, label = round(NEI_3$emissions, digits = 2), nudge_y = 1e4) +
        scale_x_continuous(breaks=c(1999:2008), labels=c(1999:2008))

png(file = "plot4.png", width = 640, height = 480)
ggplot_1
dev.off()
