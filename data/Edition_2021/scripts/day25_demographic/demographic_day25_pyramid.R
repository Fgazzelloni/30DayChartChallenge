

# Demographic Day 25 - Second attempt 
# inspired by: https://github.com/ZainulArifin1/WeeklyPlot/tree/main/Week%202%2030%20DAY%20CHART

library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(RColorBrewer)
library(ggpol)
library(gganimate)
library(gifski)

options(scipen = 999)


# Data are from IHME healthdata.org for European Union countries, 5-year cat 
# Population change 2010-2019
# link to population data: http://ghdx.healthdata.org/record/ihme-data/gbd-2019-population-estimates-1950-2019
#################### population ##################
# data wrangling
population2010 <- read.csv("IHME_GBD_2019_POP_2010_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2010=val)%>%
  select(age_group_name,sex_name,population2010)
population2011 <- read.csv("IHME_GBD_2019_POP_2011_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2011=val)%>%
  select(age_group_name,sex_name,population2011)
population2012 <- read.csv("IHME_GBD_2019_POP_2012_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2012=val)%>%
  select(age_group_name,sex_name,population2012)
population2013 <- read.csv("IHME_GBD_2019_POP_2013_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2013=val)%>%
  select(age_group_name,sex_name,population2013)
population2014 <- read.csv("IHME_GBD_2019_POP_2014_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2014=val)%>%
  select(age_group_name,sex_name,population2014)
population2015 <- read.csv("IHME_GBD_2019_POP_2015_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2015=val)%>%
  select(age_group_name,sex_name,population2015)
population2016 <- read.csv("IHME_GBD_2019_POP_2016_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2016=val)%>%
  select(age_group_name,sex_name,population2016)
population2017 <- read.csv("IHME_GBD_2019_POP_2017_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2017=val)%>%
  select(age_group_name,sex_name,population2017)
population2018 <- read.csv("IHME_GBD_2019_POP_2018_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2018=val)%>%
  select(age_group_name,sex_name,population2018)
population2019 <- read.csv("IHME_GBD_2019_POP_2019_Y2020M10D15.csv")%>%
  filter(age_group_id %in% 5:21)%>%
  filter(location_name == "European Union")%>%
  filter(!sex_name=="both")%>%
  mutate(population2019=val)%>%
  select(age_group_name,sex_name,population2019)


EU_pop <- population2010%>%
  inner_join(population2011,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
inner_join(population2012,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
  inner_join(population2013,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
  inner_join(population2014,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
  inner_join(population2015,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
  inner_join(population2016,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
  inner_join(population2017,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
  inner_join(population2018,by=c("age_group_name","sex_name"))
EU_pop <- EU_pop%>%
  inner_join(population2019,by=c("age_group_name","sex_name"))


EU_pop$age_group_name<-sub("to","-",EU_pop$age_group_name)
EU_pop$age_group_name<-sub("80 plus","80+",EU_pop$age_group_name)
EU_pop$age_group_name<-sub("1 - 4","01 - 04",EU_pop$age_group_name)
EU_pop$age_group_name<-sub("5 - 9","05 - 09",EU_pop$age_group_name)
names(EU_pop)

EU_pop <- EU_pop %>%
  gather(year, pop, 3:12) 


EU_pop$year<-sub("population","",EU_pop$year)
EU_pop<-EU_pop%>%rename(sex=sex_name)
EU_pop<-EU_pop%>%rename(age=age_group_name)
names(EU_pop)

EU_pop <- EU_pop%>%
  mutate(pop = ifelse(sex == 'female', as.integer(pop * -1), as.integer(pop)))

s<-EU_pop%>%filter(age=="80+")


range(EU_pop$pop)

# plotting #####################################################
EU_10_19_Pyramid <- EU_pop %>%
  ggplot(aes(x = age, y = pop, fill = sex)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) +
  scale_y_continuous(breaks = c(-20000000, -10000000, -5000000, 0, 5000000, 10000000, 20000000),
                     label = c("20M", "10M", "5M", "0", "5M", "15M", "20M")) +
  coord_flip() + 
  labs(title = "European Union Population \nchange 2010-2019\n\n{closest_state}",
       subtitle ="",
       y = "Population",
       caption = "@fgazzelloni | Data Source: IHME | Demographic Day25") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "palegreen") ,
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, 'cm'),
        legend.background = element_blank(),
        legend.text = element_text(size = 15,face = 'bold'),
        plot.title = element_text(size = 22,hjust = 0.0,face = 'bold'),
        axis.title.x = element_text(size = 16,face = 'bold'),
        axis.title.y = element_text(size = 16,face = 'bold'),
        axis.text.x = element_text(size = 10,face = 'bold'),
        axis.text.y = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 8,hjust = 1,face = 'bold',color = "#CC6666"),
        plot.margin = margin(10,10,10,10))


EU_10_19_Pyramid <- EU_10_19_Pyramid + 
  transition_states(year,transition_length = 1,state_length = 2) + 
  enter_fade() +
  exit_fade() + 
  ease_aes('cubic-in-out')

animate(EU_10_19_Pyramid,
        fps = 40,duration = 5,width = 1200,height = 1400,res = 120,
        renderer = gifski_renderer('demographic_day25.gif'))

