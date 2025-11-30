
#30DayChartChallenge 2022 #Day19 - Global Change

rm(list=ls())


path="/Users/federica/Documents/R/R_general_resources/EDA/30DayChartChallenge/data/Edition_2022/day18_oecd"
setwd(path)


library(tidyverse)
airtransport <- read_csv("AIRTRANS_CO2_19042022233714776.csv")

options(scipen = 999)
df <- airtransport%>%
  janitor::clean_names()%>% #count(flight_type)
  select(pollutant,country,time,value,frequency=frequency_2,flight_type)%>%
  mutate(value=round(value))

df1 <-df %>% 
  count(country,time,value)%>%
  filter(time==as.integer(substr(time, 1, 4)))%>%
  distinct()

df2%>%DataExplorer::profile_missing()

 
  
  
  my_countries <- df1 %>%
  group_by(country)%>%
  summarize(tot_value_per_country=round(mean(value)))%>%
  arrange(-tot_value_per_country)%>%
  ungroup()%>%
  slice(1:10)%>%
  select(country)%>%
  unlist()

  
  

  
  df2 <- df1 %>%
  #filter(country%in%my_countries) %>%
  group_by(country,time)%>%
  summarise(avg_value=mean(value),.groups="drop")%>%
  arrange(-avg_value) %>%
  ungroup() 
  

  
  
df3 <- df2%>%filter(time==2020)


df3 <-df2%>%filter(!str_detect(country,"Monaco"))

# it's Monaco
  
library(hrbrthemes)
library(ggthemes)  
library(ggthemr)
library(tvthemes)

mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(186)

ggplot(df3,aes(x=factor(time),
               y=avg_value,group=country,color=country,fill=country))+
  geom_area(alpha=0.2)+
  geom_point(size=0.2)+
  geom_line()+
  scale_color_manual(values=mycolors)+
  scale_fill_manual(values=mycolors)+
  labs(x="",y="",
       title="Global CO2 emissions",
       caption="#30DayChartChallenge 2022 #Day19 - Global Change | OECD data | DataViz: Federica Gazzelloni")+
theme_dark()+
  theme(legend.position = "none",
        plot.title = element_text(size=30,color="white"),
        plot.caption = element_text(size=12,color="white"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color="white",size=12),
        axis.ticks.x = element_line(size=6,color="white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color="grey90",size=2),
        plot.background = element_rect(fill="grey20"),
        panel.background = element_rect(fill="grey20"))


ggsave("day19_global_change.png",
       dpi=320,
       width = 9,
       height = 6)




##------------


library(streamgraph)

# Basic stream graph: just give the 3 arguments
streamgraph(df2, 
                  key="country", 
                  value="avg_value", 
                  date="time", 
            offset="zero",
            height="300px", width="1000px")
pp 

