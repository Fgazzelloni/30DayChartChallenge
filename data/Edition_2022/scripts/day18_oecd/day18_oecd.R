
#30DayChartChallenge 2022 day18 - OECD

# Source:
  
# - https://www.oecd-ilibrary.org/environment/data/oecd-environment-statistics/air-transport-co2-emissions_13d4f295-en
# - https://stats.oecd.org/viewhtml.aspx?datasetcode=AIRTRANS_CO2&lang=en


rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
airtransport <- read_csv("AIRTRANS_CO2_19042022233714776.csv")

options(scipen = 999)
df <- airtransport%>%
  janitor::clean_names()%>% #count(flight_type)
  select(pollutant,country,time,value,frequency=frequency_2,flight_type)%>%
  mutate(value=round(value))


df_yr <- df %>%
  filter(time==as.integer(substr(time, 1, 4)))%>%
  distinct()


# count(country)
my_countries <- df_yr %>%
  group_by(country)%>%
  summarize(tot_value_per_country=round(mean(value)))%>%
  arrange(-tot_value_per_country)%>%
  ungroup()%>%
  slice(1:10)%>%
  select(country)%>%
  unlist()

df2 <- df_yr %>%
  filter(country%in%my_countries)

library(extrafont)
# loadfonts()
library(hrbrthemes)
library(tvthemes)
library(ggthemes)
library(geomtextpath)




df3 <- df2 %>%
  group_by(time,country)%>%
  mutate(avg_value=mean(value)) %>%
  ungroup()



df3 %>%
  filter(!country%in%c("Canada","Japan")) %>% 
  ggplot(aes(x=factor(time),y=avg_value,label=country,group=country))+
  geom_hline(aes(yintercept=12268195),linetype="dashed",size=2,color="grey90")+
  geom_textline(aes(color=country),
                size=4,
                fontface="bold",
                linewidth = 2,
                show.legend = F)+
  scale_y_log10(labels=scales::label_number(scale = 1e-6))+
  labs(title="CO2 emissions related to \ncommercial passenger, freight, and general aviation flights",
       caption="#30DayChartChallenge 2022 #Day18 - OECD
DataSource: OECD - International Civil Aviation Organisation (ICAO)
       Top CO2 level Countries - tonnes of CO2-equivalent | DataViz: Federica Gazzelloni")+
  annotate("segment", x = 0.8, xend = 0.8, y = 55000000, yend = 85000000, 
           colour = "darkred", size=6, alpha=0.6)+
  coord_cartesian(ylim = c(2690700, 46048382), 
                  xlim = c(1.5,8.5),clip = "off")+
  tvthemes::scale_color_attackOnTitan()+
  ggthemes::theme_fivethirtyeight()+
  theme(legend.position = "none",
        axis.text = element_text(face="bold",size=10),
        plot.caption = element_text(vjust=0.5),
        plot.background = element_rect(fill="grey85",color="grey85"),
        panel.background = element_rect(fill="grey85",color="grey85"))


ggsave("day18_oecd.png",
       dpi=320,
       width = 9,
       height = 6)

