
capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

# calculate the numbers
capacity%>%
  arrange(-year) %>%
  group_by(type)%>%
  mutate(pct_change = ifelse(round((total_gw/lead(total_gw) - 1) * 100)>100,100,round((total_gw/lead(total_gw) - 1) * 100)))%>%
  ungroup() %>%
  filter(!is.na(pct_change))%>%
  group_by(type)%>%
  summarize(avg_pct_change=round(mean(pct_change)))

average_cost%>% # count(year)
  drop_na() %>%
  pivot_longer(cols=c(2,3,4),names_to="type",values_to="mwh")%>%
  mutate(type=gsub("_mwh","",type)) %>%
  arrange(-year) %>%
  group_by(type) %>%
  mutate(pct_change = ifelse(round((mwh/lead(mwh) - 1) * 100)>100,100,round((mwh/lead(mwh) - 1) * 100)))%>%
  ungroup() %>%
  filter(!is.na(pct_change))%>%
  group_by(type)%>%
  summarize(avg_pct_change=round(mean(pct_change)))


plot1 <- capacity%>% 
  filter(!type=="Storage")%>%
  mutate(tot_cum=cumsum(total_gw))%>%
  arrange(-year) %>%
  group_by(type)%>%
  mutate(pct_change = ifelse(round((total_gw/lead(total_gw) - 1) * 100)>100,100,round((total_gw/lead(total_gw) - 1) * 100)))%>%
  ungroup() %>%
  ggplot(aes(x=factor(year),y=total_gw,fill=factor(year)))+
  geom_col()+
  geom_line(group=1,color="grey70")+
  geom_text(aes(label=pct_change),family="Roboto Condensed")+
  facet_wrap(~type,scales = "free")+
  scale_x_discrete(breaks=seq(2014,2020,2))+
  labs(x="",y="",
       title="Energy Power Sources",
       subtitle = "Berkeley Lab data shows among total gigawatts of powers capacity from 2014 to 2020:\nSolar, Wind and Nuclear increased by 50%, 15%, and 1% respectively while Coal registered 10%\ndecrease, Gas and Other sources decreased by 10%.")+
  ggthemes::scale_fill_hc()+
  ggthemes::theme_hc()+
  theme(text=element_text(family="Roboto Condensed"),
        legend.position = "none",
        plot.title = element_text(face="bold",size=44),
        plot.caption = element_text(hjust=0))


plot2 <- average_cost%>%  #count(year)
  drop_na() %>%
  pivot_longer(cols=c(2,3,4),names_to="type",values_to="mwh")%>%
  mutate(type=gsub("_mwh","",type)) %>% 
  ggplot(aes(x=(year),y=mwh,group=type))+
  geomtextpath::geom_textline(aes(label=type,color=type),hjust=0.05,size=6,family="Roboto Condensed",fontface="bold") +
  scale_x_continuous(breaks=seq(2009,2021,2))+
  ggthemes::scale_color_hc()+
  labs(title="Average cost trend of increasing capacity power sources",
       subtitle="Estimated costs in dollars per megawatt-hour (MWh) 2009-2021",
       x="",y="",
       caption="DataSource: Berkeley Lab | #30DayChartChallenge #Day29 StoryTelling\nDataViz: Federica Gazzelloni | Visualization excluded Storage as power source") +
  ggthemes::theme_hc()+
  theme(text=element_text(family="Roboto Condensed"),
        legend.position = "none",
        plot.title = element_text(face="bold",size=18),
        plot.caption = element_text(hjust=0,size=12),
        axis.line.x = element_line(color="grey70",size=1))+
  annotate("text",x=c(2013,2015,2018),y=c(140,100,80),label=c("-16% Solar","-6% Wind","-7% Gas"),
           size=12,family="Roboto Condensed") +
  annotate("segment", x = 2018, xend = 2018, y = 160, yend = 100,colour = "grey70",size=8,
           arrow = arrow(length = unit(1.5,"cm")))


library(patchwork)

plot1/plot2

ggsave("storytelling.png",
       dpi=320,
       width = 7,
       height = 9)
