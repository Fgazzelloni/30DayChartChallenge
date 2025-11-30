library(tidyverse)
library(ISLR2)
data(Wage)
Wage %>%
  mutate(education=gsub("\\d. ","",education)) %>% #count(year)
  group_by(education)%>%
  mutate(mean=mean(wage),
         sd=sd(wage)) %>%
  ungroup() %>% # pull(mean)%>%summary
  select(education,mean,sd) %>%
  distinct()%>%
  ggplot(aes(y=fct_reorder(education,mean),
             xdist = distributional::dist_normal(mean, sd),
             layout = "weave",
             fill = stat(x < 111.70))) + 
  ggdist::stat_dots(position = "dodge", color = "grey70")+
  geom_vline(xintercept = 111.70, alpha = 0.25) +
  scale_x_continuous(breaks = c(20,60,90,112,140,180,220)) +
  tvthemes::scale_fill_hilda()+
  labs(x="Wage values from 2003 to 2009",
       y="",color="Race",
       title="Wage distribution vs education 2003-2009",
       subtitle="values are ",
       Caption="#30DayChartChallenge 2022 #day15 - Multivariate\nDataSource: {ISLR2} Wage dataset | DataViz: Federica Gazzelloni")+
  tvthemes::theme_theLastAirbender()+
  theme(legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(0.5,units="cm"),
        legend.direction = "horizontal",
        legend.position = c(0.8,0.1))


