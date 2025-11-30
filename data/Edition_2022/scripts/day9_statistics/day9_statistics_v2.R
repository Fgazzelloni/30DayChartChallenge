library(ISLR2)
library(tidyverse)
library(ggdist)
library(distributional)
library(extrafont)
# loadfonts()

p <- Wage %>%
  mutate(education=gsub("\\d. ","",education)) %>% #count(year)
  group_by(education)%>%
  mutate(mean=mean(wage),
         sd=sd(wage)) %>%
  ungroup() %>% # pull(mean)%>%summary
  select(education,mean,sd) %>%
  distinct()%>%
  ggplot(aes(y=fct_reorder(education,mean),
             xdist = dist_normal(mean, sd),
             layout = "weave",
             fill = stat(x < 111.70))) + 
  stat_dots(position = "dodge", color = "grey70")+
  geom_vline(xintercept = 111.70, alpha = 0.25) +
  scale_x_continuous(breaks = c(20,60,90,112,140,180,220)) +
  tvthemes::scale_fill_hilda()+
  labs(x="Wage values from 2003 to 2009",
       y="",color="Race",fill="wage < avg",
       title="Wage distribution vs education 2003-2009",
       subtitle="Normalized values",
       caption="#30DayChartChallenge 2022 #day9 - Distribution/Statistics - v2\nDataSource: {ISLR2} Wage dataset | DataViz: Federica Gazzelloni")+
  tvthemes::theme_avatar()+
  theme(text = element_text(family="Chelsea Market"),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(0.5,units="cm"),
        legend.direction = "horizontal",
        legend.position = c(0.8,0.1))


ggsave("day9_statistics_v2.png",
       dpi=320,
       width = 9,
       height = 6)

library(patchwork)
p/p


ggsave("poster.pdf",
       dpi=320,
       height =841 , width = 594,
       units = "mm")
