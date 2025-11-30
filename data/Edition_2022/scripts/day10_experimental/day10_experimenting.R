data()
tabplot::tableplot(airquality,
                   select=Ozone)

summary(airquality$Ozone)
length(airquality$Ozone)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(RcmdrPlugin.KMggplot2)


airquality%>%
  mutate(Ozone=ifelse(is.na(Ozone),0,Ozone)) %>%
  ggplot(aes(y=Ozone,x=seq(0,100,0.6535948))) +
  geom_step()+
  RcmdrPlugin.KMggplot2::geom_stepribbon(aes(ymin =0, ymax = Ozone + 0),
                                         fill = "grey70")+
  geom_smooth(se=T)+
  geom_hline(aes(yintercept=mean(Ozone)),
             linetype="dashed")+
  scale_x_reverse(limits=c(100,0))+
  coord_flip() +
  labs(title="Ozone level",
       subtitle = "experimenting RcmdrPlugin.KMggplot2",
       caption="#30DayChartChallenge 2022 day10 - Experimental\nDataSource: airquality dataset from {datasets} | DataViz: Federica Gazzelloni ")+
  tvthemes::theme_brooklyn99()+
  theme(axis.title.y = element_blank())



ggsave("day10_experimenting.png")
