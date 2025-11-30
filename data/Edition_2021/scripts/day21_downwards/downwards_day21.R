
# http://www.bdlc.umontreal.ca/CHMD/prov/que/que.htm
# https://github.com/ZainulArifin1/WeeklyPlot/tree/main/Week%202%2030%20DAY%20CHART


library(tidyverse)
library(extrafont)

options(scipen = 999)

df <- read.delim("https://www.prdh.umontreal.ca/BDLC/data/que/Exposures_5x1.txt",sep="",skip=1)

#df%>%ggplot(aes(x=Year,y=Total,group=Age,color=factor(Age)))+geom_line()

df$Age<-sub("1-4","01-04",df$Age)
df$Age<-sub("5-9","05-09",df$Age)
df$Age<-sub("905-099","95+",df$Age)

my_df <- df%>%
  arrange(Year,desc(Age))%>%
  filter(!Age=="110+" & !Age=="100-104" & !Age=="105-109" & !Age=="0")%>%
  pivot_longer(cols=c("Female","Male"),names_to="Gender",values_to="Exposure")%>%
  select(-Total)

my_df_plot <- my_df %>%
  ggplot(aes(x = Age, y = Exposure, fill = Gender)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#d07be0","#29ffc6")) +
  scale_y_continuous(
    breaks = c(0, 10000000, 20000000, 30000000, 40000000),
    label = c("0M", "10M", "20M","30M", "40M")) +
    coord_flip(clip = "on") +
    theme(plot.background = element_rect(fill = "#5e5556") ,
          panel.background = element_rect(fill = "#5e5556"),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
   
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
    
    legend.position = "top",
    legend.title = element_text(family="Luminari",size = 15,face = 'bold',color = 'white'),
    legend.key.size = unit(0.75, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(family="Luminari",size = 15,face = 'bold',color = 'white'),
    
    plot.title = element_text(family="Luminari",size = 22,hjust = 0.5,face = 'bold',color = 'white'),
    plot.subtitle = element_text(family="Luminari",size = 12,hjust = 0,face = 'bold',color = 'white'),
    plot.caption = element_text(family="Luminari",size = 10,hjust = 1,face = 'bold',color = "white"),
    
    axis.line.x = element_line(size=2,color="white"),
    axis.title.x = element_text(family="Luminari",size = 16,face = 'bold',color = '#242422'),
    axis.text.x = element_text(family="Luminari",size = 12,face = 'bold', color = 'white'),
    axis.text.y = element_text(family="Luminari",size = 10,face = 'bold',color = 'white'),
    axis.ticks = element_blank(),
    axis.title.y = element_text(family="Luminari",size = 16,face = 'bold',color = '#242422'),
    plot.margin = margin(10,10,10,10)
  )


exposure_plot <- my_df_plot +
  labs(title = "Quebec Exposure-to-risk Change\n\n{1921-2016}",
       subtitle = "Exposure is pivoted downwards by age increasing",
       y = "Exposure-to-risk (Population in millions)", x="Age",
       caption = "Viz @fgazzelloni | Data Source: Canadian Human Mortality Database | Downwards Day21") 


# saving ######################################


ragg::agg_png(here::here("day21_downwards", "downwards_day21.png"),
              res = 320, width = 14, height = 8, units = "in")
exposure_plot

dev.off()





