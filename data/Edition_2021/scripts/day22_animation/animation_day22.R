library(tidyverse)
library(ggpol)
library(gganimate)
library(gifski)
library(extrafont)

options(scipen = 999)

#Data input and transformation
df_backup <- read.delim("https://www.prdh.umontreal.ca/BDLC/data/bco/E0per.txt",sep="",skip=1)


df <- df_backup%>%
  pivot_longer(cols=c("Female","Male"),names_to="Gender",values_to="life_expectancy")



le_plot <- df%>%
  ggplot(aes(x=Year,y=life_expectancy,group=Gender,color=factor(Gender))) + 
  geom_point(show.legend = FALSE, alpha = 0.7) +
  geom_line() +
  labs(title = "British Columbia Life Expectancy at birth",
       subtitle = "from 1921 to 2016 | Animation Day22",
       caption = "Viz Federica Gazzelloni | Data Source: Canadian Human Mortality Database",
       y = "Life Expectancy", x="Time(Year)",
       color="Gender") +
  
  scale_fill_manual(values = c('#ff7129', '#d3b4d0')) +
  #scale_y_continuous(breaks = c(-3000000, -6000000, -9000000, 0, 3000000, 6000000, 9000000),label = c("3M", "6M", "9M", "0", "3M", "6M", "9M")) +
  theme_light() +
    theme(plot.background = element_rect(fill = "#edf7fa") ,
          panel.background = element_rect(fill = "#edf7fa"),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          
          legend.position = "top",
          legend.title = element_text(family="Georgia",size = 15,face = 'bold',color = '#035770'),
          legend.key.size = unit(0.75, 'cm'),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(family="Georgia",size = 15,face = 'bold',color = '#035770'),
          
          plot.title = element_text(family="Georgia",size = 20,hjust = 0.5,face = 'bold',color = "#035770"),
          plot.subtitle = element_text(family="Georgia",size = 12,hjust = 0,face = 'bold',color = "#035770"),
          plot.caption = element_text(family="Georgia",size = 10,hjust = 1,face = 'bold',color = "#035770"),
          
          axis.line.x = element_line(size=2,color="#035770"),
          axis.title.x = element_text(family="Georgia",size = 16,face = 'bold',color = "#035770"),
          axis.text.x = element_text(family="Georgia",size = 10,face = 'bold', color =  "#035770"),
          axis.text.y = element_text(family="Georgia",size = 10,face = 'bold',color =  "#035770"),
          axis.ticks = element_blank(),
          axis.title.y = element_text(family="Georgia",size = 16,face = 'bold',color =  "#035770"),
          plot.margin = margin(10,10,10,10)
    )
  
  

final <-le_plot + 
  transition_reveal(life_expectancy) +
  shadow_wake(wake_length = 0.1, alpha = FALSE)



animate(final,gifski_renderer("day22_animation/animation_day22.gif"))




anim_save(animate(final, nframes = 151, width = 9, height = 6.5, units = "in", res = 300), 
          filename = "day22_animation/animation_day22.gif",
          end_pause = 10, rewind = F, fps = 15)












