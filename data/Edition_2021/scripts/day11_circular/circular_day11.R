# circular day 11-----------

# Libraries
library(tidyverse)
library(lubridate)
library(extrafont)

  
############################### REGIONS #####################  

Covid19_it_reg<- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
names(Covid19_it_reg)

df_reg <- Covid19_it_reg %>%
  rename(regions="denominazione_regione",
         deaths="deceduti") %>%
  mutate(date=format(as.Date(data),"%Y-%m-%d")) %>%
  select(date,regions,deaths)%>%
 group_by(date) %>%
  summarize(deaths.sum=sum(deaths)) %>% ungroup() %>%
  mutate(deaths.new=c(0,diff(deaths.sum)),
         month=month(date),
         year=year(date),
         month.year=paste(month,year,sep="_"))

########################################################

# Make the plot
final <- ggplot(df_reg, aes(x=as.factor(month.year), y=log(deaths.new),
                   fill=month.year,labels=deaths.sum))+ 
  geom_bar(stat="identity") +
  labs(title="Covid19 - Italy \nNew deaths per month",
       subtitle="Normalized with log(deaths)",
       caption="Viz @fgazzelloni | Datasource: Civil Protection | Circular Day 11",
       x="Time (month.year)",y="Normalized deaths")+
  ylim(-10,10) +
  theme_minimal() +
  theme(plot.title = element_text(size=28,family="Arial Black"),
        plot.subtitle = element_text(size=12,family="Arial Black"),
    legend.position = "none",
        axis.text.x.top = element_text(size=3),
    axis.text = element_blank(),
    axis.text.x = element_text(size=8,family="Arial Black",angle=0,
                               vjust = -2,hjust=-5),
    #axis.text.y = element_text(vjust = 2,hjust=5),
    axis.title = element_text(size=8,family="Arial Black"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#663fd9e2" , color = NA),
    plot.margin = unit(rep(1,4), "cm") 
  ) +
  coord_polar(start=0) 
  

##########################


ragg::agg_png(here::here("day11", "Circular_day11.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()









