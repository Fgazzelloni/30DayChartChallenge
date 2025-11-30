#

library(tidyverse)
library(ggforce)
library(extrafont)



options(scipen = 999)

# drug use-------------

druguse <- read.csv("multiplier_survey_data.csv")
head(druguse)

plyr::count(druguse$year_start)

library(lubridate)



df<-druguse%>%
  arrange(year_start)%>%
  mutate(sprop=sample_size/sum(druguse$sample_size),
         new_val=val*sprop)%>%
  
  group_by(year_start)%>%
  summarize(avg_val=median(new_val))%>%
  ungroup()

head(df)

dim(df)

library(zoo)

p <- ggplot(data = subset(df,year_start>="1992"),aes(x=year_start,y=avg_val*10^6)) +
  geom_point(aes(size=avg_val,color="red"),alpha=0.5)+
  geom_line(aes(y = rollmean(avg_val*10^6, 3, align = "right", fill = NA)),
            color="red")+
  geom_jitter(alpha=0.3,aes(size=avg_val,fill=year_start))+
  geom_smooth(method="loess",size=0.8,color="grey4")+
  scale_x_continuous(breaks =c(1992,1994,1998,2000,2004,2006,2008,2010,2016)) +
  facet_zoom(x = year_start >= "2004" & year_start <= "2008") + 
  labs(title="Drug use trend in 97 countries between 1992-2017",
       subtitle = "proportion of surveys by year",
       caption="Viz. Federica Gazzelloni | DataSource: IHME | Trend Day26",
       x="Time(Year)",
       y="Precent")+
  theme_minimal()+
  theme(legend.position = "none",
        plot.margin = margin(5,5,5,5),
        plot.background = element_rect(size=25,color="black",fill="black"),
        panel.background = element_rect(size=3,color="white",fill="white"),
        panel.grid.major = element_line(size=0.1,color="grey3"),
        panel.grid.minor = element_line(size=0.1,color="grey3"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(vjust=1,family="Courier New",size=10,face="bold",color="white"),
        axis.ticks = element_line(color="red",size=2),
        axis.title = element_text(face="bold",size=12,family="Courier New",
                                  hjust=0.5,vjust=0.2,color="white"),
        plot.title = element_text(family="Courier New",size=25,color="white"),
        plot.subtitle = element_text(family="Courier New",size=14,color="white"),
        plot.caption = element_text(face="bold",family="Courier New",size=8,color="white")
        ) 
         


####### SAVING ######################################
ragg::agg_png(here::here("day26_trends", "trends_day26.png"),
              res = 320, width = 14, height = 8, units = "in")
p

dev.off()



