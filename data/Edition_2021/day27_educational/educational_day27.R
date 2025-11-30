# educational day27 -----------

# data : Our World in Data
# https://ourworldindata.org/educational-mobility-inequality

library(tidyverse)
library(showtext)
library(extrafont)
library(ggtext)
library(ggrepel)
library(ggExtra)

library(DataExplorer)
#profile_missing(edu)

edu_unlist<- unlist(read.csv("share-of-women-15-years-and-older-with-no-education.csv"))
edu<- read.csv("share-of-women-15-years-and-older-with-no-education.csv")
head(edu)
names(edu)<-c("country","code","year","percentage")
plyr::count(edu$country)


df <- edu%>%
  group_by(country)%>%
  mutate(abs_change=abs(c(0,diff(percentage))),
         rel_change=ifelse(percentage==0,0,abs_change/percentage*100))%>%
  ungroup()
  
abs_ch_70 <- edu%>%
  group_by(country)%>%
  filter(year=="1970")

abs_ch_10 <- edu%>%
  group_by(country)%>%
  filter(year=="2010")

abs_ch_70_10<- abs_ch_70%>%
  inner_join(abs_ch_10,by=c("country","code"))%>%
  mutate(abs_ch_70_10=percentage.y-percentage.x,
         rel_ch_70_10=abs_ch_70_10/percentage.x*100)
  

##### plotting #################
#font_info_google()
#font_families()
font_add_google("Oxygen Mono",family="Oxygen Mono")
showtext_opts(dpi = 250)
showtext_auto(enable = TRUE)

squaredplot <-  ggplot(data=df,
         mapping=aes(x=year,y=rel_change,group=country,color=country,fill=country)) + 
  geom_bin2d()+
  geom_text_repel(aes(label=country),max.overlaps = 2,vjust=-1,size=6,
                  nudge_x = 0.5,segment.colour = NA) +
  labs(title="Girls Education",
       subtitle = "relative change by country (1970 - 2010)\n",
       caption="Viz. Federica Gazzelloni | Data: OurWorldinData | Educational Day27",
       tag="\nWorld countries with the highest change\n")+
  theme_void()+
  ylim(c(0,1000))+
  theme(legend.position = "none",
        plot.tag = element_text(color="white",size=18,family="Oxygen Mono"),
        plot.tag.position = c(0.2, 0.80),
        plot.title = element_text(color="white",size=40,family="Oxygen Mono",face="bold"),
        plot.subtitle = element_text(color="white",size=25,family="Oxygen Mono"),
        plot.caption = element_text(color="white",family="Oxygen Mono",size=10),
        plot.background = element_rect(color="black",fill="black"),
        panel.background = element_rect(color="black",fill="black"),
        axis.text = element_text(color="white",family="Oxygen Mono"),
        axis.text.x = element_text(color="white",family="Oxygen Mono",size=8),
        axis.text.y = element_text(color="white",family="Oxygen Mono",size=8),
        panel.grid = element_blank(),
        plot.margin = margin(10,10,10,10))


squaredplot


# saving ######################################


ragg::agg_png(here::here("day27_educational", "educational_day27.png"),
              res = 320, width = 14, height = 8, units = "in")
squaredplot

dev.off()
