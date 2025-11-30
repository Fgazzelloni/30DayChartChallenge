
# Demographic day25---------------------------

library(tidyverse)
library(ggtext)
library(extrafont)
library(ggf)
library(ggrepel)
library(ggthemes)
library(hrbrthemes)
library(showtext)

options(scipen = 999)

write.rda("pop","pop_2010_2019.rda")

save(pop, file = "mydata.rda")
load(file = "mydata.rda")

data(pop)


my_df<-pop


########### imaging ########################
library(ggpubr)
library(jpeg)

# Download and read sample image (readJPEG doesn't work with urls)
url <- "http://mathworld.wolfram.com/images/gifs/rabbduck.jpg"
download.file(url, destfile = "rabbduck.jpg")
img <- readJPEG("rabbduck.jpg")

ger <- grid::rasterGrob(img, interpolate=TRUE)
library(cowplot)
h <- ggdraw(p)
## tweak this to fit your plot
h + draw_grob(ger, 0.4, 0.48, 0.07, 0.07)

######## plotting ##################################


p <- ggplot(data=my_df,aes(x=Deaths,y=DALY,size=pop_value)) +
  with_blur(
    geom_boxplot(aes(color="grey",fill = after_scale(alpha(colour, 0.4))),
                 size=5,show.legend = FALSE)
  ) + 
  geom_jitter(width = 0.15,height = 1,size=1,alpha=0.7) + 
  geom_text_repel(aes(label=location),max.overlaps=15,size=3,color="red",
                  segment.colour = NA,stat="identity",position = "dodge") + 
  scale_radius(range = c(1,2)) +
  scale_colour_identity() + 
  coord_polar() +
  labs(title="Population disability adj life years (DALY) by Deaths",
       subtitle="EU countries 2010-2019 rate difference",
       caption="Viz Federica Gazzelloni ! DataSource: IHME | Dempgraphic Day 25",
       x="proportion of Deaths",
       y="DALYs",
       size="EU Population",
       alpha="Transparency")+
  theme_ft_rc() + 
  theme(plot.background = element_rect(fill = "wheat"),
        panel.background = element_blank(),
        plot.title=element_text(face="bold",size= 16,family="Comic Sans MS",color="thistle4"),
        plot.subtitle=element_text(face="bold",size= 15,color="thistle4",family="Comic Sans MS"),
        plot.caption=element_text(face="bold",size= 8,color="thistle4",family="Comic Sans MS"),
        panel.grid.major = element_line(color="grey"),
        axis.text.y = element_text(size= 8,color="thistle4",family="Comic Sans MS"),
        axis.text.x = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size= 8,color="thistle4",family="Comic Sans MS"),
        legend.title = element_text(size= 8,color="thistle4",family="Comic Sans MS"))


p



####### SAVING ######################################
ragg::agg_png(here::here("day25_demographic", "demographic_day25.png"),
              res = 320, width = 14, height = 8, units = "in")
p

dev.off()