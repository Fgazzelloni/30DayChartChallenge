#30DayChartChallenge 2022 day11 - Circular
# Author: Federica Gazzelloni

# Quarterly Earnings per Johnson & Johnson Share
library(tidyverse)
# data()
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(extrafont)
# loadfonts()

# transform time series dataset into a dataframe class
res <-data.frame(value=as.matrix(JohnsonJohnson), date=time(JohnsonJohnson))
res$year <- trunc(res$date)
res$month <- (res$date - res$year) * 12 + 1

# summarize by total annual earnings
label_data <- res %>% 
  arrange(year) %>%
  group_by(year) %>% summarise(tot_val=sum(value)) %>% ungroup()

# set the labels
label_data$id <- seq(1, nrow(label_data))
number_of_bars <- nrow(label_data)
angle <- 90 - 360*(label_data$id - 0.5)/number_of_bars
label_data$hjust <-ifelse(angle< -90,1,0)
label_data$angle<- ifelse(angle< -90, angle + 180, angle)
label_data$label <- paste("$",label_data$tot_val,"in",label_data$year)

# set the colors
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(21)
mycolors2 <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(1627)

# make the circular barplot
p <- res %>% 
  ggplot(aes(x=as.factor(year),y=value,fill=alpha(month,0.7)))+
  geom_bar(stat="identity",
           width = 0.7,
           show.legend = F) +
  geom_text(data = label_data,
            aes(label=label,x=as.factor(year), y=tot_val+1,
                vjust="center",hjust=hjust,angle=angle),
            color="grey90",
            fontface="bold",
            size=4,
            family="Roboto Condensed",
            inherit.aes = F)+
  ylim(-30,60)+
  coord_polar(start = 0,theta = "x",direction = 1) +
  scale_fill_manual(values=mycolors) +
  theme_void() +
  theme(text = element_text(family = "Roboto Condensed"),
        aspect.ratio = 2/2,
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm"))

# make the map
world <- map_data("world")

world%>%count(group)

map <-world %>% #count(group)%>%dim()
  filter(!region=="Antarctica") %>%
  ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=region),show.legend = F) +
  scale_fill_manual(values=mycolors2) +
  coord_map("ortho",orientation=c(14.707540, -74.452362,0))+
  ggthemes::theme_map()+
  theme(text = element_text(family = "Roboto Condensed"))

# draw the plot
library(cowplot)

ggdraw()+
  draw_plot(p,scale=0.9,y=-0.1) +
  draw_plot(map,scale=0.28,x=-0.005,y=-0.1)+
  draw_label("Johnson & Johnson: Quarterly earnings (dollars) per share",
             x=0.5,y=0.95,size=28,color="grey90",
             fontface = "bold",
             fontfamily = "Roboto Condensed") +
  draw_label("summarized by total annual earnings from 1960 to 1980", 
             x=0.5,y=0.9, size=20,color="grey90",
             fontfamily = "Roboto Condensed",
             fontface = "bold") +
  draw_label("#30DayChartChallenge 2022 day11 - Circular\nDataSource:Johnson & Johnson dataset in {datasets}\nDataViz: Federica Gazzelloni",
             x=0.5,y=0.05,size=15,color="grey90",
             fontface = "bold",
             fontfamily = "Roboto Condensed")

# save the plot
ggsave("day11_circular.png",
       bg="grey20",
       width = 10,height = 10)  
 
  
  