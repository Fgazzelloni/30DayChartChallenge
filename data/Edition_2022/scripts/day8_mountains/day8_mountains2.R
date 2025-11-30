# Source for making a mountains.csv dataset

# https://learn.arcgis.com/en/projects/map-the-highest-mountains/
# https://learngis2.maps.arcgis.com/home/user.html?newUser=true

# first step:
  # https://learngis2.maps.arcgis.com/home/content.html

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(streamgraph)

highest_mountains <- read_csv("mountains.csv")
highest_mountains


library(cowplot)
library(hrbrthemes)
library(extrafont)
loadfonts()


ggdraw() +
  draw_image("mountains_map.png")+
  draw_label("Where are the mountains of the world?",
             fontfamily = "Eater",color="grey60",
             size = 28, x=0.5,y=0.95) +
  draw_label("approximate spatial resolution of the global distribution of mountains.",
             fontfamily = "Eater",
             size = 15, x=0.5,y=0.89,color="grey60") +
  draw_label("coursework: https://learn.arcgis.com/en/projects/map-the-highest-mountains",
             fontfamily = "Eater",
             size = 12, x=0.5,y=0.85,color="grey60") +
  draw_label("Map made with ArcGIS:
- basemap: Modern Antique Map\n- layer: World Mountain Ranges\n- Triangles represent the highest mountains: \nMt.Everest, Kangchenjunga, Lhotse, Makala (Himalayas)
             
             \nDataSource: Learn ArcGIS | DataEdit: Federica Gazzelloni",
             x=0.5,y=0.13,fontfamily = "Eater",color="grey60") 


ggsave("day8_mountains.png",
       width = 9.87, height = 9.46,
       bg="grey15",
       dpi=320)


