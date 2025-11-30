
library(tidytuesdayR)
library(tidyverse)
library(extrafont)


tuesdata <- tidytuesdayR::tt_load(2021, week = 17)
netflix <- tuesdata$netflix


p<-ggplot(netflix, aes(x=type, y=release_year)) +
  geom_tile(aes(fill = duration)) +
  labs(title = "NETFLIX",
       subtitle="shows by type and duration",
       caption="Viz Federica Gazzelloni | DataSource: Kaggle - NETFLIX | Tiles Day 23 & II TidyTuesday W17",
       y = "",
       x="") +
  theme_linedraw()+
  theme(legend.position = "none",
        plot.title = element_text(color="white",face="bold",size=30,family="Trebuchet MS"),
        plot.subtitle = element_text(color="white",face="bold",size=18,family="Trebuchet MS"),
        plot.caption = element_text(color="white",face="bold",size=8,family="Trebuchet MS"),
        
        plot.background=element_rect(fill="navy"),
        axis.text = element_text(color="white",face="bold",size=12,family="Trebuchet MS"))




####### SAVING ######################################
ragg::agg_png(here::here("day23_tiles", "tiles_day23.png"),
              res = 320, width = 14, height = 8, units = "in")
p

dev.off()



#### ATTACHING LOGO ############################ 
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


tiles_day23 <- image_read("day23_tiles/tiles_day23.png")

attached_logo <- image_composite(tiles_day23, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "day23_tiles/tiles_day23.png", format = "png") # save final plot


############################################


