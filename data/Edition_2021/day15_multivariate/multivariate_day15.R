# Multivariate day 15 ####################################

# load libraries ############################

library(GGally)
library(medflex)
library(patchwork)
library(extrafont)
library(xkcd)


# load data & plotting #####################################

data(UPBdata, package = "medflex")

main_plot <- ggpairs(
  UPBdata[, c("age", "initiator", "gender", "negaff")],
  aes(colour=gender,alpha=0.7),
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")
) + 
  ggtitle("Relationships") + 
  labs(subtitle="Strategies for the Estimation of Natural Direct and Indirect Effects of bad relationships\n",
       caption="Multivariate Behavioral Research, Viz Federica Gazzelloni Datasource: R Package medflex Multivariate Day 15")+
  theme_void() + 
  theme(plot.title = element_text(family="xkcd",size=24),
        plot.subtitle = element_text(family="xkcd",size=14,color="darkred"),
        plot.caption = element_text(family="xkcd"),
        strip.text.y =  element_text(family="xkcd",size=9),
        strip.text.x = element_text(family="xkcd",size=9),
        strip.background = element_blank(),
        axis.text.x = element_text(family="xkcd",size=8),
        plot.margin = margin(5,8,5,8),
        plot.background = element_rect(color = "darkred",size=4))


# saving ######################################


ragg::agg_png(here::here("day15_multivariate", "Multivariate_day15.png"),
              res = 320, width = 14, height = 8, units = "in")
main_plot

dev.off()



