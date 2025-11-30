# Connections Day 18 --------------------------------------------

# https://web.stanford.edu/class/bios221/book/Chap-Graphs.html
# https://github.com/jtichon/ModernStatsModernBioJGT/tree/master/data
# https://simplemaps.com/data/world-cities


library(ggplot2)
library("rworldmap")
library(igraph)
library(ape)
library(ggnetwork)
library(extrafont)
library(cowplot)


# load data -------------------------------------------
countries <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/countries_codes_and_coordinates.csv"))
countries%>%filter(country=="United States")
world<-read.csv("day18_connections/worldcities.csv")
load("day18_connections/dist2009c.RData")

# manipulations -------------------------------------------
country09 = attr(dist2009c, "Label")
mstree2009 = ape::mst(dist2009c)
gr09 = graph.adjacency(mstree2009, mode = "undirected")


mat = match(country09, countries$country,world$country)

coords2009 = data.frame(
  lat = countries$latitude[mat],
  lon = countries$longitude[mat],
  country = country09)


layoutCoordinates = cbind(
  x = jitter(coords2009$lon, amount = 15),
  y = jitter(coords2009$lat, amount = 8))


labc = names(table(country09)[which(table(country09) > 1)])
matc = match(labc, countries$country,world$country)

dfc = data.frame(
  latc = countries$latitude[matc],
  lonc = countries$longitude[matc],
  labc)

dfctrans = dfc
dfctrans[, 1] = (dfc[,1] + 31) / 93
dfctrans[, 2] = (dfc[,2] + 105) / 238

ggeo09 = ggnetwork(gr09, arrow.gap = 0, layout = layoutCoordinates)

# plotting -----------------------------------------------
g <- ggplot(ggeo09, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", alpha = 0.5, curvature = 0.1) +
  geom_nodes(aes(color = name), size = 2) +
  theme_void() +
  geom_label(data = dfctrans, aes(x = lonc, xend = lonc, y = latc, yend = latc,
                                  label = labc, fill = labc), 
             colour = "white", alpha = 0.5, size = 3) +
  labs(
       caption= "Viz Federica Gazzelloni - DataSource:stanford.edu - Connections Day18") +
  theme(legend.position = "none")
        

plot <- ggdraw(g) +  draw_label(
  "Connections Day18 - Distances in World Countries",
  fontface = 'bold',
  x = 0.1,y=0.99,
  hjust = 0
) +
  theme_cowplot(font_size = 14)
              
# saving ######################################


ragg::agg_png(here::here("day18_connections", "connections_day18.png"),
              res = 320, width = 14, height = 8, units = "in")
plot

dev.off()

##############################################################





