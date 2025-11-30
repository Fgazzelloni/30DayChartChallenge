library(tidyverse)
bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')
bird_baths <- bird_baths %>%
  drop_na() %>%
  filter(bird_count>0)

bird_baths_type <- bird_baths %>% 
  group_by(bird_type) %>%
  summarise(tot_bird_count = sum(bird_count),.groups="drop") 

bird_baths_type <- column_to_rownames(bird_baths_type, var = "bird_type")


# Compute distances and hierarchical clustering
dd <- dist(scale(bird_baths_type), method = "manhattan")
hc <- hclust(dd, method = "ward.D2")
plot(hc)

library("ape")
colors = RColorBrewer::brewer.pal(12,"Set2")
clus4 = cutree(hc, 12)

#library(extrafont)
#loadfonts()

png(filename = "test.png",  
    width = 25, 
    height = 18, units = "cm", 
    res = 320)
plot(as.phylo(hc), 
     type = "radial", #"fan", #type = "phylogram", 
     show.tip.label = TRUE,
     edge.color = "midnightblue", 
     edge.width = 1, 
     edge.lty = 6,
     use.edge.length = T,
     node.depth = 1,
     node.pos = 2,
     cex = 0.6, 
     label.offset = 0.01,
     no.margin = TRUE,
     tip.color = colors[clus4])
dev.off()


library(extrafont)
library(showtext)
showtext_auto(enable = F)
library(cowplot)


if (requireNamespace("magick", quietly = TRUE)){
  img <- magick::image_transparent(
    magick::image_read("bird-pict.png"),
    color = "white"
  )
  img2 <- magick::image_negate("bird-pict.png")
  p<-ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, 
                linetype = 2, color = "beige") +
    draw_image("test-removebg.png", x = 1.5, y = 1.5, scale = 2.5)+
    draw_image("bird-pict.png" , x = 0.9, y = 1.9, scale = .9) 
    
}


p +
  labs(title="Avian Assemblages at Bird Baths - Australia",
       caption="DataViz: @fgazzelloni\n#TidyTuesday 2021 week36 Bird Baths\n#30DayChartChallenge 2022 #day17 - Connections")+
  theme_void()+
  theme(plot.background = element_rect(color="grey20",fill="grey20"),
        panel.background = element_rect(color="grey20",fill="grey20"),
        plot.title = element_text(family = "Roboto Condensed",
                                  color="beige",size=28,
                                  vjust=-1,hjust = 0.1),
        plot.caption = element_text(family = "Roboto Condensed",
                                    color="beige",size=15,
                                    vjust = 3,hjust = 0.9))
ggsave("bird-network.png",
       dpi=320,
       width = 8,
       height = 8)








