# animals day8
library(devtools)
library(tidyverse)
library(forecast)
library(ggridges)

##################################

# remotes::install_github("BiologicalRecordsCentre/BRCindicators")
# vignette('BRCindicators')
library(BRCindicators)

data(bats)

glimpse(bats)

plyr::count(bats$species)

df <- bats%>%filter(species=="lshorse")
df

bats$year<-as.factor(bats$year)
bats$index[is.na(bats$index)]=0
bats$collated_index[is.na(bats$collated_index)]=0
summary(bats$index)



library(paletteer)
library(pals)

animals_plot <- ggplot(data = bats, aes(x=collated_index,y=species,fill = species))+
  geom_density_ridges_gradient(jittered_points = TRUE, scale = 2,
                               point_size = 0.8, size = 0.5,col="#99D8C9"
                               )+
  labs(x="Index", y="Bats species",
       title="How different Bats' species evolve in density?",
       subtitle="Bats are one of the most likely species of whild animals involved with Covid19 Outbreak",
       caption="Viz Federica Gazzelloni - DataSource:BRCindicators - Day8\n ")+
  theme_minimal()+
  theme(text=element_text(family="Arial"),
        plot.title = element_text(size=34),
        plot.subtitle = element_text(size=20),
        plot.caption = element_text(size=8,face="bold"),
        legend.position = "none")




##########################################
ragg::agg_png(here::here("day8", "Animals_day8.png"),
              res = 320, width = 14, height = 8, units = "in")
animals_plot

dev.off()









