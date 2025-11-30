
# Physics day6 ----------------------------

# title:  How soil structure and soil hydraulic behavior change through time?

# ternary diagram for soil type classification - open project

# sources: 
 # https://essd.copernicus.org/articles/12/3189/2020/
 # https://yutannihilation.github.io/allYourFigureAreBelongToUs/3/
 # https://cran.r-project.org/web/packages/soilphysics/soilphysics.pdf

# data:
 # https://dataservices.gfz-potsdam.de/panmetaworks/showshort.php?id=escidoc:5146891

library(tidyverse)
library(ggtern)
library(ggthemes)
library(gridExtra)
library(extrafont)

# install.packages("soilphysics")
library(soilphysics)

ls("package:soilphysics")

data("SoilAggregate")
data("bulkDensity") # observations of soil bulk density and soil moisture
data("compaction") # physical soil variables related to soil compaction.
data("skp1994")

# h = matrix
# w = volumetric water content
# pr = penetration resistance
# bd = bulk density 


data(skp1994)



ternchart <- ggtern(data=skp1994,aes(W,BD,PR)) + 
  geom_point(alpha=0.4,size=2,color="turquoise") + 
  labs(x="volumetric water content (m3/m3)",
       y="soil bulk density (Mg/m3)",
       z="penetration resistance (MPa)", #soil penetration resistance values (MPa).
       title="How water content and penetration resistance behave",
       subtitle = "Least Limiting Water Range ternchart data 1994 - Viz Federica Gazzelloni - Datasource: Soilphysics\nto determine the soil critical moisture and the maximum bulk density"
       
       ) + 
  theme_rgbw() +
  theme_hidetitles() +
  theme(plot.title = element_text(family="Comic Sans MS",size=24),
        plot.subtitle = element_text(family="Comic Sans MS",size=10),
        plot.margin = margin(10,0,0,0)
        )


#----- save the plot ----------------------------------
##########################################
ragg::agg_png(here::here("day7", "Physics_day7.png"),
              res = 320, width = 14, height = 8, units = "in")
ternchart

dev.off()


