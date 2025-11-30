

# Monochrome Day24 

library(tidyverse)

# datasource is: IHME https://healthdata.org/

population_2010_2019_long <- population_2010_2019%>%
  pivot_longer(cols=3:12,names_to="population_10_19",values_to="pop_value")%>%
  rename(location=location_name)


df_cause_and_pop <- bubble_df%>%
  inner_join(population_2010_2019_long,by="location")%>%
  select(1,3:7,10)

# load more libraries-----------------

library(ggfx)
library(gridExtra)
library(ggrepel)
library(extrafont)
options(scipen = 999)

# plotting -------------------------------

p<-ggplot(data=df_cause_and_pop,aes(x=log10((YLL+YLD)/DALY),y=log10(DALY),size=pop_value)) +
  geom_jitter(aes(size=pop_value,alpha=0.5),width = 0.15,height = 1) +
  geom_text(aes(label=cause),check_overlap = TRUE)+
  coord_polar() +
  scale_color_identity() +
  labs(title="European Countries",
       subtitle="DALYs by Cause years 2010-2019",
       caption="Viz Federica Gazzelloni ! DataSource: IHME | Monochromo Day 24",
       x="proportion of DALY components",
       y="DALYs",
       size="Population",
       alpha="Transparency")+
  theme_grey() + 
  theme(plot.background = element_rect(fill = "black"),
        plot.title=element_text(face="bold",size= 25,family="Impact",color="white"),
        plot.subtitle=element_text(face="bold",size= 15,color="white",family="Impact"),
        plot.caption=element_text(face="bold",size= 8,color="white",family="Impact"),
        panel.grid.major = element_line(color="grey"),
        axis.text.y = element_text(size= 8,color="white",family="Impact"),
        axis.text.x = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size= 8,color="white",family="Impact"),
        legend.title = element_text(size= 8,color="white",family="Impact"))


####### SAVING ######################################
ragg::agg_png(here::here("day24_monochromo", "monochrome_day24.png"),
              res = 320, width = 14, height = 8, units = "in")
p

dev.off()



