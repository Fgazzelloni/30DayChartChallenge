# Abstract Day10 -----------

# Inspired by Hierarchical edge bundling and wanting to know more about 
# visualizing clustering
# source: https://www.r-graph-gallery.com/hierarchical-edge-bundling.html

###########################################################

# General-purpose data wrangling
library(tidyverse)  
# Parsing of HTML/XML files  
library(rvest)    
# String manipulation
library(stringr)   
# Verbose regular expressions
library(rebus)     
# Eases DateTime manipulation
library(lubridate)

require(stats); require(graphics)


# Table extracted from: https://www.worldometers.info/geography/alphabetical-list-of-countries/

url <- "https://www.worldometers.info/geography/alphabetical-list-of-countries/"

table_world_countries <-xml2::read_html(url)

wcl<-table_world_countries%>%
  rvest::html_node("table") %>% 
  rvest::html_table(header = TRUE)%>%
  select(2:5)

wcl <- as.tibble(wcl)

# sum(is.na(wcl))

wcl$`Population(2020)`<-gsub(",","",wcl$`Population(2020)`)
wcl$`Population(2020)`<-as.numeric(as.character(wcl$`Population(2020)`))
wcl$`Land Area (Km²)`<-gsub(",","",wcl$`Land Area (Km²)`)
wcl$`Land Area (Km²)`<-as.numeric(as.character(wcl$`Land Area (Km²)`))
wcl$`Density(P/Km²)`<-gsub(",","",wcl$`Density(P/Km²)`)
wcl$`Density(P/Km²)`<-as.numeric(as.character(wcl$`Density(P/Km²)`))


names(wcl)[3] <- gsub(" ","",names(wcl)[3])

names(wcl)<-c("country","population","area","density")
glimpse(wcl)

wcl <- transform(wcl, country = factor(country))

clean_wcl <- wcl%>%arrange(population)%>%
  mutate(area=round(area,2),area_made=round(population/density,2))%>%
  select(1,2,5,4)

head(clean_wcl)

summary(clean_wcl$population)

table(percent_rank(clean_wcl$country),clean_wcl$country)

kclust <- kmeans(clean_wcl[,2:3],centers=5)  
table(kclust$cluster,clean_wcl$country)
############################################ PCA #############################

rid<-seq(1,length(clean_wcl$country),1)
class(rid)

df_wcl<- cbind(rid,clean_wcl)

df <-df_wcl%>%mutate(dens_log=log10(density))%>%
  select(country,rid,dens_log)%>%
  mutate(rid = paste0("rid_", rid))%>%
  pivot_wider(names_from = "rid", values_from = "dens_log", values_fill = 0)


library(recipes)

pca_rec <- recipe(~., data = df) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 5)

pca_prep <- prep(pca_rec)

pca_prep

library(extrafont)
library(ggrepel)
bake(pca_prep, new_data = NULL) %>%
  ggplot(aes(PC1, PC2, label = country)) +
  geom_point(color = "midnightblue", alpha = 0.4, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "Comic Sans MS") +
  labs(color = NULL)

#------------

pca_comps <- tidy(pca_prep, 2) %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  left_join(df_wcl %>% mutate(terms = paste0("rid_", rid))) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup()


pca_comps %>%
  mutate(value = abs(value)) %>%
  ggplot(aes(value, fct_reorder(terms, value), fill = country)) +
  geom_col(position = "dodge") +
  geom_text(check_overlap = TRUE, hjust = "inward", aes(value, fct_reorder(terms, value),label= country),family = "Comic Sans MS")+
  facet_wrap(~component, scales = "free_y") +
  labs(
    x = "Absolute population density",
    y = NULL, fill = NULL,
    title = "What World Countries are the most diverse in density?",
    subtitle = "Seychelles, Antigua and Barbuda and San Marino are different"
  ) +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank()
        
        )

library(embed)

umap_rec <- recipe(~., data = df) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

umap_prep

image <- readJPEG("background-world-map-1.jpg")
library(ggpubr)
library(jpeg)
library(ggimage)
library(magick)

main_plot<-bake(umap_prep, new_data = NULL) %>%
  ggplot(aes(umap_1, umap_2, label = country)) +
  background_image(image) + 
  geom_point(color = "midnightblue", alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "Comic Sans MS",size=5) +
  labs(color = NULL)+
  theme_void()+
  theme(plot.background = element_blank(),
        plot.margin = margin(5,5,5,5))




#-----------------------


library(patchwork)

final <- main_plot + plot_annotation(
  title = "World Countries driven by population density",
    subtitle = "What else they have in common?",
  caption = "Visualization: Federica Gazzelloni | Abstract Day10 | Data:Worldometers",
  theme = theme(plot.title = element_text(size=24,family="Comic Sans MS",face = "bold"),
                plot.subtitle = element_text(size=14,family="Comic Sans MS"),
                plot.margin = margin(10,10,10,10),
                plot.background = element_rect(fill = "#663fd9e2" , color = NA),
                plot.caption = element_text(family = "Comic Sans MS", size = 9, 
                                color =  "midnightblue", margin = margin(15,0,0,0), hjust = 0.95)
  )
)

ragg::agg_png(here::here("day10", "Abstract_day10.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()

###########################################################################

