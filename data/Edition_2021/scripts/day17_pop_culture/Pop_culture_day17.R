
# Pop Culture day 17 - Data for IHME latest Covid19 projections ----------------

# load libraries ---------------------------

library(viridis)
library(hexbin)
library(tidyverse)

# load data <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

# latest IHME covid19 projections -------------------------------
library(downloader)
url="https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip"
download(url, dest="ihme-covid19_latest.zip", mode="wb") 
unzip("ihme-covid19_latest.zip")

# select data sets of interest ------------------------------
df <- read.csv("2021-04-16/best_masks_hospitalization_all_locs.csv")
df3 <- read.csv("2021-04-16/worse_hospitalization_all_locs.csv")


# manipulations -----------------------------------------

my_df_mask <- df%>% select(date,location_name,
                           confirmed_infections,
                           #mobility_composite,
                           total_pop)

my_df_mandate_easing <- df3%>% select(date,location_name,
                                      confirmed_infections)

my_df <- my_df_mask%>% left_join(my_df_mandate_easing,by=c("date","location_name"))

names(my_df)<-c("date","location","infections_UM","population","infections_ME")

my_df_global <-my_df %>% filter(location =="Global")

UM_norm<-rnorm(my_df_global$infections_UM)
ME_norm<-rnorm(my_df_global$infections_ME)


# plotting ----------------------------------------------------

# inspired by:
# http://sape.inf.usi.ch/quick-reference/ggplot2/coord


library(extrafont)
base_family="Arial Rounded MT Bold"
base_size=12
half_line <- base_size/2



main_plot <- ggplot(data.frame(x = UM_norm, y = ME_norm), 
                    aes(x = x, y = y)) +
  geom_hex() + 
  coord_fixed() + 
  scale_fill_identity() + 
  labs(title = "Global Covid19 Infections \nUniversal Mask vs Mandate Easing",
       caption = "Viz Federica Gazzelloni - DataSource: IHME Covid19 latest projections - Pop Culture day 17",
       x = "Universal Mask - Infections projection",
       y = "Mandate Easing - Infections projection") +
  theme_void() + 
  theme(line = element_line(colour = "grey85", size = 0.4, linetype = 1, lineend = "round"), 
        rect = element_rect(fill = "gray88", colour = "grey85", size = 2, linetype = 1), 
        text = element_text(family = base_family, face = "plain", colour = "white", size = base_size, 
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),  
                            debug = FALSE), 
        
        axis.line = element_line(colour = "black", size = 0.4, linetype = 1, lineend = "butt"), 
        axis.text = element_text(size = base_size * 1.1, colour = "black"), 
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1), 
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
        axis.ticks = element_line(colour = "gray94", size = 1.3), 
        axis.ticks.length = unit(half_line, "pt"),
        axis.title = element_text(colour = "red"),
        axis.title.x = element_text(margin = unit(c(3.5, 0, 0, 0), "mm"), 
                                    vjust = 1, size = base_size * 1.3, face = "bold"), 
        axis.title.y = element_text(angle = 90, margin = unit(c(0, 3.5, 0, 0), "mm"), 
                                    vjust = 1, size = base_size * 1.3, face = "bold"), 
        
        panel.background = element_rect(fill = "red", colour = NA), 
        panel.border = element_rect(colour = "grey71", fill = NA, size =4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(colour = "gray70"), 
        plot.title = element_text(color="black",size = base_size * 1.5, hjust = 0, vjust = 0, 
                                  face = "bold", 
                                  margin = margin(b = half_line * 1),family=base_family), 
        plot.subtitle = element_text(color="black",size = 8, hjust = 0, vjust = 0, 
                                     margin = margin(b = half_line * 0.9)), 
        plot.caption = element_text(size = 8, hjust = 1, vjust = 1, 
                                    margin = margin(t = half_line * 0.9), color = "purple"), 
        plot.margin = margin(15,15,15,15))

# saving ######################################


ragg::agg_png(here::here("day17_pop_culture", "Pop_culture_day17.png"),
              res = 320, width = 14, height = 8, units = "in")
main_plot

dev.off()

