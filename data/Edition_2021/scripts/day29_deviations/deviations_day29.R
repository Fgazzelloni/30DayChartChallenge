#  Deviations day29


# load libraries ############################
library(tidyverse)
library(DataExplorer)
library(ggthemes)



# load data ################################
data_all_countries<-read.csv("data_all_countries.csv")

# check and wrangling #######################
head(data_all_countries)
profile_missing(data_all_countries)
plyr::count(data_all_countries$location_name)

# select countries of interest 
my_countries<-c("Madagascar","Bahamas","San Marino")

# and plotting #####################################

deviations_plot <- data_all_countries %>%
  filter(location_name %in% my_countries) %>%
  arrange(location_name) %>%
  group_by(measure_name,location_name) %>%
  summarize(avg_measure=median(val)) %>%
  ungroup() %>%
  mutate(se = sd(avg_measure) / sqrt(length(avg_measure))) %>%

  ggplot(aes(x=measure_name,y=avg_measure,color=location_name)) +
  geom_bar(stat="identity", aes(fill=measure_name), alpha=0.8) +
  geom_errorbar(aes(x=measure_name, ymin=avg_measure-se, 
                     ymax=avg_measure+se),
                 stat = "identity",
                 position = "identity",
                 na.rm = FALSE,
                 orientation = NA,
                 show.legend = NA,
                 inherit.aes = TRUE,
                 width=0.4, 
                 colour="red", 
                 alpha=0.8, size=0.8) +
  labs(title="Global Metrics deviation for 3 selected Countries:\n",
       caption="Viz. @fgazzelloni | Datasource: IHME - healthdata.org | Deviations Day29",
       x="Global Metrics",
       y="Median value per metric",
       color="Country",
       fill="Metric") + 
  scale_x_discrete(labels=c("DALYs (Disability-Adjusted Life Years)" = "DALYs",
                            "Deaths" = "Deaths",
                            "YLDs (Years Lived with Disability)" = "YLDs",
                            "YLLs (Years of Life Lost)" = "YLLs")) +
  facet_wrap(~location_name) +
  annotate("text",x=1,y=0.3, label="deviations",color="red",size=3) +
  annotate("curve", x = 1, xend = 1.2, y = 0.25, yend = 0.35,
           colour = "lightblue") +
  annotate("pointrange", x = 1, y = 0.2, ymin =0.25, ymax = 0.35,
           colour = "red", size = 0.3) +
  theme_economist() +
  theme(axis.text.x = element_text(angle=0),
        axis.title.x = element_text(hjust=0.5,vjust=-1),
        axis.title.y = element_text(hjust=0.5,vjust=1.2),
        plot.title = element_text(size=35),
        plot.margin = margin(7,7,7,7),
        panel.background = element_rect(size=2),
        legend.position = "top",
        legend.title = element_text(face="bold"),
        #legend.key = element_text(),
        legend.box = "vertical" )

######### saving ###############################

ragg::agg_png(here::here("day29_deviations", "deviations_day29.png"),
              res = 320, width = 14, height = 8, units = "in")
deviations_plot

dev.off()

