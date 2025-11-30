

# Some inspiration by:
# https://stackoverflow.com/questions/50305206/how-to-normalize-array-between-1-and-10
# https://github.com/danielpballer/Tidy_Tuesday/blob/master/Scripts_for_final_plots/2021_Week_10_Super_Bowl_Ads_Final_Plot.Rmd


library(tidyverse)
library(lubridate)
library(zoo)
library(ggalluvial)
library(igraph)
library(colormap)
library(ggraph)
library(ggtext)
library(extrafont)
library(RColorBrewer)

Covid19_it_reg<- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
names(Covid19_it_reg)

start = 1
end = 10
width = end - start

df<-Covid19_it_reg %>%
  rename(regions="denominazione_regione",ICU="terapia_intensiva",positives="totale_positivi",hospidalized="ricoverati_con_sintomi",
         deaths="deceduti", recovered="dimessi_guariti" ) %>%
  mutate(date=format(as.Date(data),"%Y-%m-%d"),
         month.year=as.yearmon(date))

df_top_ten<-df%>%arrange(desc(deaths))%>%group_by(regions)%>%summarize(deaths.sum=sum(deaths))

top_ten<-df$regions[1:10]
unlist(top_ten)


my_df<-df%>%select(date,regions,positives,deaths,hospidalized,ICU,recovered)%>%
  group_by(regions)%>%
  summarize(positives=round((positives - min(positives))/(max(positives) - min(positives))*width + start),
            deaths=round((deaths - min(deaths))/(max(deaths) - min(deaths))*width + start),
            hospidalized=round((hospidalized - min(hospidalized))/(max(hospidalized) - min(hospidalized))*width + start),
            ICU=round((ICU - min(ICU))/(max(ICU) - min(ICU))*width + start),
            recovered=round((recovered - min(recovered))/(max(recovered) - min(recovered))*width + start))%>%
  pivot_longer(cols=c("positives","deaths","hospidalized","ICU","recovered"),names_to="category",values_to = "values")%>%
  mutate(values=round(values))%>%filter(regions==c("Abruzzo","Basilicata",
                                                   "Calabria","Campania",
                                                   "Emilia-Romagna","Friuli Venezia Giulia",
                                                   "Lazio","Liguria",
                                                   "Lombardia","Marche"))



final_plot = my_df%>%
  ggplot(aes(axis1 = regions, axis2 = category, y = values)) +
  scale_x_discrete(limits = c("regions", "category"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = regions)) +
  scale_fill_brewer(palette="Paired")+
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), family = "Freebooter", size = 4) +
  theme_minimal() +
  ggtitle("Italian Regions by Covid19 outcomes chategory",
          "How do they change in time?") +
  labs(caption = "Viz @fgazzelloni | Datasource:Civil Protection  | Stripes - Day 12") +
  theme_void()+
  theme(plot.title = element_text(hjust = .5, size = 24, family = "Freebooter"),
        plot.subtitle = element_text(hjust = .5,size = 16, family = "Freebooter"),
        plot.caption = element_text(size = 10),
        legend.position = "None")


final_plot


##########################


ragg::agg_png(here::here("day12", "Circular_day12.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()


