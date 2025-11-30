# 3-D
# source: Tidytuesday week 9 
# Employment and Earnings | BLS | BLS Article

library(tidytuesdayR)
library(tidyverse)
library(rayshader)
library(rayrender)
library(sp)
library(scales)
library(raster)
library(DataExplorer)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(extrafont)
fonts()


tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

employed <- tuesdata$employed
earn <- tuesdata$earn

head(employed)
head(earn)

plyr::count(employed$race_gender)
plyr::count(earn$race)


df <- employed %>%
  filter(race_gender==c("Asian","gray31 or African American","White")) %>%
  rename(race=race_gender) %>%
  inner_join(earn,by=c("race","year")) %>%
  select(-employ_n,-ethnic_origin) 

plyr::count(df$industry)
profile_missing(df)


missing_industries <- df %>%
  filter(is.na(industry)) %>%
  select(-industry,-industry_total)


profile_missing(df)
plyr::count(df$ethnic_origin)
plyr::count(missing_industries$minor_occupation)
plyr::count(df$minor_occupation)
plyr::count(df$year)
plyr::count(df$age)


my_df<-df%>%filter(age==c("16 to 24 years","25 to 54 years"))%>%
  rename(earning=median_weekly_earn)%>%
  mutate(earning_prop=earning/n_persons*10000)

range(my_df$earning_prop)
log2(0.1437547)


employment_3D<-ggplot(my_df,aes(x=earning,y=log2(earning_prop)))+
  stat_density_2d(aes(fill=..density..), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c(option = "A") +
  facet_wrap(~race)+
  labs(title="Earning level by race",
       subtitle="Employed persons by industry, sex, race, and occupation\nWeekly earnings data from the Current Population Survey",
       caption="Viz @fgazzelloni | 3D Day30 | Datasource: TidyTuesday w9 \nEmployment and Earnings | BLS | BLS Article ",
       fill="Density",
       x="Earnings(weekly)",
       y="Proportion of normalized Earnings (log2)")+
  theme_base()+
  theme(strip.text = element_text(size=5,face="bold",color="white",family="Comic Sans MS"),
        strip.background = element_rect(color="gray31",fill="darkseagreen4"),
        plot.background = element_rect(color="gray31",fill="darkslateblue"),
        panel.background = element_rect(color="gray31",fill="darkslateblue"),
        legend.background = element_rect(color="gray31",fill="darkseagreen4"),
        legend.title = element_text(color="white",face="bold",family="Comic Sans MS"),
        legend.text = element_text(color="white",family="Comic Sans MS"),
        plot.title = element_text(size=20,color="white",family="Comic Sans MS"),
        plot.subtitle = element_text(size=10,color="orange",family="Comic Sans MS"),
        plot.caption = element_text(size=8,color="white",family="Comic Sans MS"),
        axis.title = element_text(color="white",family="Comic Sans MS"),
        axis.text = element_text(color="white",family="Comic Sans MS"),
        axis.text.x = element_text(size=8,family="Comic Sans MS"),
        axis.text.y = element_text(size=8,family="Comic Sans MS"))

# render plot as a 3D #######################

plot_gg(employment_3D,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
        zoom = 0.55, phi = 30)
render_snapshot("3D_day30")








