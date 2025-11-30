
# sources: 
# https://www.statista.com/statistics/529312/sweden-number-of-accidental-deaths-by-type-and-gender/
# https://r-charts.com/part-whole/waffle-chart-ggplot2/
# https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext
# https://rud.is/rpubs/building-pictograms.html

# https://stackoverflow.com/questions/56817353/how-to-save-ggrough-chart-as-png


library(tidyverse)
library(waffle)
library(hrbrthemes)
library(cowplot)


# library(extrafont)

#install.packages("rsvg")
#library(rsvg)

#remotes::install_github("rstudio/fontawesome")
#library(fontawesome)

# install_fa_fonts()

# extrafont::font_import()
#extrafont::loadfonts(quiet = TRUE)

# extrafont::fonttable() %>% 
#   as_tibble() %>% 
#   filter(grepl("Aw", FamilyName)) %>% 
#   select(afmfile, FullName, FamilyName, FontName)

# fa_grep("car-crash","dizzy","flushed","fill-drip","frown","frown-open","medrt",
#         "running","procedures","prescription-bottle-alt","pills","skull-crossbones",
#         "sad-tear","swimmer","street-view","water",
#         "walking")
fa_list()


#---------------

df <- gender <- readxl::read_excel("R_general_resources/EDA/30DayChartChallenge_2022/day2_pictogram/statistic_id529312_accidental-deaths-in-sweden-2020-by-type-and-gender.xlsx", 
                           sheet = "Data",skip=1)

df1 <- df[c(-1,-2),]

names(df1) <- c("accidents","men","women")

df2 <-df1 %>%
  mutate(accidents=as.factor(accidents)) %>%
  mutate(men=as.integer(men),
    women=as.integer(women),
    men=round(men/sum(men)*100),
    women=round(women/sum(women)*100)) 



men <- ggplot(df2, aes(label = accidents, values = men)) +
  geom_pictogram(aes(colour = accidents), 
                 n_rows = 5,
                 flip = F, make_proportional = T) +
  scale_color_discrete(name = NULL,type=RColorBrewer::brewer.pal(5, "Set2")) +
  scale_label_pictogram(
    name = NULL,
    values = c(`Drowning accidents` ="swimmer",
               `Falling accidents` ="running",
               `Other accidents` ="procedures",
               `Poisoning` = "skull-crossbones",
               `Traffic accidents` ="car-crash")) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75,
                                   family="Roboto Condensed"),
        legend.position = c(0.5,1.1),
        legend.direction = "horizontal")


women <- ggplot(df2, aes(label = accidents, values = women)) +
  geom_pictogram(aes(colour = accidents), 
                 n_rows = 5,
                 flip = F, make_proportional = T) +
  scale_color_discrete(name = NULL,type=RColorBrewer::brewer.pal(5, "Set2")) +
  scale_label_pictogram(
    name = NULL,
    values = c(`Drowning accidents` ="swimmer",
               `Falling accidents` ="running",
               `Other accidents` ="procedures",
               `Poisoning` = "skull-crossbones",
               `Traffic accidents` ="car-crash")) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75),
        legend.position = "none")



ggdraw()+
  draw_line(x = c(0.15, 0.15),
            y = c(0.75, 0.95),
            color = "blue", size = 7) +
  draw_label("Accidental deaths in Sweden in 2020, by type and gender", 
             x = 0.5, y = 0.93,fontface = "bold",
             fontfamily = "Roboto Condensed",size=27) +
  draw_label("A higher number of men than women died from accidents in Sweden in 2020. \nThe most common accident which led to death that year was falling accidents, \nwhich caused the deaths of 567 men and 492 women.",
  x=0.5, y=0.85)+
  draw_label("Men",angle=90,x=0.12,y=0.6,size=15,fontfamily = "Roboto Condensed")+
  draw_plot(men, scale = 0.88, x = 0, y = 0.05) +
  draw_label("Women",angle=90,x=0.12,y=0.3,size=15,fontfamily = "Roboto Condensed")+
  draw_plot(women, scale = 0.88, x = 0, y = -0.28) +
  draw_label("#30DayChatChalleng 2022 day2 - Pictogram | DataSource: Statista | DataViz: Federica Gazzelloni ", 
             x = 0.5, y = 0.05, fontfamily = "Roboto Condensed",size=12) 








