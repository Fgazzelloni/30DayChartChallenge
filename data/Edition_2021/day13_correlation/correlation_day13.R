
###### correlation day 13 #################################
# Inspired by: TidyTuesday 2019/35 - Simpsons Guest Stars by Wikipedia - author: "Cedric Scherer"
# data for Stats for pub health program created  by imperial college




# load libraries-----------------------

library(tidyverse)
library(ggtext)
library(patchwork)
library(ggcorrplot)
library(extrafont)


theme_update(rect = element_rect(color = NA,
                                 fill = "#a9f51d"),
             line = element_blank(),
             text = element_text(color = "black"))

################# MANIPULATION ############################

data<- read.csv(file="cancer data for MOOC 1.csv", header = TRUE, sep = ',')
sum(is.na(data))
data[is.na(data)]<-0

data$fruitveg<- data$fruit + data$veg
names(data)<-c("patient_id","Age","Gender","BMI","Smoking","Exercise",
               "Fruit","Veg","Cancer","Fruit & Veg")

corr_df<-data%>%select(-1)

corr<-cor(corr_df)
corr2<-cor(corr_df[c(-6,-7)])

####################### PLOTTING ######################
correlation <- ggcorrplot(corr,method="circle")+
  ggtitle(label="Cancer and contributing factors",
  subtitle="Causation is multifactorial, and provide examples of modification of the risk factors through prevention, \nunderstanding of population cancer aetiology interaction effects between risk factors and joint factors.\n'Evaluating intrinsic and non-intrinsic cancer risk factors' 2018\n(Wu, S., Zhu, W., Thompson, P. et al.)")+

  theme(plot.title = element_text(family="Courier New",size=14,face="bold",color="#460046"),
        plot.subtitle = element_text(family="Courier New",size=11),
        axis.text = element_text(size=11,family="Courier New",face="bold"),
    legend.title = element_text(size=8,family="Courier New"),
    legend.text = element_text(size=8,family="Courier New"))
    #plot.margin = margin(5,5,5,5))


correlation2<-ggcorrplot(corr2,hc.order = TRUE, type = "lower",
                         outline.col = "white",lab = TRUE)+
  theme(legend.position = "none",
        axis.text = element_text(size=11,family="Courier New",face="bold"))

main_plot <-correlation + correlation2

################### WIGGETTING ############################
labels <-
  tibble(
    labels = c(
      "<img src='exercise-png-Transparent-Images.png'
    +     width='100' /><br><b style='color:#00947E'>Exercise</b><br><i style='color:#00947E'>13%</i></b>",
      "<img src='fruitveg.png'
    +     width='90' /><br><b style='color:#FF5180'>Fruit & Veg</b><br><i style='color:#FF5180'>-25%</i></b>",
      "<img src='obesity.png'
    +     width='90' /><br><b style='color:#FF5180'>BMI</b><br><i style='color:#FF5180'>-12%</i></b>",
      "<img src='gender.png'
    +     width='90' /><br><b style='color:#FF5180'>Gender</b><br><i style='color:#FF5180'>20%</i></b>",
      "<img src='smoking.png'
    +     width='90' /><br><b style='color:#FF5180'>Smoking</b><br><i style='color:#FF5180'>30%</i></b>"
    ),
    x = 1:5,
    y = rep(1, 5)
  )

legend <-
  ggplot(labels, aes(x, y)) +
  geom_richtext(aes(label = labels),
                fill = NA,
                color = NA,
                vjust = 0) +
  annotate("text", x = 3.5, y = 1.018,
           label = "Correlated Risk factors",
           size = 15,
           fontface = "bold",
           family = "Courier New") +

  scale_x_continuous(limits = c(0.6, 6.1)) +
  scale_y_continuous(limits =  c(1, 1.02)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#87cf80"))

########################### CAPTIONING #####################################

caption <-
  ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       caption = "Viz @fgazzelloni| Source: Imperial College data Mooc | Correlation Day 13")+
  theme(line = element_blank(),
        plot.caption = element_text(size=8, family="Courier New",color="#460046",face="bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",color = "transparent"),
        panel.border = element_rect(color = "transparent"),
        axis.text = element_blank())


###################### SAVING ############################

final <- legend + main_plot + caption + plot_layout(ncol = 1,heights = c(0.6, 1, 0))

ragg::agg_png(here::here("day13_correlation", "Correlation_day13.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()


