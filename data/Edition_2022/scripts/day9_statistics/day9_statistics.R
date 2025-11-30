rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------  
# https://www.sciencebase.gov/catalog/item/60ba5a00d34e86b9388d86bc

library(tidyverse) 
frog <- read_csv("Oregon_spotted_frog_telemetry_at_Crane_Prairie_OR.csv", 
                 col_names = FALSE,
                 trim_ws = FALSE,
                 skip = 2)

names(frog) <- frog[1,]
frog <- frog[-1,]

glimpse(frog)

frog2 <- frog[,-1]

 

frog3 <- frog2 %>% #count(Frequency)%>%View
  mutate(Frequency=gsub("164.","0.",Frequency),
         Frequency=gsub("165.","1.",Frequency),
         Frequency=as.numeric(Frequency),
         Frequency=round(Frequency,2),
         SurveyDate=as.Date(SurveyDate,"%m/%d/%Y"),
         ymon=zoo::as.yearmon(SurveyDate),.after=SurveyDate,
         ymon_c=as.character(ymon),
         Gender=case_when(Female==1~"Female",TRUE~"Male"),
         Gender=as.factor(Gender),
         Day=Ordinal) %>% 
  select(SurveyDate,ymon,ymon_c,Day,Gender,Frequency,Type,Structure,Substrate) %>% 
  group_by(Gender) %>%
  mutate(avg_freq_gender=mean(Frequency),.after=Frequency) %>%
  ungroup()

frog3 %>%
  ggplot(aes(x=SurveyDate,y=Frequency)) +
  geom_point(aes(color=Gender)) 
  #geom_line(aes(group=Day)) +
  geom_







#------
library(ggstatsplot)
df <- ggstatsplot::bugs_long
df%>%glimpse

ggstatsplot::ggbarstats(df,gender,region)

ggstatsplot::ggbarstats(frog3,ymon,Frequency)

#-------

tabplot::tableplot(frog3,
                   select = c(ymon_c,
                              Day,
                              Gender,
                              avg_freq_gender,
                              Frequency,
                              Type,
                              Structure,
                              Substrate))

set.seed(123)
tabplot::tableplot(frog3, 
                   sortCol=ymon_c, 
                   #pals = list()
                   from=0,to=100,
                   decreasing=F,
                   nBins=40,
                   scales="auto",
                   IQR_bias=T,
                   max_levels=10,
                   numMode = "MB",
                   # bias_brokenX=-10,
                   title="Spotted Frog",
                   fontsize.title = 24,
                   fontsize=10,
                   colorNA=NA,
                   legend.lines = 3,
                   showNumAxes = F,
                   rotateNames = 0,
                   relative = T) 

#---------
data()
tabplot::tableplot(airquality,
                   select=Ozone)

summary(airquality$Ozone)
length(airquality$Ozone)

library(RcmdrPlugin.KMggplot2)


airquality%>%
  mutate(Ozone=ifelse(is.na(Ozone),0,Ozone)) %>%
  #pull(Ozone) %>% summary()
  ggplot(aes(y=Ozone,x=seq(0,100,0.6535948))) +
  geom_step()+
  RcmdrPlugin.KMggplot2::geom_stepribbon(aes(ymin =0, 
                                             ymax = Ozone + 0),
                                         fill = "grey70")+
  geom_smooth(se=T)+
  geom_hline(aes(yintercept=mean(Ozone)),
             linetype="dashed")+
  scale_x_reverse(limits=c(100,0))+
  #scale_y_reverse()+
  coord_flip() +
  labs(title="Ozone level",
       subtitle = "experimenting RcmdrPlugin.KMggplot2")+
 tvthemes::theme_brooklyn99()+
   theme(axis.title.y = element_blank())



ggsave("test.png")

library(cowplot)
ggdraw()+
  draw_image("Rplot.png",scale=1) +
  draw_label("ciao",x=0.5,y=0.5)

#--------



ggstatsplot::ggbarstats(data=frog3,
                        x=ymon,y=Frequency,
                        # counts=NULL,
                        type="bayes",
                        paired=T,
                        results.subtitle=F,
                       #  label.args=
                        proportion.test=F,
                        #bf.message=T,
                        #ratio = c(0.5,0.5),
                        sampling.plan="poisson",
                        title="FROG",
                        subtitle="spotted",
                        caption="DataViz:Federica Gazzelloni",
                        legend.title = "Time(Year-Month)",
                        xlab=NA,
                        ggtheme=ggthemes::theme_fivethirtyeight(),
                        package="LaCroixColoR",
                        palette="MelonPomelo")
ggsave("statistics.png")


#------

library(extrafont)
loadfonts()
library(showtext)
showtext.auto()
showtext::showtext_end()



p <- frog3 %>%
  ggplot(aes(y=Frequency,fill="red"))+
  geom_density(color="grey30",size=1.5) +
  #geom_hline(aes(yintercept=0.48))+
  coord_flip() +
  viridis::scale_fill_viridis(discrete = T) +
  scale_y_continuous(breaks = seq(0.17,1.50,0.266))+
  labs(title="FROG SPOTTED BY FREQUENCY",
       subtitle="Rana Pretiosa",
       caption="30DayChartChallenge 2022 day9-Statistics\nDataSource:www.sciencebase.gov\nDataViz: Federica Gazzelloni",
       x="Density") +
  tvthemes::theme_parksAndRecLight()+
  theme(text = element_text(family="xkcd"),
    legend.position = "none")

#median(frog3$Frequency)
plot(density(frog3$Frequency))

p1 <-frog3 %>%
  ggplot(aes(x=factor(ymon),y=Frequency,fill=ymon_c))+
  geom_violin(color="grey30",size=0.2)+
  geom_jitter(size=0.5,show.legend = F,shape=21,stroke=0.2) +
  geom_hline(aes(yintercept=median(Frequency)),linetype="dashed",size=1,color="grey70")+
  viridis::scale_fill_viridis(option = "D",discrete = T) +
  labs(fill="Observation period") +
  tvthemes::theme_parksAndRecLight()+
  theme(text = element_text(family="xkcd"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position=c(-0.5,0.85),
        legend.background = element_rect(fill="grey80"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.key.size = unit(9,units = "pt"),
        panel.grid.major = element_line(size=0.3),
        panel.grid.minor = element_line(size=0.3))

library(GGally)
p2 <- frog3 %>%
  ggplot()+
  aes(x = Structure, fill = Gender, weight = Frequency, by = factor(Structure)) +
  geom_bar(position = "fill",color="grey30",size=0.2) +
  geom_text(stat = "prop", position = position_fill(.5),size=2,color="grey50",
            family="xkcd")+ # GGally
  viridis::scale_fill_viridis(discrete = T) +
  coord_cartesian()+
  tvthemes::theme_parksAndRecLight()+
  theme(text = element_text(color="grey50",family="xkcd"),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=0,size=3.5),
        axis.text.y = element_blank(),
        legend.position=c(-0.35,0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.background = element_rect(fill="grey80"),
        legend.key.size = unit(9,units = "pt"),
        panel.grid.major = element_line(size=0.3),
        panel.grid.minor = element_line(size=0.3))

library(cowplot)

range(frog3$Frequency)
ggdraw()+
  draw_plot(p)+
  draw_plot(p1,scale=0.3, x=0.3,y=0.29,height = 0.99,width = 1.05)+
  draw_plot(p2,scale=0.3, x=0.3,y=0,height = 0.99,width = 1.05) +
  draw_label("Frequencies are adjusted \nto be from 0.17 to 1.50",x=0.2,y=0.05,fontfamily = "xkcd")

ggsave("day9_statistics.png")




