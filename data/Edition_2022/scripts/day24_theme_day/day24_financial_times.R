path<- "/Users/federica/Documents/R"
setwd(path)

# sources: https://raw.githubusercontent.com/johnburnmurdoch/johnburnmurdoch.github.io/master/slides/r-ggplot/r-scripts.R?

library(tidyverse)
edu_exp <- read_csv("total-government-expenditure-on-education-gdp.csv")

edu_exp1 <- edu_exp%>%
  janitor::clean_names()%>%
  rename(value=government_expenditure_on_education_total_percent_of_gdp)

edu_exp2 <- complete(edu_exp1, entity,year,fill=list(value=0))

options(scipen = 999)


# edu_exp2_ft <- edu_exp2%>%
#   mutate(year2=cut(year,4))%>%
#   group_by(entity,year2)%>%
#   summarize(tot=sum(value))%>%
#   ungroup()%>%
#   arrange(year2)

# remotes::install_github("Financial-Times/ftplottools")
# library(ftplottools)

# source('~/Documents/R/R_general_resources/EDA/ft_theme/ft_colors.R')
# source('~/Documents/R/R_general_resources/EDA/ft_theme/ft_theme.R')

edu_exp2_ft<- edu_exp2%>%
  filter(value>0)

class<- rev(c("World","High income",
          "Middle income",
          "Upper middle income",
          "Low & middle income",
          "Lower middle income",
          "Low income"))

levels(class)<-class

# colors<-ft_o_colors[7:13]
#names(colors)<- class



edu_exp2_ft2 <- edu_exp2_ft%>%
  filter(entity%in%class)%>%
  filter(year>1998 & year<=2017)

wld_y<- edu_exp2_ft2%>%filter(entity=="High income")%>%select(value)%>%unlist()
wld_x<-edu_exp2_ft2%>%filter(entity=="High income")%>%select(year)%>%unlist()  


library(extrafont)
library(showtext)
sysfonts::font_add_google("Zen Antique Soft","zas")

showtext.auto(enable = T)
showtext_opts(dpi = 320)

library(geomtextpath)

edu_exp2_ft2%>%summary

p <-ggplot(data= edu_exp2_ft2%>%filter(entity=="World"),
       aes(x=factor(year),y=(value),color=entity,
             group=factor(entity),label=entity))+
  #geom_line(size=1,key_glyph = "timeseries")+
  geom_textline(size=6,key_glyph = "timeseries",
                fontface="bold",#hjust=0,
                linewidth = 2,
                show.legend = F)+
  geom_line(data= edu_exp2_ft2%>%filter(!entity=="World"),
                size=0.5,key_glyph = "timeseries",show.legend = T) +
  geom_line(data= edu_exp2_ft2%>%filter(entity=="High income"),
            size=1,key_glyph = "timeseries",show.legend = T) +
  #geom_point(size=2,show.legend = F)+
  geom_segment(aes(yend=0,xend=factor(year)),
               size=4,alpha=0.1,
               inherit.aes = T,show.legend = F) +
  geom_segment(aes(yend=wld_y,xend=factor(wld_x)),
               size=4,alpha=0.4,
               inherit.aes = T,show.legend = F)+
  geom_segment(aes(y= 7, yend=8,
                   x=19, xend=19),
               size=10,alpha=0.4,
               inherit.aes = T,show.legend = F)+
  geom_segment(aes(y= 5.8, yend=6.3,
                   x=16.8, xend=16.8),
               size=40,
               inherit.aes = T,show.legend = F)+
  scale_y_log10()+
  scale_x_discrete(expand = c(0,0.5),breaks=seq(1999,2017,3)) +
  scale_color_manual(breaks=class,
                     values=scales::brewer_pal('qual',2)(7))+
  guides(colour = guide_legend(nrow = 2))+
  coord_cartesian(clip = "off",ylim = c(2.646,5.370))+
  labs(title="The Gap between High-income countries and the World AVG",
       subtitle="on Government expenditure on education total(% GDP)",
       color="",
       x="Time (Year)",y="Total (% of GDP)",
       caption="\n#30DayChartChallenge 2022 #Day24 - theme day: Fiancial Times\nDataSource: UNESCO Institute for Statistics via OurWorldInData\n 
       DataViz: Federica Gazzelloni |Twitter: @fgazzelloni")+
  ftplottools::ft_theme(base_size=16,
                        legend_right=F)+
  theme(text = element_text(family="zas"),
        axis.text.x = element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(vjust=0.5,hjust=1,size=11),
        plot.background = element_rect(fill="#fff1e0",color="#fff1e0"),
        panel.background = element_rect(fill="#fff1e0",color="#fff1e0"),
        legend.box.background = element_blank(),
        legend.text = element_text(size=12,hjust=-0.02))

library(cowplot)

ggdraw(p)+
  draw_label("Financial Times", fontfamily = "zas",
             x=0.15,y=0.08)

ggsave("test_ft5.png",
       dpi=320,
      height = 6.22 , 
      width =  8.51)



  
