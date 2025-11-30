path<- "/Users/federica/Documents/R"
setwd(path)


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
library(ftplottools)

# source('~/Documents/R/R_general_resources/EDA/ft_theme/ft_colors.R')
# source('~/Documents/R/R_general_resources/EDA/ft_theme/ft_theme.R')

edu_exp2_ft<- edu_exp2%>%
  filter(value>0)

class<- c("High income",
          "Middle income",
          "Upper middle income",
          "Low & middle income",
          "Lower middle income",
          "Low income","World")

levels(class)<-class

colors<-ft_o_colors[7:13]
names(colors)<- class



edu_exp2_ft2 <- edu_exp2_ft%>%
  filter(entity%in%class)%>%
  filter(year>1998)

wld_y<- edu_exp2_ft2%>%filter(entity=="High income")%>%select(value)%>%unlist()
wld_x<-edu_exp2_ft2%>%filter(entity=="High income")%>%select(year)%>%unlist()  

ggplot(data= edu_exp2_ft2%>%filter(entity=="World"),
       aes(x=factor(year),y=(value),color=entity,fill=entity,
             group=factor(entity)))+
  geom_line(size=1)+
  geom_line(data= edu_exp2_ft2%>%filter(!entity=="World"),
                size=0.3) +
  geom_point(size=2)+
  geom_segment(aes(yend=0,xend=factor(year)),
               size=4,alpha=0.1,
               inherit.aes = T) +
  geom_segment(aes(yend=wld_y,xend=factor(wld_x)),
               size=4,alpha=0.2,
               inherit.aes = T)+
  scale_y_log10()+
  scale_x_discrete(expand = c(0,0.5),breaks=seq(1970,2020,5)) +
  ftplottools::scale_color_ft()+
  ftplottools::scale_fill_ft()+
  labs(title="Government expenditure on education total (% of GDP) by Income class",
       color="",
       x="Time (Year)",y="Total (% of GDP)",
       caption="#30DayChartChallenge 2022 #Day24 - FT-theme\nDataSource: UNESCO Institute for Statistics via OurWorldInData\n 
       DataViz: Federica Gazzelloni")+
  ftplottools::ft_theme(base_family="Impact",
                        base_size=14,
                        legend_right=F)+
  theme(axis.text.x = element_text(size=12),
        plot.caption = element_text(hjust=0.5))

ggsave("test_ft.png")



  
