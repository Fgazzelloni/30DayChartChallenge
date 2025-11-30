
#30DaychartChallenge 2022 day6 data day

## (1) Setup and loading packages ----
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install.packages("devtools")  # Only needed if you don't have devtools installed
#library(devtools)
#devtools::install_github("drsimonj/ourworldindata")

#library(easypackages)
easypackages::libraries("ourworldindata", "tidyverse", "lubridate", "countrycode", "maps")


#https://drsimonj.svbtle.com/ourworld-an-r-data-package
?ourworldindata

financing_healthcare %>% names()

df <- financing_healthcare %>%
  select(year,country ,contains("insurance")) %>%
  pivot_longer(cols = contains("insurance"),names_to="types",values_to="values") %>%
  mutate(values=ifelse(is.na(values),0,values)) %>%
  group_by(types) %>%
  mutate(values=round(values/sum(values)*100)) %>%
  ungroup() %>%
  arrange(year)

df %>% summary(year)
df %>% count(country)
levels(df$types) <- unique(df$types)

library(waffle)
?waffle()


df2 <- df %>%
  filter(country=="United States") %>%
  filter(year %in% c(1989,1996,2005))


ggplot(df2,aes(fill = factor(types), values = values)) +
geom_waffle(n_rows = 10, size = 0.3, colour = "black", 
            make_proportional = TRUE,
            radius = unit(4, "pt"),
            height = 0.9, width = 0.9) +
facet_wrap(~year) +
  scale_colour_manual(
    values = c("black", "white", "white","white")
  ) +
labs(title="United States financing healthcare in 1989,1996,2005",
       fill="Type of Insurance") +
hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())



df3 <- financing_healthcare %>% 
  filter(!is.na(health_exp_total)) %>%
  group_by(country,year) %>%
  summarize(exp=sum(health_exp_total),.groups="drop") %>%
  ungroup() 

# df3%>%pull(exp)%>%summary()
set.seed(123)
  ggplot(df3, aes(x=factor(year),y=exp,group=country)) +
  geom_point(size=0.2,color="grey10")+
  geom_line(color="grey85",size=0.2) +
  geom_line(data= subset(df3, country=="United States"),
            aes(x=factor(year),y=exp,group=country),
            color="grey52") +
    geom_line(data= subset(df3, country=="Luxembourg"),
              aes(x=factor(year),y=exp,group=country),
              color="grey50") +
    ggrepel::geom_text_repel(data =df3 %>% group_by(country) %>% 
                summarize(year=max(year),exp=max(exp)) %>% ungroup() %>%
                  filter(exp>1000), 
              mapping= aes(x=factor(year),y=exp,group=country,label=country),
              family = "Roboto Condensed",
              nudge_x = 0, max.overlaps = Inf,
              segment.size=0.2,
              segment.color="grey40",
              box.padding = 0.5,
              segment.curvature = -1e-20,
              arrow = arrow(length = unit(0.009, "npc"), type = "closed", ends = "first"))+
    geom_hline(aes(yintercept=mean(exp)))+
    geom_text(aes(x=5,y=900,label="Average: 653.201$"),size=6)+
    scale_x_discrete(breaks=seq(1995,2013,3),expand = expansion(add=c(0,7)))+
    scale_y_continuous(breaks = seq(0,6900,1000))+
    labs(title="World countries comparison of total health expenses:",
         subtitle="from 1995 to 2013 - data are in constant 2011 international dollars.",
         caption="#30DayChartChallenge 2022 day6 - Data day | DataSource: financing_healthcare {ourworldindata} | DataViz: Federica Gazzelloni",
         x="Time (Year)", y="Total Health Expenses") +
    hrbrthemes::theme_ipsum_rc() +
    theme(axis.title.x = element_text(hjust = 0.8,size=10),
          axis.title.y = element_text(size=10),
          plot.title = element_text(size=25),
          plot.subtitle = element_text(size=18))
    
ggsave("day6_data_day.png",dpi = 320,
       width = 12,height = 10)
  



