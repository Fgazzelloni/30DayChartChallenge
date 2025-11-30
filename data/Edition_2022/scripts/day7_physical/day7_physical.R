
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# sources:
# https://ourworldindata.org/grapher/violence-against-children?country=~OWID_WRL

library(tidyverse)
df <- read.csv("violence-against-children.csv",header = T)

df %>% View()

# quote = "Proportion.of.children.aged.1-14.years.who.experienced.physical.punishment.and.or.psychological.aggression.by.caregivers"
# quote <- gsub("\\."," ",title)


names(df)[4] <- "proportion"

df1 <- df %>% 
  janitor::clean_names() 
  
df1 %>%count(entity)
library(ggtext)
library(MetBrewer)


# density(df1$proportion)

df1 %>% 
  filter(!code=="") %>%
  arrange(proportion) %>%
  ggplot(aes(x=proportion))+
  geom_density(aes(fill="red"),size=1) + 
  geom_text(aes(x=proportion, 
                y=(seq(8e-03,3.2e-02,0.0003809524)),  # y=(seq(1.613e-05,3.322e-02,0.0005270456)),
                label=entity,size=rev(proportion)),
            #position = position_dodge(width=0.1),
            #position = position_jitter(seed = 123,width = 0.1),
            position = "identity",
            family="Roboto Condensed",
            check_overlap = T)+
  
  # annotate("text", x = 70, y =-0.001,
  #          size=3.5,
  #          family="Roboto Condensed",angle=0,
  #          color="black",
  #          label="#30DayCharChallenge 2022 day6 - Physical DataSource: ourworldindata.org | DataViz: Federica Gazzelloni") +
  # 
  # these values can be retrieved with: density(df1$proportion) + adjustments
  # summary(df1$proportion)
  annotate("point", x=41.4,y=4.5e-03,size=3) +
  annotate("point", x=94.00,y=18e-03,size=3) +
  
  annotate("text",x=42.4,y=3e-03,label="41.4%",
           size=7,
           family="Roboto Condensed") +
  annotate("text", x=94.00,y=16e-03,label="94%",
           size=7,
           family="Roboto Condensed") +
  
  # extra point
  annotate("point", x=67.80,y=1.83e-02,size=3) +
  annotate("text", x=67.80,y=2e-02,label="67.80%",
           size=7,
           family="Roboto Condensed") +

  annotate("point", x=86.6,y=3.1e-02,size=3) +
  annotate("text", x=82,y=3.1e-02,label="86.6%",
           size=7,
           family="Roboto Condensed") +
  
  # annotate("text", x=50,y=3.1e-02,
  #          label=":\n69 Countries around the World",
  #          size=7,
  #          family="Roboto Condensed") +
  # 
  geom_richtext(aes( x = 70 , y = 0.002),
                size=8, family="Roboto Condensed",
                label="Protect Me with Love and Care",
                fill = NA, label.color = NA) +
  
# bounds of the text
  # annotate("point", x=41.4,y=8e-03) +
  # annotate("point", x=86.72,y=3.2e-02,color="green") + # (3.2e-02 - 8e-03)/63

  geom_richtext(aes( x = 52 , y = 0.03),
                size=10, family="Roboto Condensed",
                fill = NA, label.color = NA,
                label="Violence against children") +
  geom_richtext(aes( x = 55 , y = 0.028),
               size=10, family="Roboto Condensed",
               label="from 2008 to 2020",
               fill = NA, label.color = NA) +
 
  
  geom_richtext(aes( x = 55 , y = 0.024),
                size=5, family="Roboto Condensed",
                label="Proportion of children aged 1-14 years<br>who experienced physical punishment<br>and or psychological aggression<br>by caregivers in last month",
                fill = NA, label.color = NA)+
  scale_fill_manual(values=met.brewer("Morgenstern", 6)) +
  scale_x_continuous(breaks = seq(41.4,94,8.6)) +
  scale_y_continuous(expand = c(0.001,0.0001)) +
  labs(caption="#30DayCharChallenge 2022 day6 - Physical \nDataSource: Data from multiple sources compiled by the UN & ourworldindata.org \n DataViz: Federica Gazzelloni") +
  theme_void() +
  theme(plot.background = element_rect(fill=met.brewer("Morgenstern", 1),
                                       color=met.brewer("Morgenstern", 1)),
        panel.background = element_rect(fill=met.brewer("Morgenstern", 1),
                                        color=met.brewer("Morgenstern", 1),
                                        size=0),
        plot.margin = margin(0,0,0,0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.text.x = element_text(color="black",vjust = 0,family="Roboto Condensed"),
        # axis.text.y = element_blank(),
        # axis.ticks.x = element_line(size=2),
        # axis.line.x = element_line(size=1,
        #                            arrow = arrow(type='closed', length = unit(5,'pt'))),
        # axis.ticks.y = element_blank(),
        # axis.title.y = element_blank(),
        # axis.title.x = element_blank(),
        plot.caption = element_text(size = 9,
                                    family="Roboto Condensed",hjust = 0.5),
        plot.caption.position = "panel",
        legend.position = "none") 


ggsave("day7_physical3.png",
       width = 9,
      height =  8,
       dpi = 320)


dev.off()

df1 %>% count(year)
  filter(!code=="") %>%
  ggplot(aes(x=factor(year),y=entity))+
  geom_hex(aes(fill=entity)) +
  theme(legend.position = "none")


