
library(tidyverse)
library(igraph)

library(ggraph)
library(RColorBrewer)
theme_set(silgelib::theme_roboto())

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')
sports <- sports %>%
  filter(!sports=="All Track Combined")

men <- sports %>% #count(state_cd)
  select(year,state_cd,men_sports=sports,partic_men) %>%
  drop_na()



women <- sports %>% #count(state_cd)
  select(year,state_cd,women_sports=sports,partic_women) %>%
  drop_na()


df <- men %>%
  left_join(women,by=c("year","state_cd"))
 

df2 <- distinct(df)


df2_1 <- df2 %>%
  mutate(students=partic_men+partic_women) %>%
  count(men_sports,women_sports,
        wt= students,
        name="students",
        sort=T) %>%
  mutate(students=students/1000) %>%
  filter(students>1000)



df3 <-df2 %>%
  mutate(students=partic_men+partic_women) %>%
  select(men_sports,women_sports,students) 

df2_2<-df2_1 %>%
  graph_from_data_frame(directed = FALSE)

dim(df3)
library(showtext)
library(sysfonts)
library(extrafont)

showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
font_families_google()
font_add_google(name="Noto Sans",family="notosans")
font_add_google(name="Over the Rainbow",family="notosans")

set.seed(1109)
set.seed(0104)
df2_2 %>%
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(edge_alpha = students, 
                    edge_width = students), 
                color = "grey20") +
  geom_node_point(aes(fill=name,color=name),
                  size=15,
                  stroke=4,
                  shape=21,
                  alpha=0.5,
                  show.legend = F) +
  geom_node_point(aes(color=name),size=5,show.legend = F) +
  geom_node_text(mapping = aes(label = name), 
                 position = position_nudge(y = 0.1),
                 #position = position_dodge(width = 0),
                 #position = position_jitter(seed = 111),
                 repel = TRUE, 
                 color="grey15",
                 size=5,
                 family = "notosans",
                fontface="bold")+
  scale_colour_manual(values= rep( brewer.pal(8,"Dark2") , 30)) +
  scale_fill_manual(values= rep(brewer.pal(9,"Paired") , 30)) +
  labs(title="US college Sports 2015-2019",
       subtitle="Among all sports which ones are the most played by males vs females?",
       caption="#30daychartchallenge 2022 - day1 Part to whole\nDataSource: Equity in Athletics Data Analysis | DataViz: Federica Gazzelloni")+
  theme(text=element_text(color="grey15",size=12,family="notosans"),
        plot.background = element_rect(color="grey30",fill="grey80"),
        legend.position = c(0.93,0.9)) 
 
  


ggsave("day1_part-to-whole.png",width =  8.56, height =  8.44)



#scale_size_discrete( range = c(0.1,10) )


geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) )

