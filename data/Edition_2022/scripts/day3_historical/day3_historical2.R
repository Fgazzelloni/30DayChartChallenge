

#30DayChartChallengre 2022 day3 - Historical

# data from: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022
# DuboisChallenge2022 challenge n°8

library(tidyverse)

df <- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge08/data.csv")

library(hrbrthemes)
library(showtext); showtext_auto()
font_add_google("Rajdhani", "rajdhani")
# font_add_google("Titillium Web", "web")
# bg         <- "#e1d1c1" 

df2 <- df %>%
  arrange(-Year)

df2[7,1]  <- 1875
df2[7,2]  <- 0




ch8 <-  df2 %>%  
  ggplot() + 
  
  geom_line(data= subset(df2, Year%in%c(1875,1875)),
            mapping = aes(x=Year, y= `Houshold Value (Dollars)`),
            color="grey50",size=6.3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875)),
            mapping = aes(x=Year, y= `Houshold Value (Dollars)`),
            color="#FFCDCB",size=6) +
  
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880)),
            mapping= aes(x=Year +2, y= `Houshold Value (Dollars)`),
            color="grey50",size=6.3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880)),
            mapping= aes(x=Year +2, y= `Houshold Value (Dollars)`),
            color="#989EB4",size=6) +
  
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885)),
            mapping= aes(x=Year +4, y= `Houshold Value (Dollars)`),
            color="grey50",size=6.3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885)),
            mapping= aes(x=Year +4, y= `Houshold Value (Dollars)`),
            color="#b08c71",size=6) +
  
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890)),
            mapping= aes(x=Year +6, y= `Houshold Value (Dollars)`),
            color="grey50",size=6.3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890)),
            mapping= aes(x=Year +6, y= `Houshold Value (Dollars)`),
            color="#FFC942",size=6) +
  
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890,1895)),
            mapping= aes(x=Year +8, y= `Houshold Value (Dollars)`),
            color="grey50", size=6.3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890,1895)),
            mapping= aes(x=Year +8, y= `Houshold Value (Dollars)`),
            color="#EFDECC", size=6) +
  
  geom_line(mapping= aes(x=Year +10, y= `Houshold Value (Dollars)`),
            color="grey50",size=5.3) +
  geom_line(mapping= aes(x=Year +10, y= `Houshold Value (Dollars)`),
            color="#F02C49",size=5) +
  
  coord_polar(theta = "y",
              start = 0,
              direction = 1, 
              clip = "off") +
  #scale_x_reverse(expand=expansion(mult=c(-0.9,-0.1),add=c(29,-0.1))) +
  #scale_y_continuous(expand=expansion(mult=c(0.09,0.01),add=c(0,-790000))) +
  
  #scale_y_continuous(expand = c(1, 0, 0.05, 1))

  scale_x_reverse(expand=expansion(add=c(11,-1))) +
  scale_y_continuous(expand=expansion(add=c(0,-600000))) +
  labs(title="ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE 
       OWNED BY GEORGIA NEGROES.")+
  theme_void() +
  theme(text = element_text(family="rajdhani",
                            face="bold",
                            color="grey27"),
        aspect.ratio =2/1.95,
        plot.background = element_rect(color= "#d9ccbf", fill= "#d9ccbf"),
        plot.title = element_text(hjust=0.5))

    
library(cowplot)    

final <- ggdraw()+
  draw_plot(ch8) +
  draw_label(label = "
                      1875__________ $ 21,186
                      1880_______ $ 498,532
                      1885________ $ 736,170
                      1890______ $ 1,173,624
                      1895_____ $ 1,322,694
                      1899_____ $ 1,434,975",
             x=0.35,y=0.826,
             size=8.5,fontfamily = "rajdhani")


final

ggsave("test3.png",final)

ragg::agg_png(here::here("test2.png"),
              #scaling=1.5,
              res = 300, 
              width = 6, 
              height = 4, units = "in")
final

dev.off()    
    
