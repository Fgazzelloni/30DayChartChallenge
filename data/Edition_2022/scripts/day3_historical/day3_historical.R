

#30DayChartChallengre 2022 day3 - Historical

# data from: https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022
# DuboisChallenge2022 challenge n°8

library(tidyverse)

df <- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge08/data.csv")

library(hrbrthemes)

df2 <- df %>%
  arrange(-Year) %>%
  mutate(y_id=rev(seq(1,6,1))) 

# df2 %>%
#   ggplot() + 
#   geom_segment(aes(x=1875, xend= 1880, y= 0, yend= 21186),color="red") +
#   geom_segment(aes(x=1880, xend= 1885, y= 0, yend= 498532),color="green") +
#   geom_segment(aes(x=1885, xend= 1890, y= 0, yend= 736170),color="blue") +
#   coord_polar()
                 
  
df2[7,1]  <- 1875
df2[7,2]  <- 0
df2[7,3]  <- 0


  
df2 %>%
  ggplot() + 
  geom_line(data= subset(df2, Year%in%c(1875,1875)),
            mapping = aes(x=Year, y= `Houshold Value (Dollars)`),
            color="pink",size=3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880)),
            mapping= aes(x=Year +1, y= `Houshold Value (Dollars)`),
            color="blue",size=3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885)),
    mapping= aes(x=Year +2, y= `Houshold Value (Dollars)`),
    color="grey50",size=3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890)),
    mapping= aes(x=Year +3, y= `Houshold Value (Dollars)`),
    color="orange",size=3) +
  geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890,1895)),
    mapping= aes(x=Year +4, y= `Houshold Value (Dollars)`),
    color="beige",size=3) +
  geom_line(mapping= aes(x=Year +5, y= `Houshold Value (Dollars)`),
            color="red",size=3) +
  coord_polar(theta = "y",
              start = 0,
              direction = 1, 
              clip = "on") +
  scale_x_reverse() +
  theme_void() +
 # theme(aspect.ratio = 1/2)
  theme(aspect.ratio = 9 / 16)


  ylim(0, NA)
  scale_y_continuous(limits = c(0,2300000))
#---------------------
  
  
  dubois_pal_ch8 <- c("pink"="#ffc0cb",
                  "blue"="#4682b4",
                  "brown" = "#654321",
                  "gold" = "#ffd700",
                  "tan" = "#d2b48c",
                  "red"="#dc143c")

 df2 %>%
    ggplot() + 
    geom_line(data= subset(df2, Year%in%c(1875,1875)),
              mapping = aes(x=Year, y= `Houshold Value (Dollars)`),
              color="#FFCDCB",size=6) +
    geom_line(data= subset(df2, Year%in%c(1875,1875,1880)),
              mapping= aes(x=Year +2, y= `Houshold Value (Dollars)`),
              color="#989EB4",size=6) +
    geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885)),
              mapping= aes(x=Year +4, y= `Houshold Value (Dollars)`),
              color="#b08c71",size=6) +
    geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890)),
              mapping= aes(x=Year +6, y= `Houshold Value (Dollars)`),
              color="#FFC942",size=6) +
    geom_line(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890,1895)),
              mapping= aes(x=Year +8, y= `Houshold Value (Dollars)`),
              color="#EFDECC", size=6) +
    geom_line(mapping= aes(x=Year +10, y= `Houshold Value (Dollars)`),
              color="#F02C49",size=6) +
    coord_polar(theta = "y",
                start = 0,
                direction = 1, 
                clip = "off") +
    scale_x_reverse(expand=expansion(add=c(11,-5))) +
   scale_y_continuous(expand=expansion(add=c(0,-600000))) +
   theme_void() +
   theme(aspect.ratio = 1)
   
 #theme(aspect.ratio = 4 / 3)
    #theme(aspect.ratio = 1/1.1)
    
 #library(cowplot) 

 
 final <-ggdraw()+
   draw_plot(p,scale=1.2)
 
 #ggsave("test.png",width = 6, height = 6)
 

dev.off()  
  

ragg::agg_png(here::here("test2.png"),
              scaling=1.5,
              res = 320, 
              width = 6, 
              height = 4, units = "in")
final

dev.off()
  
  

df2 %>%
  mutate(`Houshold Value (Dollars)`=scale(`Houshold Value (Dollars)`)) %>%
 mutate(label=rev(c("1875_____ $ 0",
   "1875_____ $ 21,186",
        "1880____ $ 498,532",
        "1885____ $ 736,170",
        "1890__ $ 1,173,624",
        "1895__ $ 1,322,694",
        "1899__ $ 1,434,975"))) 

p <- df2 %>%
   ggplot() + 
  geom_path(data= subset(df2, Year%in%c(1875,1875)),
            mapping = aes(x=Year, y= `Houshold Value (Dollars)`),
            color="#FFCDCB",size=3) +
  geom_path(data= subset(df2, Year%in%c(1875,1875,1880)),
            mapping= aes(x=Year +2, y= `Houshold Value (Dollars)`),
            color="#989EB4",size=3) +
  geom_path(data= subset(df2, Year%in%c(1875,1875,1880,1885)),
            mapping= aes(x=Year +4, y= `Houshold Value (Dollars)`),
            color="#b08c71",size=3) +
  geom_path(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890)),
            mapping= aes(x=Year +6, y= `Houshold Value (Dollars)`),
            color="#FFC942",size=3) +
  geom_path(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890,1895)),
            mapping= aes(x=Year +8, y= `Houshold Value (Dollars)`),
            color="#EFDECC",size=3) +
  geom_path(data= subset(df2, Year%in%c(1875,1875,1880,1885,1890,1895,1899)),
            mapping= aes(x=Year +10, y= `Houshold Value (Dollars)`),
            color="#F02C49",size=3) +
  coord_polar(theta = "y",
              start = 0.5,
              direction = 1, 
              clip = "on") +
  scale_x_reverse(expand=expansion(mult=0,add=c(11,-5))) +
  scale_y_continuous(expand=expansion(mult=-0.05,add=c(0,-300000))) +
# geom_text(data = subset(df2,!`Houshold Value (Dollars)`==0),
#           aes(x=Year, y= `Houshold Value (Dollars)`,
#               label = paste(label,"   ",sep="")),
#           adj=1, nudge_y=.5, nudge_x = -0000, 
#           color="black",size=3) +
  theme_void() +
  theme(aspect.ratio = 1)



# library(cowplot) 


final <-ggdraw()+
  draw_plot(p,scale=1.2)

#ggsave("test.png",width = 6, height = 6)


dev.off()  


ragg::agg_png(here::here("test2.png"),
              scaling=1.5,
              res = 320, 
              width = 6, 
              height = 4, units = "in")
final

dev.off()


# https://stackoverflow.com/questions/44170871/how-does-ggplot-scale-continuous-expand-argument-work
#scale_x_continuous(limits = c(1, 7), 
 #                  expand = expand_scale(mult = c(0, 0.5), 
  #                                       add = c(2, 0))
                   # left most position will be 1 - (7-1) * 0.0  -2 = -1, 
                   # right most position will be 7 + (7-1) * 0.5 = 10

