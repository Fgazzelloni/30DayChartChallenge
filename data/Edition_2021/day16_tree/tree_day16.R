# Tree day 16 ####################################


# load libraries ############################
library(dplyr)
library(ggplot2)
library(medflex)
library(extrafont)

library(treeio)
library(ggtree)



# data ##################################

# Data source: UPBdata
# individuals who divorced between March 2008 and March 2009 
# in four major courts in Flanders 

# 385 individuals related to romantic relationship and breakup characteristics

# initiator: of the divorce
# att: anxious attachment level
# negaff: experienced negative affectivity

# attbin: anxious attachment level: 
         # 1 = higher than sample mean, 0 = lower than sample mean

# attcat: multicategorical anxious attachment level: 
         # L = low, M = intermediate, H = high

# UPB: displayed unwanted pursuit behavior(s) towards the ex-partner


# load data & selection ---------------------
data(UPBdata, package = "medflex")
head(UPBdata)
str(UPBdata)


UPBdata$UPB = as.factor(ifelse(UPBdata$UPB <= 0.4, "Low", "High"))

my_UPB<-UPBdata%>%dplyr::select(-2,-3)



# transform data set into hclust ############################
hc <- hclust(dist(my_UPB), method = "average")

# transform hclust into Phylo ############################
my_hc<-as.phylo(hc)

# manipluation ##################################
groupInfo <- split(my_hc$tip.label, 
                   gsub("_\\w+", "", my_hc$tip.label))

my_hc2 <- groupOTU(my_hc, groupInfo)

# checking for edges ###################################
edge=data.frame(my_hc2$edge, edge_num=1:nrow(my_hc2$edge))
colnames(edge)=c("parent", "node", "edge_num")

# plotting #######################################
main_plot<-ggtree(my_hc2, aes(color=group), layout='circular',ladderize=F) + 
  geom_tiplab(size=1, aes(angle=angle)) +
  geom_point2(aes(subset=(node==1)), shape=21, size=5, fill='green')+
  ggtitle("Unwanted pursuit behavior: \nTree related to romantic relationship and breakup characteristics \non 385 individuals who divorced between March 2008 and March 2009")+
  #geom_text(label="")+
  #geom_text(data = labels, aes(x, y, label = edu), size = 5)+
  labs(subtitle="Strategies for the Estimation of Natural Direct and Indirect Effects of bad relationships",
       caption="Multivariate Behavioral Research, Viz Federica Gazzelloni Datasource: R Package medflex Tree Day 16")+
  theme_void() + 
  annotate("text", x = 0, y = 1.018,
           label = "Correlated Risk factors",
           size = 8,
           fontface = "bold",
           family = "Courier New")+
  theme(legend.position = "none",
        plot.title = element_text(family="Andale Mono",color="darkred",size=12),
        plot.subtitle = element_text(family="Andale Mono",size=9,color="darkolivegreen"),
        plot.caption = element_text(family="Andale Mono",size=8,face="bold"),
        axis.text.x = element_text(family="Andale Mono",size=8),
        plot.background = element_rect(color = "lightgoldenrod",size=44))

# saving ######################################


ragg::agg_png(here::here("day16_tree", "Tree_day16.png"),
              res = 320, width = 14, height = 8, units = "in")
main_plot

dev.off()






