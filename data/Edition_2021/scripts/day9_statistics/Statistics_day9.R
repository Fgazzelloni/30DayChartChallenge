# Statistics Day9 ------------------------------------

library(tidyverse)
#library(ggthemes)
#library(ggrepel)
#library(stringr)


# load data --------------------

# url = "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv"

df_backup <- readxl::read_excel("download.xlsx")

############################################################

df <- read.csv("selected_countries.csv")


png("stats.png", width = 770, height = 580, unit = "px")

par(oma=c(3,3,3,3))
par(mfrow=c(1,1),mar=c(5,5,2,1)+0.1,bg="white")
my_bar <- barplot(height=df$weekly_count,
          names=df$country,
          density=c(5,10,20,30,7),
          angle=c(0,45,90,11,36) ,
          font.axis=1,#border="verde",
          col="brown",
          las=2 ,
          ylim=c(0,5500),
          names.arg=c("US","India","Italy","Poland","Mexico","Ukraine","Russia","France","Hungary","Peru"),
          main = "2021 - week 13")

text(my_bar,df$weekly_count+118,labels=df$weekly_count,cex=0.9)
mtext("Selected countries by higher Covid19 Deaths", side=3, line=2, cex=2, col="forestgreen")  
mtext("Viz Federica Gazzelloni | Datasource: ECDC | Statistics - Day9", side=1, line=5, adj=1.0, cex=1, col="forestgreen")


dev.off()


