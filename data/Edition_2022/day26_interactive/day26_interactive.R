# Library
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)

# https://rstudio.github.io/dygraphs/gallery-plot-labels.html

# codesource: https://r-graph-gallery.com/318-custom-dygraphs-time-series-example.html
# datasource: https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/evolution.html
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)

# Check type of variable
#str(data)
#data%>%head


df <- data%>%
  mutate(date=as.Date(date,"%Y-%m-%d"))



# Then you can create the xts necessary to use dygraph
don <- xts(x = df$value, order.by = df$date)

presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = F, width = 60)
}


# Finally the plot
p <- dygraph(don, 
main = "Bitcoin: decentralized peer-to-peer electronic exchange - 2013-2018 (DataViz: @fgazzelloni)",
ylab = "Value of Bitcoin in $") %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.13, drawGrid = FALSE, colors="black") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1) %>%
  presAnnotation("2017-12-17", text = "Top 20k$") %>%
  dyShading(from = "2013-04-28",to="2013-11-01",color = "#b2d8d8") %>%
  dyShading(from = "2013-11-02", to = "2014-05-18", color = "#CCEBD6") %>%
  dyShading(from = "2014-05-19", to = "2017-05-25", color = "#b2d8d8") %>%
  dyShading(from = "2017-05-26", to = "2017-11-11", color = "#CCEBD6") %>%
  dyShading(from = "2017-11-12", to = "2018-02-06", color = "#b2d8d8") %>%
  dyShading(from = "2017-11-17", to = "2018-02-06", color = "#dfe3ee") %>%
  dyShading(from = "2018-02-07", to = "2018-04-18", color = "#b2d8d8") %>%
  dyEvent("2013-09-05", "CryptoLocker crippled more than 250,000 computer systems", labelLoc = "bottom") %>%
  dyEvent("2013-12-01", "Massive Protests in Ukraine for free Europe trade", labelLoc = "bottom") %>%
  dyEvent("2014-03-18","Russia Annexes Crimea and Threatens the Rest of Ukraine", labelLoc = "bottom") %>%
  dyEvent("2015-09-30", "Russian military intervention in the Syrian civil war", labelLoc = "bottom") %>%
  dyEvent("2015-11-13", "Three teams of ISIS terrorists struck at four locations in Paris", labelLoc = "bottom") %>%
  dyEvent("2016-06-23", "The UK has voted to leave the EU by 52% to 48%", labelLoc = "bottom") %>%
  dyEvent("2017-01-20", "Barack Obama ended his tenure and Donald Trump became the 45th president of the United States", labelLoc = "bottom") %>%
  dyEvent("2017-05-25", "The Organization of American States examines the crisis in Venezuela", labelLoc = "bottom") %>%
  dyEvent("2017-12-01", "US CFTC approved Bitcoins trading on three major markets", labelLoc = "bottom") %>%
  dyEvent("2017-12-22", "Fell from an all-time trading high to 12k$", labelLoc = "bottom") 

p 
# save the widget
library(htmlwidgets)
saveWidget(p, file=paste0( getwd(), "/data/Edition_2022/day26_interactive/day26_interactive.html"))



