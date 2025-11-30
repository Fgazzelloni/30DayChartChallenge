library(waffle)
library(hrbrthemes)
library(extrafont)
library(dplyr)

library(showtext)



extrafont::loadfonts()


extrafont::fonttable() %>% 
  as_tibble() %>% 
  filter(grepl("Awesome", FamilyName)) %>% 
  select(afmfile, FullName, FamilyName, FontName)


fa_grep("bread|pizza|apple|pear|peach|lemon|sandwich")

tibble(
  food_group = factor(
    c("Fruit", "Sandwiches", "Pizza"),
    levels=c("Fruit", "Sandwiches", "Pizza")
  ),
  consumption = c(5, 20, 52)
) -> xdf2

ggplot(xdf2, aes(fill = food_group, values = consumption)) +
  geom_waffle(make_proportional = TRUE) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()



ggplot(xdf2, aes(label = food_group, values = consumption)) +
  geom_pictogram(aes(label=food_group,value=consumption))+
  scale_label_pictogram(name = NULL,
                        values = c(
                          Fruit = "apple-alt", 
                          Sandwiches = "bread-slice", 
                          Pizza = "pizza-slice"
                        ))
  
  

