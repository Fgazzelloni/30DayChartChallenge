library(tidyverse)
library(igraph)

library(ggraph)
theme_set(silgelib::theme_roboto())

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv"
erasmus <- read_csv(url)


student_graph <-
  erasmus %>%
  filter(mobility_duration >= 7)  %>%
  count(sending_country_code, receiving_country_code, wt = participants, name = "students") %>%
  filter(sending_country_code != receiving_country_code) %>%
  mutate(across(contains("country_code"), countrycode::countrycode,
                origin = "eurostat", destination = "country.name")) %>%
  filter(students > 10) %>%
  graph_from_data_frame(directed = FALSE)


student_graph %>%
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(edge_alpha = students, edge_width = students), color = "midnightblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, family = "RobotoCondensed-Regular")
