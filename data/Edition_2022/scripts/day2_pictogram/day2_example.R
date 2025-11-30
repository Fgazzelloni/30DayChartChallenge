library(tidyverse)
#library(tidytuesdayR)
library(waffle)
library(extrafont)
library(patchwork)

# load data
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

view(unvotes)
view(roll_calls)
view(issues)

# data cleanup
A <- unvotes %>%
  inner_join(roll_calls %>% select(rcid, short, descr)) %>%
  filter(country %in% "United Kingdom") %>%
  filter(str_detect(short, "REFUGEE")) %>%
  count(country, vote) %>%
  
  # ggplot 
  ggplot(aes(label = vote, values = n)) +
  labs(fill = NULL, colour = NULL) +
  geom_pictogram(n_rows = 20,
                 aes(colour = vote),
                 flip = TRUE,
                 make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c(yes = "#A6D785", no = "#e4e4e4", abstain = "#c3c3c3")) +
  scale_label_pictogram(
    name = NULL,
    values = c(abstain = "child", no = "child", yes = "child")) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  theme_enhance_waffle() +
  ggtitle("United Kingdom") +
  labs(x="",y="") +
  theme(plot.margin = margin(t=4,1,1,1, "lines")) + # move legend above plot title
  theme(legend.direction="horizontal", 
        plot.title = element_text(hjust = 0.09)) +
  theme(legend.position = c(0.5, 1.2))

B <- unvotes %>%
  inner_join(roll_calls %>% select(rcid, short)) %>%
  filter(country %in% "United States") %>%
  filter(str_detect(short, "REFUGEE")) %>%
  count(country, vote) %>%
  
  # ggplot 
  ggplot(aes(label = vote, values = n)) +
  labs(fill = NULL, colour = NULL) +
  geom_pictogram(n_rows = 20,
                 aes(colour = vote),
                 flip = TRUE,
                 make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c(yes = "#A6D785", no = "#e4e4e4", abstain = "#c3c3c3")) +
  scale_label_pictogram(
    name = NULL,
    values = c(abstain = "child", no = "child", yes = "child")) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  theme_enhance_waffle() +
  ggtitle("United States") +
  labs(x="",
       y="") +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.09))

(A/B) + plot_annotation(
  title = 'REFUGEES WELCOME?',
  subtitle = 'The United Kingdom has voted in agreement with the United Nations on issues regarding
  refugees more frequently than the United States. The majority of issues relate to the
  treatment of Palistinian refugees. Each figure represents one issue raised at the
  United Nations General Assembly since 1946.',
  caption = '@danni_scales | source: Harvard Dataverse',
  theme = theme(plot.title = element_text(size = 26, hjust = 0.5),
                plot.subtitle = element_text(size = 14, hjust = 0.5))) &
  theme(text = element_text(family ="System Font", size = 14, color="grey40"),
        strip.text.x = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(fill = "white"))

ggsave("week_13.png", width=10, height=9)
