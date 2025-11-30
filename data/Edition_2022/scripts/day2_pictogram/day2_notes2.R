

# devtools::install_github("JohnCoene/echarts4r.assets")
library(echarts4r.assets)
library(waffle)
waffle(
  c('Yes=70%' = 70, 'No=30%' = 30), rows = 10, colors = c("#FD6F6F", "#93FB98"),
  title = 'Responses', legend_pos="bottom"
)

# Steps to download and install fontawesome fonts

library(extrafont)
  
# download: https://github.com/FortAwesome/Font-Awesome/tree/master/webfonts
# fa-solid-900.ttf, fa-regular-400.ttf and fa-brands-400.ttf

# library(remotes)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()


extrafont::font_import(path="/Library/Fonts", pattern = "fa-", prompt =  FALSE)


loadfonts()

library(dplyr)
extrafont::fonttable() %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(grepl("Awesom", FamilyName)) %>% 
  select(FamilyName, FontName, fontfile)

library(showtext)
font_add(family = "FontAwesome5Free-Solid", regular = "/Users/federica/Library/Fonts/fa-solid-900.ttf")
font_add(family = "FontAwesome5Free-Regular", regular = "/Users/federica/Library/Fonts/fa-regular-400.ttf")
font_add(family = "FontAwesome5Brands-Regular", regular = "/Users/federica/Library/Fonts/fa-brands-400.ttf")
showtext_auto()

waffle(
  c(`Poor=10` =10, `Average=18` = 18, `Excellent=7` =7), rows = 5, colors = c("#FD6F6F", "#93FB98", "#D5D9DD"),
  use_glyph = "female", glyph_size = 12 ,title = 'Girls Performance', legend_pos="bottom"
)
