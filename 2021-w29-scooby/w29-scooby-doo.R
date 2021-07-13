
## ---------------------------
##
## TidyTuesday Week 29: Scooby Doo 
##
## Author: Allison Koh
##
## Date Created: 2021-07-12
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

# setup / load dependencies -----------------------------------------------
if(!require("pacman")) install.packages("pacman")

# from CRAN 
pacman::p_load(
  tidyverse,
  tidytuesdayR,
  DataExplorer,
  showtext,
  extrafont,
  qdapRegex # TC function for title case 
)

# from GitHub 
pacman::p_load_gh(
  "davidsjoberg/ggsankey",
)

# helper function for writing alt text 
# https://twitter.com/thomas_mock/status/1375853258145734660
write_alt_text <- function(
  chart_type, 
  type_of_data, 
  reason, 
  misc,
  source
){
  glue::glue(
    "{chart_type} of {type_of_data} where {reason}. \n\n{misc}\n\nData source from {source}"
  )
}


# viz stuff  --------------------------------------------------------------

# add fonts 
font_add_google(name = "Fira Sans", family = "fira")
font_add_google(name = "Roboto", family = "roboto")

# show fonts 
showtext_auto() # automatically show text for new graphics devices 

# goals  ------------------------------------------------------------------
#' minimize time making plot 
#' focus on a subset of the data 
#'   - monsters? 
#'   - characters (caught/captured/unmasked)

# import  -----------------------------------------------------------------
import <- tt_load("2021-07-13")
scooby <- import$scoobydoo

# explore  ----------------------------------------------------------------
DataExplorer::plot_intro(scooby)
DataExplorer::plot_missing(scooby) # only one column with missings: `trap_work_first`

glimpse(scooby)

scooby %>% 
  count(motive, sort = TRUE) 

scooby %>% 
  count(setting_country_state, sort = TRUE) %>% View() # mostly US, some other countries 
names(scooby)

# wrangle  ----------------------------------------------------------------
scooby_tidy <- scooby %>% 
  filter(format != c("Crossover", "Movie", "Movie (Theatrical)")) %>% 
  select(
    c("index"), 
    starts_with(c("caught", "captured", "unmask")), 
    -ends_with(c("other", "not"))
  ) %>% 
  pivot_longer(
    cols = starts_with(c("caught", "captured", "unmask")),
    names_to = c("action", "character"), 
    names_sep = "_",
    values_to = "value"
  ) %>% 
  mutate(
    value = as.integer(as.logical(value)), 
    action = recode(
      action, 
      unmask = "unmasked\nthe monster",
      caught = "caught\nthe monster",
      captured = "was captured"
    ),
    character = unlist(TC(character))
  ) %>% 
  filter(value != 0) 

scooby_sankey <- scooby_tidy  %>% 
  make_long(character, action) 

# visualize ---------------------------------------------------------------

# create palette manually 
scooby_pal <- c("#B2C6C7", "#C570C6", "#128a84", "#B06E0E", "#79af30", "#B9D675", "#A44138", "#4b0055")

# test palette 
pie(rep(1, 8), col=scooby_pal)

# Fred = 3 
# Daphnie = 2
# Scooby = 4
# Shaggy = 5

# plot 
scooby_sankey %>% 
  ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) + 
  geom_sankey(flow.alpha = .6) +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_manual(values = scooby_pal) + 
  labs(
    title = str_wrap("JINKIES! Scooby Doo Characters and their Roles"),
    subtitle = str_wrap("In each episode of Scooby Doo, there are characters who are captured, characters who caught the monsters, and characters who unmask the monsters. This chart visualizes the proportion of characters who were involved in each, based on episode-level data from ScoobyPedia."),
    caption = "#TidyTuesday 2021, Week 29 | @allisonkoh | Source: Scoobypedia"
  ) +
  theme_void() + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 15, family = "fira", face = "bold"),
    text = element_text(family = "roboto")
  )

# export  -----------------------------------------------------------------
ggsave("scooby-plot.png")


# write alt text ----------------------------------------------------------
write_alt_text(
  "Sankey diagram", 
  "data on Scooby Doo characters and the actions they partake in during episodes,",
  "the proportions between characters and actions are compared.",
  "The characters are Velma, Shaggy, Scooby, Fred, and Daphnie.\n\nTitle: JINKIES! Scooby Doo Characters and their Roles\n\nCaption: In each episode of Scooby Doo, there are characters who are captured, characters who caught the monsters, and characters who unmask the monsters. This chart visualizes the proportion of characters who were involved in each, based on episode-level data from ScoobyPedia.",
  "ScoobyPedia."
)

# code graveyard ----------------------------------------------------------

# test <- scooby_tidy %>% 
#   count(index, action, character)
# 
# pivot_wider(
#   names_from = "action",
#   values_from = "value"
# ) %>% 
#   pivot_wider(
#     names_from = "action",
#     values_from ="value"
#   ) %>% 
#   count(character, caught, captured, unmask) %>% 
#   filter()
# 
# scooby_tidy %>% 
#   mutate(
#     caught = if_else(caught == TRUE, character, NA)
#   )
# scooby_tidy %>% 
#   mutate(
#     caught = if_else()
#   )

# format data for ggsankey 
# scooby_sankey <- scooby_tidy %>% 
#   make_long(caught, captured, unmask)
# 
# scooby_sankey1 <- scooby %>% 
#   make_long(starts_with(c("unmask", "caught", "captured")), -ends_with(c("other", "not"))) 
# filter(node == "TRUE", next_node == "TRUE")

# scooby_sankey %>% 
#   ggplot(aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node, levels = c("unmasked", "captured", "Daphnie", "Fred","Scooby", "Shaggy", "caught", "Velma")), label = node)) + 
#   geom_sankey(flow.alpha = .6) +
#   geom_sankey_label(size = 3, color = "white", fill = "gray40") +
#   scale_fill_manual(values = scooby_pal) + 
#   labs(
#     title = str_wrap("JINKIES! Scooby Doo Characters and their Roles"),
#     subtitle = str_wrap("In each episode of Scooby Doo, there are characters who are caught by monsters, characters who capture the monsters ("),
#     caption = "#TidyTuesday 2021, Week 29 | @allisonkoh | Source: Kaggle"
#   ) +
#   theme_void() + 
#   theme(
#     legend.position = "none",
#     plot.title = element_text(size = 15, family = "fira", face = "bold"),
#     text = element_text(family = "roboto")
#   )
