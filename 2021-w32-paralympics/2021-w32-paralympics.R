
## ---------------------------
##
## TidyTuesday 2021 Week 32- Paralympics
##
## Author: Allison Koh
##
## Date Created: 2021-08-02
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

# setup / load dependencies -----------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  tidytuesdayR,
  gganimate
)

# theme specs 
theme_set(
  theme_minimal() + 
    theme(
      legend.position = "right",
      plot.title = element_text(size = 20, face = "bold", hjust = 0, margin = margin(10,0,0,0)),
      plot.caption = element_text(color = "gray40"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(hjust = 0.5)
    )
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

# import data -------------------------------------------------------------
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv', guess_max = 10000)

# wrangle ----------------------------------------------------------------- 
top_countries_df <- athletes %>% count(abb, sort=TRUE) %>% top_n(25) 
top_countries <- top_countries_df$abb

athletes_tidy <- athletes %>% 
  count(type, abb, year) %>% 
  filter(abb %in% top_countries) %>% 
  mutate(year = factor(year))

# visualize ---------------------------------------------------------------
anim_plot <- athletes_tidy %>% 
  ggplot(aes(x = type, y = fct_rev(abb), size = n, color = n)) +
  geom_point() + 
  labs(
    title = "Paralympic Medals in {closest_state}",
    x = "",
    y = "",
    color = "",
    caption = "Data Source: International Paralympic Committee\nVisualization: Allison Koh"
  ) + 
  guides(size = "none") + 
  scale_color_gradient2(
    low = "#aa272f", 
    mid = "#00539f", 
    high = "#008542", 
    midpoint = 116
  ) + 
  transition_states(states = year) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

# animate 
animate(anim_plot, nframes = 400, duration = 15, fps = 20, end_pause = 5)

# save animation
anim_save("2021-w32.gif")

# write alt text 
write_alt_text(
  "Animated bubble chart",
  "paralympic medal counts over time for the top 25 represented countries,",
  "athletics and swimming are the most consistently represented category across all countries, and the USA has the most medals",
  "The legend is a three-part gradient of colors from the paralympic flag, with green representing the lowest number and red representing the highest.",
  "International Paralympics Committee"
)

# code graveyard ----------------------------------------------------------
# athletes_tidy <- athletes %>% 
#   # Wrangling code from Tan Ho (@_TanHo)
#   tidyr::extract(event, into = c("event","category"), regex = "(.*) (.*)$") %>%
#   count(year, type) 


