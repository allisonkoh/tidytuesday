
## ---------------------------
##
## TidyTuesday 2021 Week 24: Great Lakes Fish   
##
## Author: Allison Koh
##
## Date Created: 2021-06-07
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

# goals  ------------------------------------------------------------------
#' make at least one plot with time series data 
#' play with new geoms! YAY 

# setup / load dependencies -----------------------------------------------

# remotes::install_github("davidsjoberg/ggstream")

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  tidytuesdayR, 
  ggstream
)

theme_set(theme_void())
 
# import data -------------------------------------------------------------
fish_tt <- tidytuesdayR::tt_load("2021-06-08")
fish_raw <- fish_tt[["fishing"]]

# inspect -----------------------------------------------------------------
dim(fish_raw)
names(fish_raw)
glimpse(fish_raw)

# wrangle -----------------------------------------------------------------
fish_tidy <- fish_raw %>% 
  mutate(across(where(is.character), as.factor))

by_species <- fish_tidy %>% 
  drop_na(grand_total) %>% 
  group_by(species) %>% 
  summarize(total_obs = sum(grand_total)) %>%
  arrange(-total_obs)

top_species <- as_vector(by_species$species[c(1:8, 10, 12)])

fish_tidy_subset <- fish_tidy %>% filter(species == top_species)

# chaotic faceted stream graph  -------------------------------------------
fish_tidy_subset %>% 
  drop_na(grand_total) %>% 
  group_by(year, lake, species) %>% 
  summarize(total_obs = sum(grand_total)) %>% 
  ggplot(aes(year, total_obs, fill = species)) +
  geom_stream() + 
  facet_wrap(~lake, scales = "free", ncol = 1, strip.position = "left") + 
  scale_fill_brewer(type = "qual", palette = "Set3") +
  labs(
    title = "Total Fish Observed in the Great Lakes ~1867-2015",
    fill = "", 
    caption = "Visualization: Allison Koh | Source: Great Lakes Fishery Commission"
  ) + 
  theme(
    plot.title = element_text(hjust = 0, size = rel(1)),
    legend.position = "bottom", 
    legend.key.size = unit(0.2, "cm")
  )

# save plot ---------------------------------------------------------------
ggsave("2021-w24-plot.png", width = 4, height = 8)


# add bernie --------------------------------------------------------------

# remotes::install_github("R-CoderDotCom/ggbernie@main")
library(ggbernie)

fish_tidy %>% 
  drop_na() %>% 
  group_by(year, lake) %>% 
  summarize(total_fishies = sum(values)) %>% 
  ggplot(aes(year, total_fishies, fill = lake)) +
  geom_stream() +
  geom_bernie(aes(x = 1930, y = 20100), bernie = "sitting")
  
  
# archive -----------------------------------------------------------------

# fish_grouped <- fish_tidy %>% 
#   group_by(year, lake, species, values) %>% 
#   summarize(values_n = sum(values))
# 
# View(fish_grouped)

# fish_tidy %>% 
#   filter(str_detect(region, "^U")) %>% 
#   ggplot(aes(x = year, y = values, 
#              group = species)) + 
#   geom_line() + 
#   facet_wrap(~lake)
# 
# top_species <- fish_tidy %>%
#   count(species) %>% 
#   arrange(species, -n) %>% 
#   top_n(10)
# 
# 
# fish_tidy %>% 
#   drop_na(values) %>% 
#   group_by(year, species, lake) %>% 
#   summarize(values = sum(values)) %>% 
#   slice_max(values, n = 5, with_ties = FALSE)
# # count(species, lake) %>% 
# arrange(-n) %>% 
#   View()
# 
# test <- fish_tidy %>% 
#   drop_na(values) %>%
#   group_by(species, lake, year) %>%
#   summarize(values = sum(values)) %>% 
#   mutate(grp = cur_group_id()) %>% 
#   arrange(desc(species)) %>% 
#   slice_max(species, n = 5) 
# 
# fish_tidy %>% 
#   drop_na(values) %>%
#   group_by(year, lake) %>% 
#   summarize(values = sum(values)) %>% 
#   ggplot(aes(x = year, y = values, fill = lake)) + 
#   geom_stream() 
# # facet_wrap(~lake)
# 
# 
# fish_tidy %>% 
#   drop_na(values) %>% 
#   group_by(year, lake, species) %>% 
#   summarize(values = sum(values)) %>% 
#   ggplot(aes(x = year, y = values, color = species)) + 
#   geom_line() + 
#   facet_wrap(~lake)


