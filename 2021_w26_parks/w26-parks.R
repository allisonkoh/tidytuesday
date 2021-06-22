
## ---------------------------
##
## TidyTuesday Week 2021-26 
##
## Author: Allison Koh
##
## Date Created: 2021-06-21
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

# goals -------------------------------------------------------------------
#' make a map of dog parks per 100k residents by city in the US 
#' try not to cry too much while working with geospatial data 
#' prioritize minimizing time over code efficiency

# setup / load dependencies -----------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  here,
  tidytuesdayR,
  DataExplorer,
  maps
)

# helper functions 
vagrep <- Vectorize(agrep, "pattern")
'%ni%' <- Negate('%in%')

# set ggplot theme 
theme_set(theme_void())

# import ------------------------------------------------------------------
tt_data <- tt_load("2021-06-22")
tt_raw <- tt_data$parks

# explore -----------------------------------------------------------------
glimpse(tt_raw)
table(tt_raw$city)
DataExplorer::plot_intro(tt_raw) # 17.58% missing data 

# wrangle -----------------------------------------------------------------
# griyoed data - dog parks per 100k residents by city 
tt_tidy <- tt_raw %>%
  filter(year == "2020") %>% 
  group_by(city) %>%
  summarize(dogparks_per_100k = sum(dogpark_data, na.rm = TRUE)) %>% 
  mutate(
    city = recode(
      city, 
      `Arlington, Texas` = "Arlington TX", 
      `Arlington, Virginia` = "Arlington VA",
      `Washington, D.C.` = "WASHINGTON DC",
      `Mesa` = "Mesa AZ",
      `Irving` = "Irving TX", 
      `Irvine` = "Irvine CA",
      `Atlanta` = "Atlanta GA",
      `Aurora` = "Aurora CO",
      `Henderson` = "Henderson NV",
      `Glendale` = "Glendale AZ", 
      `Garland` = "Garland TX",
      
    ),
    city = str_replace(city, "St.", "Saint")
  )

# vector of cities to remove from map data (based on first pass)
cities_rm <- c("Arlington MA", "Wichita Falls TX", "Tulare CA", "Champaign IL", 
               "Nampa ID", "Tamarac FL", "Tamiami FL", "Saint Louis Park MN", 
               "Seattle Hill-Silver Firs WA", "South San Francisco CA", 
               "Parkway-South Sacramento CA", "West Sacramento CA", "Kenosha WI",
               "Moreno Valley CA", "Redondo Beach CA", "Renton WA", 
               "Portland ME", "Delano CA", "Plantation FL", "Pittsburg CA",
               "Orland Park IL", "Newark CA", "Newark OH", "West New York NY",
               "Miami Beach FL", "North Miami Beach FL", "North Miami FL",
               "Costa Mesa CA", "La Mesa CA", "Mesquite TX", "Metairie LA",
               "Madison AL", "East Los Angeles CA", "Lincoln CA", "North Las Vegas NV",
               "Kansas City KS", "Jacksonville NC", "West Des Moines IA", "Columbus GA",
               "Columbus IN", "Cleveland Heights OH", "Tustin CA", "Aurora IL", 
               "Atlantic City NJ", "North Atlanta GA", "Pearland TX", "Buffalo Grove IL"
)

# load map data
us_cities <- us.cities %>% rename(city = name) %>% 
  filter(city %ni% cities_rm)

# match city names 
city_match_list <- vagrep(tt_tidy$city, us_cities$city, max.distance = 1, value = TRUE)

# turn city name match list into data frame 
city_match_df <- as.data.frame(do.call(rbind, city_match_list)) %>% 
  mutate(city = rownames(.)) %>% 
  rename(city_map = V1)

# merge city match df into tt df 
dogparks <- merge(tt_tidy, city_match_df) %>% 
  select(-city) %>% 
  rename(city = city_map)

# merge tt df into map data 
dogparks_cities <- merge(dogparks, us_cities) %>% 
  filter(long >= -130) # filter out alaska and hawaii 

# visualize ---------------------------------------------------------------
# load df for base map 
states <- map_data("state")

# map!!! 
ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = "grey") +
  geom_point(data = dogparks_cities, aes(x = long, y = lat, size = dogparks_per_100k, color = dogparks_per_100k)) +
  # scale_color_parksAndRec(type = "continuous") +\
  scale_color_viridis_c(option = "C") +
  coord_map() +
  labs(
    title = "ˁ˚ᴥ˚ˀ Dog Parks in the United States ˁ˚ᴥ˚ˀ", 
    subtitle = "If you rank your cities by how many dog parks there are, you'll like this map :D",
    color = "Parks (per 100,000 residents)",
    caption = "#TidyTuesday 2021 W26 • Viz: @allisonkoh_ • Source: The Trust for Public Land"
  ) +
  guides(size = FALSE) + 
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10), size = 15),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 5, b = 15), size = 10.5),
    plot.caption = element_text(hjust = 0.95, margin = margin(t = 12)),
    legend.title = element_text(size = 10),
    legend.position = "top"
  )

# save plot ---------------------------------------------------------------
ggsave("dogpark-plot.png")
