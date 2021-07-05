
## ---------------------------
##
## TidyTuesday 2021, Week 25 - DuBois Challenge Tweets  
##
## Author: Allison Koh
##
## Date Created: 2021-06-14
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

# setup / load dependencies -----------------------------------------------
if(!require("pacman")) install.packages("pacman")

# devtools::install_github("spcanelon/TidyTuesdayAltText")
# devtools::install_github("andreacirilloac/paletter")
# devtools::install_github("ropensci/geonames")

pacman::p_load(
  tidyverse, 
  tidytuesdayR,
  DataExplorer,
  sf,
  spData,
  maps,
  mapproj,
  TidyTuesdayAltText
)

# goals  ------------------------------------------------------------------
#' visualize a map of all tweet locations
#' use a color palette that matches DuBois' visualizations
#' derive that color palette from a picture using {paletteR}
#' try adding alt text with tidytuesday (found it!)

# issues ------------------------------------------------------------------
#' couldn't find the alt text package but FOUND IT 

# import ------------------------------------------------------------------
dubois_data <- tidytuesdayR::tt_load("2021-06-15")
tweets_raw <- dubois_data$tweets

# explore -----------------------------------------------------------------
dim(tweets_raw)
names(tweets_raw)

DataExplorer::introduce(tweets_raw) %>% View()
DataExplorer::plot_intro(tweets_raw)
DataExplorer::plot_missing(tweets_raw) # some missings from location, lon, lat but otherwise things look ok 

# wrangle -----------------------------------------------------------------
# global data 
tweets_sp <- tweets_raw %>% 
  drop_na() %>% # normally it would be drop_na(lat, long) but there weren't any other NAs
  st_as_sf(coords = c("long", "lat")) 

st_crs(tweets_sp) <- 4326 # lol i have no idea what i'm doing 

countries <- world %>% 
  select(name_long, continent) %>%  
  filter(continent != "Antarctica") %>% 
  rename(country = name_long)

counts <- st_join(tweets_sp, countries) %>% 
  drop_na(country) %>% 
  count(country) %>% 
  mutate(
    n_cat = case_when(
      n >= 1 & n <= 10 ~ "<10",
      n > 10 & n <= 100 ~ "11-100",
      n > 100 ~ ">100"
      )
  )

plot_data <- full_join(as.data.frame(countries), counts, by = "country") %>% 
  st_sf(sf_column_name = "geom")

# domestic data 

states_sp <- map_data("state") %>% 
  st_as_sf(coords = c("long", "lat")) 

st_crs(states_sp) <- 4326

st_join(tweets_sp, states_sp) %>% View()

ggplot(states_map, aes(x = long, y = lat)) + 
  geom_point() + 
  coord_map()

ggplot(states_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "darkcyan", color = "darkblue", alpha = 0.5) +
  theme_void() + 
  coord_map()



# global plot -------------------------------------------------------------

# using paletteR was a fail for smol number scales and continuous vars. Manual scale time w00t
dubois2_pal <- c("#519873", "#c41438", "#0a3c91")

# global plot  
plot_data %>% 
  ggplot(aes(fill = as_factor(n_cat))) + 
  geom_sf() + 
  labs(
    title = "DUBOIS CHALLENGE TWEETS AROUND THE WORLD", 
    caption = "Source: Anthony Starks, Allen Hillery, Sekou Tyler | #TidyTuesday 2021 W25 & #DuboisChallenge | @allisonkoh_",
    fill = ""
  ) +
  scale_fill_manual(
    values = dubois2_pal,
    limits = c("<10", "11-100", ">100")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10, b = 15), family = "mono", size = 20),
    legend.position = "top",
    legend.key.size = unit(0.2, "cm"),
    plot.background = element_rect(fill = "#e4d9cb")
  )

ggsave("global_plot.png", width = 12, height = 10)

# archive -----------------------------------------------------------------
# library(quanteda)
# tweet_corp <- quanteda::corpus(tweets_raw, text_field = "content")
# tweet_dtm <- dfm(
#   tweet_corp,
#   remove_punct = TRUE,
#   remove_symbols = TRUE,
#   remove_separators = TRUE,
#   remove_twitter = TRUE,
#   remove_url = TRUE
# ) %>% 
#   dfm_remove(
#     pattern = c(stpwords = "english")
#   ) %>% 
#   dfm_wordstem()
# 
# tweet_dtm %>% View()
# 
# prepped_tweets <- PrepText(tweets_raw, 
#                            groupvar = "username", 
#                            textvar = "content",
#                            node_type = "words",
#                            tokenizer = "words",
#                            pos = "all",
#                            remove_stop_words = TRUE,
#                            language = "english",
#                            compound_nouns = TRUE)
# 
# prepped_tweets1 <- prepped_tweets %>% 
#   select(lemma, username)
# 
# prepped_tweets %>% 
#   count(lemma) %>% 
#   View()
# add login info for geonames 
# options(geonamesUsername="akoh") 

# From https://stackoverflow.com/questions/21708488/get-country-and-continent-from-longitude-and-latitude-point-in-r

# coords2continent = function(points)
# {  
#   countriesSP <- getMap(resolution='low')
#   #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
#   
#   # converting points to a SpatialPoints object
#   # setting CRS directly to that from rworldmap
#   pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
#   
#   
#   # use 'over' to get indices of the Polygons object containing each point 
#   indices = over(pointsSP, countriesSP)
#   
#   #indices$continent   # returns the continent (6 continent model)
#   indices$REGION   # returns the continent (7 continent model)
#   # indices$ADMIN  #returns country name
#   #indices$ISO3 # returns the ISO3 code 
# }
# test_coords <- head(tweets1[c("lat", "long")]) 
# 
# coords2country(test_coords) # IT WORKS! i think... jk no it doesn't. SAD 
# more exploring!
# length(unique(tweets1$location))
# head(unique(tweets1$location))
# names(tweets1)
# head(tweets1[c("lat", "long")])
# revgeo::revgeo(longitude = "-74.006015", latitude = "40.712728")
# country_vec <- coords2continent(tweets1[c("lat", "long")])
# View(tweets1)
# tweets_tidy <- tweets1 %>% 
#   mutate(country = country_vec)
# 
# tweets_tidy %>% View()
# geodat <- st_join(counts, countries) %>% 
#   drop_na(name_long) %>% 
#   distinct() %>% 
#   st_as_sf()
# get palette 
# dubois_pal <- create_palette(
#   image_path = "dubois-viz2.jpeg",
#   number_of_colors = 3,
#   type_of_variable = "categorical"
# )
