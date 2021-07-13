
## ---------------------------
##
## TidyTuesday 2021, Week 28   
##
## Author: Allison Koh
##
## Date Created: 2021-07-05
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

# setup / load dependencies -----------------------------------------------
# load/install pacman for package management 
if(!require("pacman")) install.packages("pacman")

# load/install CRAN packages 
pacman::p_load(
  tidyverse,
  tidytuesdayR,
  DataExplorer,
  janitor, # useful for data wrangling, using it to identify duplicates for this data 
  rnaturalearth, 
  rnaturalearthdata,
  sp, 
  rgeos,
  broom, 
  randomcoloR, 
  gifski # for saving animations 
)

# load/install packages from GitHub 
pacman::p_load_gh(
  "mrjoh3/ggtrack" # adds QR with source code to plot 
)


# helper functions --------------------------------------------------------
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

# for wrapping legend labels while keeping original factor levels 
# https://github.com/tidyverse/stringr/issues/107
str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

# goals -------------------------------------------------------------------
#' submit contribution within 2.5 hours! 
#' visualize a portion of the data (get ideas from the original article attached)
#' custom font
#' add QR for source code via {ggtrack}
#' generate alt text with a custom function from Tom Mock (Twitter: thomas_mock)

# grievances --------------------------------------------------------------
#' map or TS plot? I promised myself I'd look at a subset of the data but a map of independence days from the UK/not the UK would be fun to put together 
#' MAYBE both? If time? 
#' Ha ha ugh 
#' ok lol i ditched most of my original goals 

# import ------------------------------------------------------------------
# tidytuesdayR::tt_load("2021-07-06")
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

# import map data as spatial polygons data frame (spdf)
world_spdf <- ne_countries(scale = "medium", returnclass = "sp")
world_spdf_tidy <- tidy(world_spdf, region = "sovereignt") %>% 
  filter(id != "Antarctica") # ignore warnings, this seems to work 

# explore -----------------------------------------------------------------
# glimpse the whole dataset 
glimpse(holidays)

# where aer the missings 
DataExplorer::plot_intro(holidays)
DataExplorer::plot_missing(holidays) # a lot of features missing 12.5% of data 

# where countries are celebrating independence from (aka how many are celebrating independence from the UK)
holidays_tidy %>% count(independence_from) %>% arrange(-n) %>% View()


# wrangle -----------------------------------------------------------------
holidays_tidy <- holidays %>%  
  filter(date_mdy != "January 26, 1950") %>% # this is a hacky way of removing one of the India rows
  mutate(
    across(where(is.character), as.factor), 
    independence_from_uk = if_else(
      independence_from == "United Kingdom", 1, 0
    ),
    independence_from = recode(
      independence_from, 
      `Spanish Empire[72]` = "Spanish Empire",
      `Soviet Union[80]` = "Soviet Union", 
      `Soviet Union[55]` = "Soviet Union"
    ),
    independence_from_uk = replace_na(independence_from_uk, 0),
    i_from = case_when(
      str_detect(independence_from, "United Kingdom") ~ "United Kingdom", # the UK is sneaky and has a lot of categories 
      country == "United States" ~ "United Kingdom",
      independence_from == "France" ~ "France", 
      independence_from == "Spanish Empire" ~ "Spanish Empire",
      independence_from == "Soviet Union" ~ "Soviet Union",
      independence_from == "Ottoman Empire" ~ "Ottoman Empire",
      independence_from == "Portugal" ~ "Portugal",
      independence_from == "Russian Soviet Federative Socialist Republic" ~ "Russian Soviet Federative Socialist Republic",
      independence_from == "Spain" ~ "Spain",
      independence_from == "Belgium" ~ "Belgium",
      independence_from == "Empire of Japan" ~ "Empire of Japan",
      independence_from == "SFR Yugoslavia" ~ "SFR Yugoslavia",
      TRUE ~ "Other"
    ),
    # i_from = replace_na(i_from, "No data"),
    country = recode(
      country,
      `United States` = "United States of America",
      `China, People's Republic of` = "China",
      `China, Republic of` = "Taiwan", # luv to see it 
      `Bahamas, The` = "The Bahamas",
      Eswatini = "Swaziland", # replacing with colonial name SAD
      `Gambia, The` = "Gambia",
      Tanzania = "United Republic of Tanzania",
      `Congo, Democratic Republic of the` = "Democratic Republic of the Congo",
      `Congo, Republic of the` = "Republic of Congo",
      `Guinea-Bissau` = "Guinea Bissau", 
      Micronesia = "Federated States of Micronesia",
      `Netherlands, The` = "Netherlands",
      `North Macedonia` = "Macedonia",
      `São Tomé and Príncipe` = "Sao Tome and Principe",
      `Serbia` = "Republic of Serbia", 
      `Vatican City` = "Vatican"
      #,
      # Tuvalu = ???? I can't find the replacement/colonial distinction WELP 
    )
  ) %>% 
  rename(id = country) 


# merge with map data -----------------------------------------------------
# reorder levels for factor `i_from`
i_from_levels <- c("United Kingdom", "Ottoman Empire", "France", "Portugal", "Spanish Empire", "Russian Soviet Federative Socialist Republic", "Soviet Union", "Spain", "Belgium", "SFR Yugoslavia", "Empire of Japan", "Other", "No data")

# merge data 
world_spdf_merged <- world_spdf_tidy %>% 
  left_join(., holidays_tidy, by = "id") %>% 
  select(-c("independence_from_uk", "name_of_holiday", "event_commemorated_and_notes", "date_mdy", "date_of_holiday", "month", "day", "weekday", "date_parsed", "year_of_event", "independence_from")) %>% 
  mutate(
    year = case_when(
      id == "Kashmir" ~ 1947,
      TRUE ~ year
    ),
    year = replace_na(year, 0), 
    i_from = case_when(
      year == 0 ~ "No data",
      TRUE ~ i_from
    ),
    i_from = replace_na(i_from, "No data"),
    i_from = factor(i_from, levels = i_from_levels)
  )

# color palette  ----------------------------------------------------------
# generate a palette of 11 distinct colors 
palette <- randomcoloR::distinctColorPalette(11)

# manually add colors for "other" and "no data" categories 
other <- "#443A83FF"
none <- "lightgrey"

# combine palette with other and no data colors 
pal <- c(palette, other, none)

# test palette 
pie(rep(1, 13), col=pal)

# manually replace colors that don't make sense (will not necessarily be these colors when code is reproduced)
pal[4] <- "#36c9ac"
pal[7] <- "#00bfff"

# plot --------------------------------------------------------------------
holidays_mapped <- world_spdf_merged %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = world_spdf_tidy, fill = "darkgrey", color = "black") + 
  geom_polygon(aes(fill = str_wrap_factor(i_from, 25)), color = "black") +
  transition_reveal(as.integer(year)) +
  # coord_map() + 
  theme_void() + 
  labs(
    title = "International Independence Days Over Time",
    subtitle = "Year: {frame_along}",
    fill = "Colonial Legacy (Are they escapable? That's a question for a different plot...)",
    caption = "#TidyTuesday 2021, Week 28\nViz: Allison Koh (@allisonkoh_)\nData: Wikipedia"
  ) + 
  scale_fill_manual(values = pal) +
  guides(
    fill = guide_legend(
      title.position = "top",
      ncol = 3
    )
  ) + 
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.95),
    plot.title = element_text(hjust = 0.05),
    plot.subtitle = element_text(hjust = 0.03)
  ) 

animate(holidays_mapped, end_pause = 30)


# save plot and write alt text --------------------------------------------
anim_save("tt-2021-28.gif")
write_alt_text(
  "Animated world map",
  "years that countries around the world declared independence and the countries from which countries declared independence",
  "the years recorded range from 1291 and 2011, and the colonizer from which the most countries declared independence is the United Kingdom (big surprise there)",
  "Title: International Independence Days Over Time. Year: animated number ranging from 1291 to 2011. Caption: Colonial Legacy (Are they escapable? That's a question for a different plot...). Legend contains: United Kingdom, Ottoman Empire, France, Portugal, Spanish Empire, Russian Soviet Federative Socialist Republic, Soviet Union, Spain, Belgium, SFR Yugoslavia, Empire of Japan, Other, and No data.",
  "Wikipedia and the #TidyTuesday project by the R4DS community. Contribution and data preparation by Isabella Velasquez."
  )

# code graveyard ----------------------------------------------------------
# world_spdf_tidy %>% count(id) %>% View()
# holidays_tidy <- holidays %>% 
#   mutate(
#     across(where(is.character), as.factor), 
#     independence_from_uk = if_else(
#       independence_from == "United Kingdom", 1, 0
#     ),
#     independence_from_uk = replace_na(independence_from_uk, 0),
#     country = recode(
#       country,
#       `Bahamas, The` = "The Bahamas",
#       Eswatini = "Swaziland", # replacing with colonial name SAD
#       `Gambia, The` = "Gambia",
#       Tanzania = "United Republic of Tanzania"#,
#       # Tuvalu = ???? I can't find the replacement/colonial distinction WELP 
#     )
#   ) %>% 
#   rename(id = country) 
# explore and wrangle su
# subset for countries celebrating emancipation from the UK 
# holidays_tidy_subset <- holidays %>% 
#   mutate(across(where(is.character), as.factor)) %>% 
#   filter(independence_from == "United Kingdom")

# look at subset 
# glimpse(holidays_tidy_subset)
# holidays_tidy_subset %>% View() 

# find missings 
# DataExplorer::plot_missing(holidays_tidy_subset)

# look for whether there are any countries with two independence days 
# holidays_tidy_subset %>% janitor::get_dupes(country) # Cyprus
# check if independence from UK var merged correctly 
# merged_correctly <- world_spdf_merged %>% 
#   count(id, independence_from_uk) %>% 
#   filter(independence_from_uk == 1) 
# 
# total_ind <- holidays_tidy %>% filter(independence_from_uk == 1)
# 
# setdiff(total_ind$id, merged_correctly$id)
# 
# glimpse(world_spdf_merged)
# 
# world_spdf_merged %>% 
#   count(independence_from_uk)
# 
# names(holidays_tidy)  
# names(world_spdf_tidy)
# holidays_tidy %>% count(independence_from_uk) 
# world_spdf_tidy %>% 
#   ggplot(aes(x = long, y = lat, group = group)) + 
#   geom_polygon() +
#   coord_map() +
#   theme_void()
# setdiff(holidays_tidy$id, world_spdf_tidy$id)
# setdiff(world_spdf_tidy$id, holidays_tidy$id)
  # world_spdf_merged %>% count(year) %>% View()
  # DataExplorer::plot_intro(world_spdf_merged)
  # DataExplorer::plot_missing(world_spdf_merged)
  # 
  # new_DF<-dplyr::filter(world_spdf_merged,is.na(i_from)) 
  # new_DF %>% count(id) 
  # 
  # world_spdf_merged
  # holidays_tidy %>% count(i_from) %>% arrange(-n) 
# base_map <- world_spdf_tidy %>% 
#   ggplot(aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "white", color = "black") + 
#   coord_map()
# ggtrack(holidays_mapped, 
#         qr_content = "https://github.com/allisonkoh")
# palette <- distinctColorPalette(11)

# uncomment line below for reproducibility based on line above 
# static plot to test stuff 
# world_spdf_merged %>% 
#   ggplot(aes(x = long, y = lat, group = group)) + 
#   geom_polygon(data = world_spdf_tidy, fill = "darkgrey", color = "black") + 
#   geom_polygon(aes(fill = str_wrap_factor(i_from, 25)), color = "black") +
#   # transition_reveal(as.integer(year)) + 
#   coord_map() + 
#   theme_void() + 
#   labs(
#     title = "International Independence Days Over Time",
#     subtitle = "Year: {frame_along}",
#     fill = "Colonial Legacy (Are they escapable? That's a question for a different plot...)",
#     caption = "#TidyTuesday 2021, Week 28\nViz: Allison Koh (@allisonkoh_)\nData: Wikipedia"
#   ) + 
#   scale_fill_manual(values = pal) +
#   guides(
#     fill = guide_legend(
#       title.position = "top",
#       ncol = 3
#     )
#   ) + 
#   theme(
#     legend.position = "bottom",
#     # panel.background = element_rect(fill = "lightblue"),
#     plot.caption = element_text(hjust = 0.95)
#   ) # -> holidays_mapped
# viridis_pal <- viridis::plasma(11)

# brewer_pal <- brewer.pal(11, "Set3")
