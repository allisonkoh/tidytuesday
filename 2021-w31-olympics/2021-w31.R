
## ---------------------------
##
## TidyTuesday 2021, Week 31 - Olympics Medals
##
## Author: Allison Koh
##
## Date Created: 2021-07-26
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
  DataExplorer,
  WDI,
  countrycode,
  janitor,
  showtext,
  rlang,
  dplyr,
  tidyr,
  cowplot,
  imager
)

# add fonts 
font_add_google(name = "Ubuntu Condensed", family = "ubuntu")

# show fonts 
showtext_auto() # automatically show text for new graphics devices 

# theme specs
theme_set(
  theme_void() + 
    theme(
      legend.position = "none",
      text = element_text(family = "ubuntu"),
      plot.title = element_text(size = 15, family = "ubuntu", face = "bold", margin = margin(t = 5)),
      plot.subtitle = element_text(margin = margin(t = 5, b = 20), vjust = 0.9),
      plot.caption = element_text(color = "gray40", margin = margin(t = 20, b = 5, r = 5))
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

# goals -------------------------------------------------------------------
#' do something with TS data 
#' proportion of medals against number of people in each country 
#' animated ts plot? possible aes: 
#'   - country = flag geom 
#'   - size = proportion of medals won per # on team 
#' other features to include; animate the plot, TS format? 
#' an idea: proportion circle barplots! with each circle corresponding to how it is on the logo 
#' https://www.visualcinnamon.com/portfolio/olympic-feathers/
#' host country effect: average # medals (weighted) when a country is host vs. when not host 

# questions/issues --------------------------------------------------------
#' how to subset? top 10 based on the metric? 

# import ------------------------------------------------------------------
import <- tt_load("2021-07-27")
olympics <- import$olympics
regions <- import$regions

# explore imported data ---------------------------------------------------
DataExplorer::create_report(olympics)
skim(olympics)

# wrangle -----------------------------------------------------------------

# tidy regions 
regions_tidy <- regions %>% 
  clean_names()

# tidy olympics
olympics_tidy <- olympics %>% 
  drop_na(medal) %>% 
  filter(year >= 1960) %>% 
  count(noc, year) %>% 
  rename(n_medals = n) %>% 
  left_join(., regions_tidy, by = "noc") %>% 
  rename(country = region) %>% 
  unite("country_year", c("country", "year"), remove = FALSE) %>% 
  select(-notes) 

# WDI indicators
gdp_ppp_df <- WDI(indicator = "NY.GDP.MKTP.PP.CD", country = "all", start = 1960, end = 2016) 

# merge in iso3c country codes and region and recode region var 
country_df <- countrycode::codelist %>% 
  select(iso2c, iso3c, continent) %>% 
  left_join(., gdp_ppp_df, by = "iso2c") %>% 
  rename(gdp_ppp = NY.GDP.MKTP.PP.CD) %>% 
  unite("country_year", c("country", "year"), remove = FALSE) %>% 
  drop_na()

# merge with medal data; calculate GDP-adjusted medal count (medals per total GDP*100 billion)
olympics_merged <- olympics_tidy %>% 
  left_join(., country_df, by = "country_year") %>% 
  rename(year = year.x, country = country.x) %>% 
  filter(year.y >= 1992) %>% 
  select(noc, country, year, country_year, continent, gdp_ppp, n_medals) %>% 
  mutate(
    medals_per_gdp = n_medals/gdp_ppp*10e11, # medals per 100b$ in GDP
  ) %>% 
  select(-gdp_ppp) %>% 
  group_by(noc, country, continent) %>% 
  mutate(
    av_medals_per_gdp = mean(medals_per_gdp), 
    continent = factor(continent, levels = c("Europe", "Africa", "Americas", "Asia", "Oceania"))
  )

# top 10 countries with the chosen metric (log-transformed average medals per $100billion in GDP)
olympics_merged_top10 <- olympics_merged %>% 
  count(noc, continent, av_medals_per_gdp) %>% 
  arrange(av_medals_per_gdp) %>% 
  ungroup() %>% 
  slice_head(n = 10)

# visualize  --------------------------------------------------------------

# logo 
olympics_logo <- load.image(file = "olympics_logo.png")

# base plot 
main_plot <- olympics_merged %>% 
  ggplot() + 
  labs(
    title = "GDP-Adjusted Olympic Medals",
    subtitle = "Country average medal count per $100billion in total GDP PPP, 1992-2016", 
    caption = "Data Sources: Kaggle and World Bank\nVisualization: Allison Koh (@allisonkoh_)"
  )

# write function for continent subplots
make_subplot = function(land, color){
  olympics_merged %>% 
    filter(continent == land) %>% 
    ggplot(aes(x = noc, y = av_medals_per_gdp, fill = continent, color = continent)) + 
    geom_bar(stat = "identity") + 
    coord_polar() + 
    scale_fill_manual(values = color) +
    scale_color_manual(values = color) 
}

# make subplots
euro_plot <- make_subplot("Europe", "#0085C7")
africa_plot <- make_subplot("Africa", "#000000")
americas_plot <- make_subplot("Americas", "#DF0024")
asia_plot <- make_subplot("Asia", "#F4C300")
oceania_plot <- make_subplot("Oceania", "#009F3D")

# frequency chart with top 10 countries
bar_plot <- olympics_merged_top10 %>% 
  ggplot(aes(x = reorder(country, av_medals_per_gdp), y = av_medals_per_gdp, fill = continent, color = continent, label = country)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(y = 0), color = "black", size = 2, hjust = 0) + 
  scale_fill_manual(values = c("#ffb3bf", "#ffe066")) +
  scale_color_manual(values = c("#ffb3bf", "#ffe066")) +
  coord_flip() + 
  labs(title = "Top Countries") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 9) 
  ) 

ggdraw() + 
  draw_plot(main_plot) + 
  draw_image(olympics_logo, x = .83, y = .85, width = .15, height = .15) +
  draw_plot(euro_plot, x = .15, y = .35, width = .5, height = .5) + 
  draw_plot(oceania_plot, x = .4, y = .1, width = .5, height = .5) +
  draw_plot(americas_plot, x = .47, y = .39, width = .5, height = .5)  +
  draw_plot(asia_plot, x = .15, y = .15, width = .5, height = .5) + 
  draw_plot(africa_plot, x = .26, y = .44, width = .5, height = .5) +
  draw_plot(bar_plot, x = .02, y = .02, width = .2, height = .2) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )
  
# save plot  --------------------------------------------------------------
ggsave("2021-w31.png", width = 6, height = 4)


# alt text ----------------------------------------------------------------
write_alt_text(
  "Faceted circular bar plot",
  "GDP-adjusted olympic medal counts (# medals per $100billion GDP PPP),",
  "the top-scoring countries using this metric are Kuwait, Thailand, Indonesia, Vietnam, Mexico, the Philippines, Saudi Arabia, United Arab Emirates, India, and China.",
  "The data for GDP included in calculations comes from the {WDI} R package, and the Olympic medals data from Kaggle included ranges from 1992 to 2016.",
  "Kaggle and the World Bank."
)

# code graveyard ----------------------------------------------------------
# olympics %>% 
#   count(sport, sort = TRUE) 
# 
# olympics %>% 
#   count(team, sort = TRUE) %>% 
#   View()
# 
# olympics %>% 
#   count(year)
# 
# olympics_tw <- olympics_tidy %>% 
#   filter(str_detect(team, "Taipei"))
# 
# olympics_tw %>% 
#   count(team) 
# 
# olympics_tw %>% View()
# str_detect(olympics$team, "Germ")
# olympics_tidy <- olympics %>% 
#   mutate(
#     medal = replace_na(medal, "none"),
#     team = str_remove(team, "-[:digit:]"),
#     medal_won = case_when(
#       medal != "none" ~ "yes",
#       medal == "none" ~ "no"
#     ),
#     medal_weight = case_when(
#       medal == "Gold" ~ 3,
#       medal == "Silver" ~ 2,
#       medal == "Bronze" ~ 1, 
#       medal == "none" ~ 0
#     )
#   ) 
# separate_rows(team, sep = "/") %>% # separate rows for those who belong to two teams
#   count(year, team, medal_won) %>% 
#   pivot_wider(
#     names_from = medal_won,
#     values_from = n, 
#     names_glue = "medal_won_{medal_won}"
#   ) %>% 
#   mutate(
#     medal_won_no = replace_na(medal_won_no, 0),
#     medal_won_yes = replace_na(medal_won_yes, 0),
#     medals_prop = medal_won_yes/(medal_won_no+medal_won_yes)
#   ) 
# names(olympics)
# olympics_tidy %>% 
#   arrange(-medal_won_yes, -medals_prop) %>% 
#   View()
# regions %>% View()
# 
# olympics_n22_df <- olympics_tidy %>% 
#   count(team, sort = TRUE) %>% 
#   filter(n == 22) 
# 
# olympics_n22 <- olympics_n22_df$team
# 
# olympics_tidy %>% 
#   count(team, sort = TRUE) %>% 
#   View()
# 
# olympics_tidy %>% 
#   count(medal)
# 
# olympics_tidy %>% 
#   ggplot(aes(x = team)) +
#   geom_histogram(stat = "count") + 
#   coord_flip()
# 
# olympics_tidy1 <- olympics_tidy %>% 
#   filter(team %in% olympics_n22)
# 
# olympics_tidy1 %>% 
#   arrange(-medals_prop, -medal_won_yes)
# 
# olympics_tidy1 %>% View()
# 
# 
# olympics_tidy %>% 
#   count(medal)
# 
# olympics_tidy %>% 
#   ggplot(aes(x = team)) +
#   geom_histogram(stat = "count") + 
#   coord_flip()
# 
# olympics_tidy1 <- olympics_tidy %>% 
#   filter(team %in% olympics_n22)
# 
# olympics_tidy1 %>% 
#   arrange(-medals_prop, -medal_won_yes)
# 
# names(olympics)
# olympics_tidy %>% 
#   arrange(-medal_won_yes, -medals_prop) %>% 
#   View()
# regions %>% View()
# 
# olympics_n22_df <- olympics_tidy %>% 
#   count(team, sort = TRUE) %>% 
#   filter(n == 22) 
# 
# olympics_n22 <- olympics_n22_df$team
# 
# olympics_tidy %>% 
#   count(team, sort = TRUE) 
