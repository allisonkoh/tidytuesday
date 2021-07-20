## ---------------------------
##
## TidyTuesday 2021, Week 30: US Droughts
##
## Author: Allison Koh
##
## Date Created: 2021-07-19
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
  skimr,
  ggridges,
  geofacet,
  showtext
)

# font stuff 
showtext_auto()
font_add_google("Fira Sans", "fira")

# image quality options for gganimate
options(gganimate.dev_args = list(width = 6, height = 4, units = 'in', res=300))

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


# TODO --------------------------------------------------------------------
#' Make a map. Either animated or change in some data point over time 
#' 
 
# import ------------------------------------------------------------------
import <- tt_load("2021-07-20")
drought <- import$drought

# explore -----------------------------------------------------------------
DataExplorer::create_report(drought) # WOAH THIS IS OP 

drought %>% count(state_abb)

range(drought$map_date)

glimpse(drought)

drought_moar %>% 
  ggplot(aes(x = area_pct)) + 
  geom_histogram()

drought %>% 
  mutate(drought_lvl = as.factor(drought_lvl)) %>% 
  ggplot(aes(x = drought_lvl, y = pop_total)) +
  geom_col()

# wrangle ----------------------------------------------------------------
drought_tidy <- drought %>% 
  group_by(valid_start, state_abb) %>% 
  mutate(
    max_level = max(pop_total),
    drought_lvl = factor(drought_lvl, levels = c("None", "D0", "D1", "D2", "D3")),
    lvl = case_when(
      drought_lvl == "None" ~ "None", 
      drought_lvl == "D0" ~ "Abnormally Dry",
      drought_lvl == "D1" ~ "Moderate Drought",
      drought_lvl == "D2" ~ "Severe Drought",
      drought_lvl == "D3" ~ "Extreme Drought"
    ),
    lvl = factor(lvl, levels = c("None", "Abnormally Dry", "Moderate Drought", "Severe Drought", "Extreme Drought"))
  ) %>% 
  filter(max_level == pop_total & drought_lvl != "None")


# visualize ---------------------------------------------------------------
ggplot(drought_tidy, aes(x = valid_start, y = lvl, fill = lvl, color = lvl)) + 
  geom_density_ridges() + 
  facet_geo(~ state_abb, grid = "us_state_grid2") +
  scale_fill_viridis_d(option = "inferno", begin = .75, end = .25) + 
  scale_color_viridis_d(option = "inferno", begin = .75, end = .25) + 
  labs(
    title = "Total Population Impacted by US Droughts Over Time", 
    subtitle = "2001-2021",
    caption = "The data on droughts are reported weekly, and were most recently updated on July 19, 2021.\n\nData Source: US Drought Monitor\nViz: Allison Koh (@allisonkoh_)",
    x = "",
    y = "",
    fill = "",
    color = ""
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "fira"),
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10), size = 20),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 5, b = 15), size = 15),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, size = 5, color = "gray40"),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )


# save plot  --------------------------------------------------------------
ggsave("2021-w30-plt.png", width = 12, height = 10)

# write alt text ----------------------------------------------------------
write_alt_text(
  "Faceted map plot",
  "US state-level information on total population affected by droughts from 2001-2021",
  "there is a spike in number of people affected by droughts across several states  in the early 2000s",
  "The categories of droughts include abnormally dry, extreme drought, moderate drought, and severe drought. The states with spikes of droughts in the early 2000s include ID, NV, UT, AZ, DC, CT, NJ, DE, and MD.",
  "US Drought Monitor (2001-2021). The data on droughts are reported weekly, and were most recently updated on July 19, 2021. Visualization by Allison Koh (Twitter: @allisonkoh_)"
)

# code graveyard ----------------------------------------------------------
# drought_tidy1 <- drought %>% 
#   mutate(drought_lvl = as.factor(drought_lvl)) %>% 
#   filter(drought_lvl != "None" | drought_lvl != "DO") %>% 
#   filter(valid_start == "2001-07-17" | valid_start == "2021-07-13") %>% 
#   group_by(state_abb, valid_start) %>% 
#   summarize(pop_drought = sum(pop_total, na.rm = T)) %>% 
#   pivot_wider(names_from = valid_start, values_from = pop_drought) %>% 
#   mutate(pop_diff = `2021-07-13` - `2001-07-17`) %>% 
#   rename(state = state_abb) 
# 
# glimpse(drought_tidy1)
# 
# drought_tidy1 %>% 
#   ggplot(aes(x = pop_diff)) + 
#   geom_density()
# range(drought_tidy1$pop_diff)
# 
# drought_tidy %>% 
#   ggplot(aes(x = valid_start, y = pop_drought, color = state_abb)) +
#   geom_point() + 
#   theme(legend.position = "none")
# 
# drought_tidy %>% 
#   ggplot(aes(x = pop_drought)) + 
#   geom_histogram()
# 
# skim(drought_tidy)
# plot_usmap(data = map_df, values = "pop_diff") + 
#   scale_fill_gradient(trans = "log2") +
#   theme(legend.position = "right")

# drought_moar <- drought %>% 
#   filter(drought_lvl == c("None", "DO", "D1"))
# select(state_abb, valid_start, drought_lvl) %>% 
#   filter(valid_start == "2001-07-17" | valid_start == "2021-07-13")
# anim_attempt1 <- plot_usmap(data = map_df, values = "pop_drought") + 
#   labs(title = "Measles Incidence per 100,000 in {frame_time}")+ 
#   transition_states(valid_start)
# 
# animate(anim_attempt1, fps = 1)
# 
# drought_tidy %>% 
#   count(valid_start)
# 
# nrow(map_df)
# drought_tidy %>% ggplot(aes(x = valid_start, y = drought_lvl, group = drought_lvl, height = sum(pop_total, na.rm = T))) + 
#   geom_density_ridges()
# 
# drought_tidy %>% View()
# 
# drought_tidy_long <- uncount(drought_tidy, as.numeric(drought_lvl))
# drought_tidy_long %>% View()
# merge in fips data 
# map_df <- left_join(state_fips, drought_tidy, by = "state") %>% 
#   # select(state_code, pop_diff) %>% 
#   rename(fips = state_code)
# glimpse(map_df)
# state_fips <- tigris::fips_codes %>% 
#   select(state, state_code) %>% 
#   distinct() 
# drought_tidy <- drought %>% 
#   mutate(drought_lvl = as.factor(drought_lvl)) %>% 
#   filter(drought_lvl != "None" | drought_lvl != "DO") %>% 
#   group_by(state_abb, valid_start) %>% 
#   summarize(pop_drought = sum(pop_total, na.rm = T)) %>% 
#   rename(state = state_abb) 
# plot_usmap(regions = "states") + labs(title = "pls", subtitle = "pls", caption = "pls\n\nData Source: US Drought Monitor\nViz: Allison Koh (@allisonkoh_)")
