## ---------------------------
##
## TidyTuesday 2021-w23; survivoR  
##
## Author: Allison Koh
##
## Date Created: 2021-05-31
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------


# jobs to be done ---------------------------------------------------------
#' graph overview of winners across all seasons 
#' maybe graph by # of challenges won and place? 


# setup / load dependencies -----------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  survivoR,
  viridis,
)

# import ------------------------------------------------------------------
castaways_raw <- survivoR::castaways
challenges_raw <- survivoR::challenges

# initial wrangling -------------------------------------------------------
challenges <- challenges_raw %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(column_label = as.character(1:nrow(.))) 

challenge_winners_left <- bind_rows(challenges$winners, .id = "column_label") %>%
  rename(
    tribe = winning_tribe,
    castaway = winners
  ) 

challenge_winners <- left_join(challenge_winners_left, challenges, by = "column_label") %>% 
  group_by(castaway, season) %>% 
  tally() %>% 
  mutate(
    castaway_season = glue::glue("{castaway}_{season}")
  ) 

castaways_left <- castaways_raw %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(
    castaway_season = glue::glue("{castaway}_{season}")
  )

castaways <- left_join(castaways_left, challenge_winners, by = c("castaway", "season", "castaway_season")) %>% 
  rename(challenges_won = n)

# inspect -----------------------------------------------------------------
dim(castaways)
names(castaways)
glimpse(castaways)

# moar wrangling! ---------------------------------------------------------
result_levels <- c(
  "1st voted out", "2nd voted out", "3rd voted out",
  "4th voted out", "5th voted out", "6th voted out",       
  "7th voted out", "8th voted out", "9th voted out",
  "10th voted out", "11th voted out", "12th voted out",
  "13th voted out", "14th voted out", "15th voted out",
  "16th voted out", "17th voted out", "18th voted out",
  "Runner-up", "Sole Survivor"
)

castaways_grouped <- castaways %>% 
  group_by(result, challenges_won) %>% 
  tally() %>%
  complete(result, nesting(challenges_won), fill = list(n = 0)) %>% 
  filter(
    result != "Ejected" & result != "Eliminated" & result != "Medically evacuated" & 
    result != "Quit" & result != "Switched" & result != "WithdrewFamily"
  ) %>% 
  mutate(
    result = recode(
      result, 
      "Co-runner-up" = "Runner-up",
      "2nd Runner-up" = "Runner-up",
      "2nd runner-up" = "Runner-up"
    ),
    result = fct_relevel(result, result_levels)
  )

# final plot --------------------------------------------------------------
castaways_grouped %>% 
  ggplot(aes(x = result, y = challenges_won, fill = n)) + 
  geom_tile() + 
  scale_fill_viridis(option = "C") + 
  labs(
    title = "Survivor: Challenges Won by Result",
    fill = "",
    caption = "Visualization: Allison Koh | Source: {survivoR} by Daniel Oehm"
  ) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("2021-w23-final-plot.png", width = 8, height = 4)

# archive -----------------------------------------------------------------
# challenge_winners <- left_join(challenge_winners_left, challenges, by = "column_label") %>% 
#   select()
# mutate(

#   castaway_tribe = glue::glue("{castaway}_{tribe}")
# ) %>% 
# group_by(castaway_tribe) %>% 
# tally()

# castaways_left <- castaways_raw %>% 
  # mutate_if(is.character, as.factor) %>% 
  # pivot_longer(
  #   contains("tribe"),
  #   names_to = "tribe_type",
  #   values_to = "tribe"
  # ) %>% 
  # mutate(
  #   castaway_season = glue::glue("{castaway}_{season}")
  # )


