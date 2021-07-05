## ---------------------------
##
## TIDY TUESDAY - WEEK 23 (`survivoR`) - Half Baked Random Forest  
##
## Author: Allison Koh
##
## Date Created: 2021-05-30
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

## ---------------------------
##
## Errors
##
## Couldn't load usemodels; was retrieving the wrong version of `recipes` for some reason  
## 
##
## --------------------------

## setup ---------------------

# load dependencies 
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  devtools,
  tidytuesdayR,
  viridis,
  tidymodels, 
  recipes,
  doParallel,
  survivoR, # week 23 not available to load from tidytuesdayR atm 
  usmap, # state-level data for region
  ranger, # something to do with RF modeling 
  vip # assess variable importance of best RF model 
)

# load tidymodels add-ons from github

# devtools::install_github("tidymodels/usemodels") # didn't work!*
devtools::install_github("tidymodels/themis")

## load data ----------------------
tt23_raw <- survivoR::castaways

## wrangle ----------------------

# retrieve state abbreviations
state_abbr <- usmap::statepop %>%
  select(full, abbr) %>% 
  rename(state = full)

# add variables: sole survivor, jury member, repeat offender, number of seasons on survivor
tt23_1 <- tt23_raw %>% 
  group_by(full_name) %>% 
  mutate(no_seasons = n()) %>% 
  ungroup() %>% 
  mutate(
    sole_survivor = if_else(result == "Sole Survivor", 1, 0),
    jury_member = if_else((!is.na(jury_status) | sole_survivor == 1), 1, 0), 
    repeat_offender = if_else(no_seasons > 1, 1, 0)
  ) 

# add row ID variable before creating final dataset 
tt23_1$row_id <- 1:nrow(tt23_1)

# add in state abbreviations and regions, also convert character vectors to factors
tt23_2 <- left_join(tt23_1, state_abbr, by = "state") %>% 
  rename(state_abbr = abbr) %>% 
  mutate(
    state_abbr = replace_na(state_abbr, "DC"),
    region = case_when(
      state_abbr %in% usmap::.northeast_region ~ "Northeast",
      state_abbr %in% usmap::.south_region ~ "South", 
      state_abbr %in% usmap::.midwest_region ~ "Midwest",
      state_abbr %in% usmap::.west_region ~ "West"
    )
  ) %>% 
  mutate_if(is.character, factor)

# subset final dataset for modeling purposes 
tt23 <- tt23_2 %>% 
  select(c(
    "sole_survivor", 
    "row_id",
    "full_name", 
    "age", 
    "region", 
    "personality_type",
    "immunity_idols_won",
    "no_seasons",
    "repeat_offender",
    )) %>% 
  purrr::modify_at(c("sole_survivor", "region", "personality_type", "immunity_idols_won", "no_seasons", "repeat_offender"), factor)

tt23j <- tt23_2 %>% 
  select(c(
    "jury_member", 
    "row_id",
    "full_name", 
    "age", 
    "region", 
    "personality_type",
    "immunity_idols_won",
    "no_seasons",
    "repeat_offender",
  )) %>% 
  purrr::modify_at(c("jury_member", "region", "personality_type", "immunity_idols_won", "no_seasons", "repeat_offender"), factor)


## explore ----------------------

# sole survivor status   

## age distribution 
tt23 %>% 
  ggplot(aes(x = age, group = sole_survivor, fill = sole_survivor)) + 
  geom_density(adjust = 1.5, alpha = 0.4) + 
  theme(legend.position = "bottom")

## personality type  
tt23 %>% 
  filter(sole_survivor == 1) %>% 
  ggplot(aes(x = personality_type)) + 
  geom_bar() + 
  coord_flip()

## immunity idols won 
tt23 %>% 
  filter(sole_survivor == 1) %>% 
  ggplot(aes(x = immunity_idols_won)) + 
  geom_bar() 

## number of seasons 
tt23 %>% 
  filter(sole_survivor == 1) %>% 
  ggplot(aes(x = no_seasons)) + 
  geom_bar()

# jury member status 

## age dist 
tt23 %>% 
  ggplot(aes(x = age, group = jury_member, fill = jury_member)) + 
  geom_density(adjust = 1.5, alpha = 0.4) + 
  theme(legend.position = "bottom")

## personality type 
tt23 %>% 
  filter(jury_member == 1) %>% 
  ggplot(aes(x = personality_type)) + 
  geom_bar() + 
  coord_flip()

## immunity idols won
tt23 %>% 
  filter(jury_member == 1) %>% 
  ggplot(aes(x = immunity_idols_won)) + 
  geom_bar() 

## number of seasons 
tt23 %>% 
  filter(jury_member == 1) %>% 
  ggplot(aes(x = no_seasons)) + 
  geom_bar()


## sole survivor: models and evaluation ----------------------

# set up model 

## train, test, validation sets
set.seed(3047)
tt23_split <- initial_split(tt23, strata = sole_survivor)
tt23_train <- training(tt23_split)
tt23_test <- testing(tt23_split)

set.seed(347)
tt23_folds <- vfold_cv(tt23_train, strata = sole_survivor)
tt23_folds

## set up model 
ss_rec <- recipe(sole_survivor ~ ., data = tt23_train) %>% 
  update_role(row_id, new_role = "ID") %>% 
  step_dummy(repeat_offender) %>% 
  step_downsample(sole_survivor)

ss_prep <- prep(ss_rec)
juiced <- juice(ss_prep)

## create model specification 
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000, 
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

## put everything together in a workflow :D 
tune_wf <- workflow() %>% 
  add_recipe(ss_rec) %>%
  add_model(tune_spec)

## train hyperparameters 
set.seed(3046)
tune_res <- tune_grid(
  tune_wf,
  resamples = tt23_folds,
  grid = 20
)

# evaluation 

## looking at AUC 
tune_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  select(mean, min_n, mtry) %>% 
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>% 
  ggplot(aes(value, mean, color = parameter)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~ parameter, scales = "free_x") +
  labs(x = "NULL", y = "AUC")

rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)

set.seed(30456)
regular_res <- tune_grid(
  tune_wf,
  resamples = tt23_folds,
  grid = rf_grid
)

## look at results before selecting final RF 
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

## choosing the best model 
best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

## vip 
sole_final_rf <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(sole_survivor ~ .,
      data = juice(ss_prep) %>% select(-row_id, -full_name)
  ) %>%
  vip(geom = "point")

## est final workflow 

final_wf <- workflow() %>%
  add_recipe(ss_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(tt23_split)

final_res %>%
  collect_metrics()


## making it to jury: models and evaluation ----------------------

# set up model 

## train, test, validation sets
set.seed(2048)
tt23j_split <- initial_split(tt23j, strata = jury_member)
tt23j_train <- training(tt23j_split)
tt23j_test <- testing(tt23j_split)

set.seed(2047)
tt23j_folds <- vfold_cv(tt23j_train, strata = jury_member)
tt23j_folds

## set up model 
j_rec <- recipe(jury_member ~ ., data = tt23j_train) %>% 
  update_role(row_id, new_role = "ID") %>% 
  step_dummy(repeat_offender) %>% 
  step_downsample(jury_member) %>% 
  step_rm(personality_type)

j_prep <- prep(j_rec)
juiced <- juice(j_prep)

## create model specification 
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000, 
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

## put everything together in a workflow :D 
tune_wf <- workflow() %>% 
  add_recipe(j_rec) %>%
  add_model(tune_spec)

## train hyperparameters 
set.seed(2049)
tune_res <- tune_grid(
  tune_wf,
  resamples = tt23j_folds,
  grid = 20
)

# evaluation 

## looking at AUC 
tune_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  select(mean, min_n, mtry) %>% 
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>% 
  ggplot(aes(value, mean, color = parameter)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~ parameter, scales = "free_x") +
  labs(x = "NULL", y = "AUC")

rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)

set.seed(20486)
regular_res <- tune_grid(
  tune_wf,
  resamples = tt23j_folds,
  grid = rf_grid
)

## look at results before selecting final RF 
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

## choosing the best model 
best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

## vip 
jury_final_rf <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(jury_member ~ .,
      data = juice(j_prep) %>% select(-row_id, -full_name)
  ) %>%
  vip(geom = "point")

## est final workflow 

final_wf <- workflow() %>%
  add_recipe(j_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(tt23j_split)

final_res %>%
  collect_metrics()

## final visualizations ----------------------

# rf results 

## sole survivor - strongest predictors are immunity idols won and "repeat offender"
sole_final_rf

## jury - strongest predictors are age and immunity idols won 
jury_final_rf 

# heatmaps 

## making it to jury - tt23j

tt23j_grouped <- tt23j %>% 
  group_by(immunity_idols_won, age, jury_member) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(immunity_idols_won = as.numeric(immunity_idols_won))

ggplot(tt23j_grouped, aes(x = age, y = immunity_idols_won)) + 
  geom_density(stat = "identity") + 
  facet_wrap(~ jury_member)

## archived code ----------------------

# tt23 %>% 
#   group_by(personality_type, sole_survivor) %>% 
#   summarize(n = n()) %>% 
#   ggplot(aes(x = personality_type, y = n, fill = sole_survivor)) + 
#   geom_bar(position = "fill", stat = "identity") + 
#   coord_flip()

# ggplot(aes(x = personality_type, fill = sole_survivor)) + 
# geom_bar(position = "fill")

## set up modeling code 
# ranger_recipe <-
#   recipe(
#       formula = sole_survivor ~ age + region + personality_type + immunity_idols_won + no_seasons + repeat_offender + row_id, 
#       data = tt23_train
#   ) %>%
#   update_role(row_id, new_role = "id") %>%

# step_string2factor(sole_survivor, region, personality_type) %>% 
# step_scale(age)

# ranger_spec <-
#   rand_forest(trees = 1000) %>%
#   set_mode("classification") %>%
#   set_engine("ranger")
# 
# ranger_workflow <-
#   workflow() %>%
#   add_recipe(ranger_recipe) %>%
#   add_model(ranger_spec)
# 
# doParallel::registerDoParallel()
# set.seed(3046346)
# ranger_rs <- 
#   fit_resamples(
#     ranger_workflow, 
#     resamples = tt23_folds, 
#     control = control_resamples(save_pred = TRUE)
#   )

