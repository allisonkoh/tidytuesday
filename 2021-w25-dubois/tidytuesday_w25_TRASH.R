
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

# devtools::install_github("cbail/textnets")
# devtools::install_github("spcanelon/TidyTuesdayAltText")

pacman::p_load(
  tidyverse, 
  tidytuesdayR,
  DataExplorer,
  textnets,
  TidyTuesdayAltText
)

# goals  ------------------------------------------------------------------
#' visualize a text network with the {textnets} package 
#' use a color palette that matches DuBois' visualizations
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
library(quanteda)
tweet_corp <- quanteda::corpus(tweets_raw, text_field = "content")
tweet_dtm <- dfm(
  tweet_corp,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_separators = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE
) %>% 
  dfm_remove(
    pattern = c(stpwords = "english")
  ) %>% 
  dfm_wordstem()

tweet_dtm %>% View()

prepped_tweets <- PrepText(tweets_raw, 
                           groupvar = "username", 
                           textvar = "content",
                           node_type = "words",
                           tokenizer = "words",
                           pos = "all",
                           remove_stop_words = TRUE,
                           language = "english",
                           compound_nouns = TRUE)

prepped_tweets1 <- prepped_tweets %>% 
  select(lemma, username)

prepped_tweets %>% 
  count(lemma) %>% 
  View()

# visualize ---------------------------------------------------------------
tweet_text_network <- CreateTextnet(prepped_tweets)
dubois_network <- CreateTextnet(prepped_tweets)
VisTextNet(dubois_network, betweenness = TRUE)

# archive -----------------------------------------------------------------
tweet_text_network


