rm(list=ls())
options(scipen = 10^6, digits=2)

library(tidyverse)
library(jsonlite)
library(lubridate)
library(stringi)
library(stringr)
library(tidytext)
library(sf)
#library(devtools)
library(urbnmapr)   # Please use devtools::install_github("UrbanInstitute/urbnmapr")
library(reticulate)
library(spacyr)
spacy_initialize()  # Please follow CRAN instructions: https://cran.r-project.org/web/packages/spacyr/readme/README.html

# Victor
#wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)

###################################
### Downloading Datasets ##########
###################################

# 1 -- 2020 Presidential Elections Results by counties
#   -- Data developed usingn The Guardian, townhall.com, Fox News, Politico, and the New York Times
#   -- More details: https://github.com/tonmcg/US_County_Level_Election_Results_08-20

if (!file.exists("results_2020.csv"))
  download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv",
                destfile = "results_2020.csv", mode = "wb")

# 2 -- 1976-2020 Presidential Elections Results by State
#   -- Dataset from MIT Election Data + Science Lab
#   -- More details: https://doi.org/10.7910/DVN/42MVDX

# 3 -- 1980-2020 News Sentiment Analysis
#   -- Data set from Federal Reserve Bank of San Francisco
#   -- More details: https://www.frbsf.org/economic-research/indicators-data/daily-news-sentiment-index/

if (!file.exists("results_2020.csv"))
  download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv",
                destfile = "results_2020.csv", mode = "wb")

# 4 -- Trump's Tweets from 2009 to 2021
#   -- Downloaded file from https://www.thetrumparchive.com/
#   -- The FAQ section provides the following Google Drive link
#   -- https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view

# 5 -- AFINN sentiment: New English AFINN wordlist
#   -- More details: https://github.com/fnielsen/afinn/tree/master/afinn/data

if (!file.exists("news_sentiment_data.xlsx"))
  download.file("https://www.frbsf.org/economic-research/indicators-data/daily-news-sentiment-index/files/news_sentiment_data.xlsx",
                destfile = "news_sentiment_data.xlsx", mode = "wb")

###################################
### Data Wrangling ################
###################################


presidents <- read_csv("1976-2020-president.csv")
glimpse(presidents)

#NAs

sapply(presidents, function(x) sum(is.na(x)))

#Select important variables
pres_margin <- presidents %>%
  select(-c(state_fips, state_cen, state_ic, office, writein, version)) %>%
  mutate(margin = round(candidatevotes / totalvotes, 3) * 100)

#Include dummy for candidate that won that election by year and state
win <- pres_margin %>%
  group_by(year, state_po) %>%
  mutate(candidate_win = ifelse(margin == max(margin), 1, 0)) %>%
  ungroup()

#GEometry  
states_sf <- get_urbn_map("states", sf = TRUE)

#Join dataframes
spatial_data <- left_join(states_sf, win,
                          by = c("state_abbv" = "state_po"))

saveRDS(spatial_data, file = "panel_elections.RDS")

party_colors <- c("blue", "red")


spatial_data %>%
  filter(year == 2020 & candidate_win == 1) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = margin),
          color = "#ffffff", size = 0.25) +
  labs(fill = "")


p = ggplot(data = filter(spatial_data, year == 2020 & candidate_win == 1)) +
  geom_sf(aes(fill = party_simplified)) +
  geom_sf_text(aes(label = state_abbv), 
               size = 2) +
  labs(title = "Election Results 2020", x = NULL, y = NULL,
       fill = NULL) +
  theme_minimal()

p1  = p + scale_color_manual(values = party_colors)  
p1

###################################
### Plotting ######################
###################################

#Idea: plot party that wins by state and year. Include margin of victory, name of candidate




###################################
### Text Processing ###############
###################################

# Sentiment Analysis of Trump's tweets

## Loading AFINN
sentiment_afinn <-
  read.delim("AFINN-en-165.txt", header = F) %>%
  as_tibble() %>%
  rename(word = 1, sentiment = 2)

## Wrangling set of tweets
tweets_df <-
  fromJSON("tweets_01-08-2021.json") %>%
  mutate(
    date = ymd_hms(date, tz = "EST"),
    text = stri_replace_all_regex(text, "(\\s?)http(s?)://.*$", ""),      # Removing URL addresses
    text = str_replace_all(text,                                          
                           c(     "&amp(;|,)?" = "and",               # Replacing HTML characters
                             '@|#|&|%|$|""|--' = "",                  # Removing symbols
                             "Donald J. Trump" = "Donald J Trump",    # standardizing Middle Name
                                  "\\-+Donald" = "Donald"))           # Formatting Trump Self-Cites
    ) %>%  
  filter(
    isRetweet == "f",                                           # Removing RTs
    !text == "",                                                # Removing empty processed tweets
    date > ymd("2015-01-01")                                    # Keeping tweets starting in 2015
    ) %>%          
  arrange(date) %>%
  mutate(tweet_id = row_number()) %>%
  select(tweet_id, date, text, device, favorites)


## Constructing list of words from tweets
tweets <- tweets_df[,c("tweet_id","text")]
list_tweets <- list()

for (i in 1:nrow(tweets)) {   # ~ Task lasts ~36min
  
  tweet <-
    tweets_df %>%
    filter(text == tweets$text[i]) %>%
    unnest_tokens(sentence, text, token = "sentences", drop = TRUE)
  
  vec_tweet <- tweet$sentence
  
  list_tweets[[i]] <-
    spacy_parse(vec_tweet, additional_attributes = c("is_stop")) %>%
    as_tibble() %>%
    select(-c(doc_id, token_id)) %>%
    mutate(tweet_id = tweets$tweet_id[i])
}

save(list_tweets, file = "list_tweets.RData")
load("list_tweets.RData")

## Developing sentiment analysis from non-stop words
trump_sentiment <-
  do.call("rbind", list_tweets) %>%
  as_tibble() %>%
  left_join(tweets_df, by = "tweet_id") %>%
  filter(is_stop == FALSE, !pos == "PUNCT") %>%
  inner_join(sentiment_afinn, by = c("lemma" = "word")) %>%
  mutate(week = as.Date(floor_date(date, unit = "week"))) %>%
  group_by(week) %>%
  summarise(value = mean(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subject = "Trump",
         value = zoo::rollmean(value, k = 4, fill = NA, align = "right"))

remove(tweets, tweet, vec_tweet)

# Sentiment of 16 major US newspapers
news_sentiment <-
  readxl::read_excel("news_sentiment_data.xlsx", sheet = "Data") %>%
  mutate(week = floor_date(ymd(date), unit = "weeks")) %>%
  rename(sentiment = `News Sentiment`) %>%
  group_by(week) %>%
  summarise(value = mean(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subject = "News")

# Merging both sentiments

sentiment_analysis <-
  rbind(trump_sentiment, news_sentiment) %>%
  filter(week > ymd("2020-01-01")) %>%
  group_by(subject) %>%
  mutate(scaled = scale(value))

ggplot(sentiment_analysis, aes(x = week, y = scaled, color = subject, group = subject)) +
  geom_line()

###################################
### Analysis ######################
###################################
