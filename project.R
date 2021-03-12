rm(list=ls())

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


# 3 -- Coronavirus Polls compiled by FiveThirtyEight about 
#   -- Trump handling Covid-19 approval rate & People's concern rate about Covid-19
#   -- More details: https://projects.fivethirtyeight.com/coronavirus-polls/

if (!file.exists("covid-19-polls-master.zip"))
  download.file("https://github.com/fivethirtyeight/covid-19-polls/archive/master.zip",
                destfile = "covid-19-polls-master.zip", mode = "wb")

# 4 -- Presidential approval rate: Trump
#   -- More details: https://projects.fivethirtyeight.com/trump-approval-ratings/

if (!file.exists("president_approval.csv"))
  download.file("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv",
                destfile = "president_approval.csv", mode = "wb")

# 5 -- 2020 Elections Forecast
#   -- More details: https://github.com/fivethirtyeight/data/tree/master/election-forecasts-2020

if (!file.exists("president2020_forecast.csv"))
  download.file("https://projects.fivethirtyeight.com/2020-general-data/presidential_national_toplines_2020.csv",
                destfile = "president2020_forecast.csv", mode = "wb")

# 6 -- Trump's Tweets from 2009 to 2021
#   -- Downloaded file from https://www.thetrumparchive.com/
#   -- The FAQ section provides the following Google Drive link
#   -- https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view

# 7 -- AFINN sentiment: New English AFINN wordlist
#   -- More details: https://github.com/fnielsen/afinn/tree/master/afinn/data

if (!file.exists("AFINN-en-165.txt"))
  download.file("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt",
                destfile = "AFINN-en-165.txt", mode = "wb")

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

sentiment_afinn <-
  read.delim("AFINN-en-165.txt", header = F) %>%
  as_tibble() %>%
  rename(word = 1, sentiment = 2)


tweets_df <-
  fromJSON("tweets_01-08-2021.json") %>%
  mutate(date = ymd_hms(date, tz = "EST"),
         text = stri_replace_all_regex(text, "(\\s?)http(s?)://.*$", ""),      # Removing URL addresses
         text = str_replace_all(text,                                          # Replacing HTML characters
                                c("&amp(;|,)?" = "and",
                                  "@|#|&|%|$" = "",
                                  "Donald J. Trump" = "Donald J Trump",
                                  '""' = '',
                                  "--" = "",
                                  "\\-+Donald" = "Donald"))) %>%  
  filter(isRetweet == "f", !text == "", date > ymd("2015-06-01")) %>%          # Removing RTs & empty processed tweets
  arrange(date) %>%
  mutate(tweet_id = row_number()) %>%
  select(tweet_id, date, text, device, favorites)

tweets <- tweets_df[,c("tweet_id","text")]
list_tweets <- list()

for (i in 1:nrow(tweets)) {   # ~ Task lasts ~36min
  
  tweet <-
    tweets_df %>%
    filter(text == tweets$text[i]) %>%
    unnest_tokens(sentence, text, token = "sentences", drop = TRUE, to_lower = FALSE)
  
  vec_tweet <- tweet$sentence
  
  list_tweets[[i]] <-
    spacy_parse(vec_tweet, entity = FALSE, additional_attributes = c("is_stop")) %>%
    as_tibble() %>%
    select(-c(doc_id, token_id)) %>%
    mutate(tweet_id = tweets$tweet_id[i])
}

words_df <-
  do.call("rbind", list_tweets) %>%
  as_tibble() %>%
  left_join(tweets_df, by = "tweet_id") %>%
  filter(is_stop == FALSE, !pos == "PUNCT") %>%
  inner_join(sentiment_afinn, by = c("lemma" = "word")) %>%
  mutate(week = floor_date(date, unit = "weeks")) %>%

weekly_sentiment <-
  words_df %>%
  group_by(week) %>%
  summarise(sentiment = mean(sentiment, na.rm = TRUE)) %>%
  ungroup()

remove(tweets, tweet, vec_tweet)


###################################
### Analysis ######################
###################################