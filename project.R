rm(list=ls())

library(tidyverse)
library(jsonlite)
library(lubridate)
library(stringi)
library(tidytext)
library(sf)
library(devtools)
library(urbnmapr)   # Please use devtools::install_github("UrbanInstitute/urbnmapr")
library(reticulate)
library(spacyr)
spacy_initialize()  # Please follow CRAN instructions: https://cran.r-project.org/web/packages/spacyr/readme/README.html

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)

###################################
### Downloading Datasets ##########
###################################

# 1 -- 2020 Presidential Elections Results by Zipcode
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
  download.file("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv",
                destfile = "covid-19-polls-master.zip", mode = "wb")

# 4 -- Trump Presidential approval rate
#   -- More details: https://projects.fivethirtyeight.com/trump-approval-ratings/

if (!file.exists("covid_approval.csv"))
  download.file("https://github.com/fivethirtyeight/covid-19-polls/archive/master.zip",
                destfile = "covid_approval.csv", mode = "wb")

# 5 -- Trump's Tweets from 2009 to 2021
#   -- Downloaded file from https://www.thetrumparchive.com/
#   -- The FAQ section provides the following Google Drive link
#   -- https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view


###################################
### Data Wrangling ################
###################################


presidents <- read_csv("1976-2020-president.csv")
glimpse(presidents)

#NAs

sapply(presidents, function(x) sum(is.na(x)))

#Select important variables
pres_margin <- presidents %>%
  select(year, state, state_po, candidatevotes, 
         totalvotes, party_simplified) %>%
  mutate(margin = round(candidatevotes / totalvotes, 3) * 100)
  

states_sf <- get_urbn_map("states", sf = TRUE)

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

spatial_data <- left_join(states_sf, pres_margin,
                          by = c("state_abbv" = "state_po"))

#spatial_data %>%
#  filter(year == 2020 & party_simplified == "REPUBLICAN") %>%
#  ggplot() +
#  geom_sf(mapping = aes(fill = margin),
#          color = "#ffffff", size = 0.25) +
#  labs(fill = "")



###################################
### Plotting ######################
###################################

#Idea: plot party that wins by state and year. Include margin of victory, name of candidate




###################################
### Text Processing ###############
###################################

tweets_df <-
  fromJSON("tweets_01-08-2021.json") %>%
  mutate(date = ymd_hms(date, tz = "EST"),
         text = stri_replace_all_regex(text, "(\\s?)http(s?)://.*$", ""), # Removing URL addresses
         text = stri_replace_all_fixed(text, c("&amp;"), c("and"))) %>%   # Replacing HTML characters
  filter(isRetweet == "f", !text == "")                                   # Removing RTs & empty processed tweets

  
tweets <- tweets_df$text
list_tweets <- list()

for (i in 1:length(tweets)) {
  
  tweet <-
    tweets_df %>%
    filter(text == tweets[i]) %>%
    unnest_tokens(sentence, text, token = "sentences", drop = TRUE)
  
  vec_tweet <- tweet$sentence
  names(vec_tweet) <- 1:length(vec_tweet)
  
  list_tweets[[i]] <-
    spacy_parse(vec_tweet) %>%
    as.tibble() %>%
    select(-sentence_id) %>%
    mutate(tweet = tweets[i])
    }

words_df <-
  do.call("rbind", list_tweets) %>%
  as_tibble() 

###################################
### Analysis ######################
###################################