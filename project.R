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
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
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
  mutate(date = as.Date(date),
         week = floor_date(date, "weeks")) %>%
  group_by(week) %>%
  summarise(value = mean(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subject = "Trump")


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
  filter(between(week, ymd("2018-01-01"), ymd("2020-11-30"))) %>%         # Keeping only last 3 years
  group_by(subject) %>%
  mutate(scaled = scale(value),
         scaled_smoothed = zoo::rollmean(scaled, k = 6,
                                         fill = NA, align = "right")) %>%
  ungroup()

# Creating Sentiment plot: Trump vs. US News
sentiment_analysis_plot <-
  ggplot(sentiment_analysis, aes(x = week, y = scaled_smoothed, color = subject, group = subject)) +
  geom_hline(yintercept = 0, color = "grey70", size = 1) +
  geom_line(size = 1.0) + 
  annotate(geom = "text",                                                                # Trump legend
           x = as.Date("2018-10-07"), y = 1.3, color = "red", fontface = "bold",  
           label = "Trump", hjust = "center", vjust = "right") +
  annotate(geom = "text",
           x = as.Date("2019-05-07"), y = 0.95, color = "blue", fontface = "bold",       # US News legend
           label = "US News", hjust = "center", vjust = "right") +
  annotate(geom = "curve",                                                               # Adding Flags
           x = as.Date("2020-01-30"), y = 0, xend = as.Date("2020-01-01"), yend = -1,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-01-01"), y = -1,
           label = "JAN.30\nThe WHO declared\na global health nemergency", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-03-15"), y = 0, xend = as.Date("2020-03-01"), yend = 1.1,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-03-01"), y = 1.1,
           label = "MAR.15\nCDC recommended\nno gatherings of 50+ people", hjust = "center", vjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-03-26"), y = 0, xend = as.Date("2020-04-28"), yend = -0.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-04-28"), y = -0.5,
           label = "MAR.26\nUSA led the world\nin confirmed\ncases: 81k", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-05-27"), y = 0, xend = as.Date("2020-05-30"), yend = 1.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-05-30"), y = 1.5,
           label = "MAY.27\nCOVID-19 deaths\nin the US\npassed 100k", hjust = "center", vjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-06-01"), y = 0, xend = as.Date("2020-08-30"), yend = 1.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-08-30"), y = 1.5,
           label = "MAY/JUN.\nBlack Lives\nMatters protests", hjust = "center", vjust = "left") +
  annotate(geom = "curve",
           x = as.Date("2020-07-13"), y = 0, xend = as.Date("2020-07-07"), yend = -0.6,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-07-07"), y = -0.6,
           label = "JUL.13\n5M+ Americans\nlost health insurance\ndue to job losses", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-08-01"), y = 0, xend = as.Date("2020-09-15"), yend = -0.2,
           curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-09-15"), y = -0.2,
           label = "AUG.1\nOnly in July\nthe USA recorded\n1.9M new\ncases", hjust = "center", vjust = "right") +
  annotate(geom = "curve",
           x = as.Date("2020-11-03"), y = 0, xend = as.Date("2020-10-30"), yend = 1.5,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = as.Date("2020-10-30"), y = 1.5,
           label = "NOV.3\nPresidential\nElections", hjust = "center", vjust = "left") +
  scale_color_manual(values = c("blue", "red")) +
  scale_y_continuous(limits = c(-2.5, 2.0), breaks = seq(-2.5, 2.0, 0.5)) +
  scale_x_date(date_breaks = "2 months",
               date_labels = format("%b-%Y"),
               limits = as.Date(c("2018-01-01","2020-11-30")),
               expand = c(0,0)) +
  labs(x = "", color = "", y = "Sentiment (scaled and smoothed)*",
       title    = "Trump lost the track of reality during COVID-19",
       subtitle = "Sentiment Analysis: Trump's tweets vs. 16 Major US News, Jan 2018 - Nov 2020",
       caption  = paste0("Source: Trump's tweets copiled by TheTrumpArchive.com. Sentiment calculated by Authors.",
                         "News' Sentiment calculated by Federal Reserve Bank of San Francisco.\n",
                         "Timeline flags from New York Times: A timeline of the Coronavirus Pandemic.\n",
                         "*Scale to mean = 0. Smoothness through rolling average 6 weeks.")) +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_text(size = 10, face="bold", colour = "black"),
        axis.title = element_text(size = 13, face="bold", colour = "black"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5))

## -- News flags from The New York Times
## -- More details: https://www.nytimes.com/article/coronavirus-timeline.html

# Saving plot
ggsave("sentiment_analysis_plot.png", sentiment_analysis_plot, width = 20, height = 8.7)

###################################
### Analysis ######################
###################################
