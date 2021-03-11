rm(list=ls())

library(tidyverse)
library(jsonlite)
library(lubridate)
library(tidytext)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"

# Fernando
#wd <- "C:/Users/vfuentesc/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"

setwd(wd)

###################################
### Data Wrangling ################
###################################

# --- Downloading 2020 Presidential Elections Results from Github

if (!file.exists("results_2020.csv"))
download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv",
              destfile = "results_2020.csv", mode = "wb")

# --- Downloading Previous Results

###################################
### Plotting ######################
###################################






###################################
### Text Processing ###############
###################################

# ---- Downloaded files from FiveThirtyEight about 
## --- Trump handling Covid-19 approval rate & People's concern rate about Covid-19
## --- More details: https://projects.fivethirtyeight.com/coronavirus-polls/

if (!file.exists("covid-19-polls-master.zip"))
  download.file("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv",
                destfile = "covid-19-polls-master.zip", mode = "wb")

## --- Trump Presidential approval rate
## --- More details: https://projects.fivethirtyeight.com/trump-approval-ratings/

if (!file.exists("covid_approval.csv"))
  download.file("https://github.com/fivethirtyeight/covid-19-polls/archive/master.zip",
                destfile = "covid_approval.csv", mode = "wb")

# ---- Downloaded file from https://www.thetrumparchive.com/
## --- The FAQ section provides the following Google Drive link
## --- https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view

tweets_df <-
  fromJSON("tweets_01-08-2021.json") %>%
  mutate(date = ymd_hms(date, tz = "EST")) %>%
  filter(isRetweet == "f")          # Removing RTs


###################################
### Analysis ######################
###################################