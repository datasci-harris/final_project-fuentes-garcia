rm(list=ls())

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

if (!file.exists(file.path("Input", "results_2020.csv")))
  download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv",
                destfile = file.path("Input","results_2020.csv"), mode = "wb")

# 2 -- 1976-2020 Presidential Elections Results by State
#   -- Dataset from MIT Election Data + Science Lab
#   -- More details: https://doi.org/10.7910/DVN/42MVDX

if (!file.exists(file.path("Input", "1976-2020-president.csv")))
  cat("File 1976-2020-president.csv needed but absent")

# 3 -- 1980-2020 News Sentiment Analysis
#   -- Data set from Federal Reserve Bank of San Francisco
#   -- More details: https://www.frbsf.org/economic-research/indicators-data/daily-news-sentiment-index/

if (!file.exists(file.path("Input", "news_sentiment_data.xlsx")))
  download.file("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv",
                destfile = file.path("Input", "news_sentiment_data.xlsx"), mode = "wb")

# 4 -- Trump's Tweets from 2009 to 2021
#   -- Downloaded file from https://www.thetrumparchive.com/
#   -- The FAQ section provides the following Google Drive link
#   -- https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view

if (!file.exists(file.path("Input", "tweets_01-08-2021.json")))
  cat("File tweets_01-08-2021.json needed but absent")

# 5 -- AFINN lexicon: New English AFINN wordlist
#   -- More details: https://github.com/fnielsen/afinn/tree/master/afinn/data

if (!file.exists(file.path("Input", "AFINN-en-165.txt")))
  download.file("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt",
                destfile = file.path("Input", "AFINN-en-165.txt"))