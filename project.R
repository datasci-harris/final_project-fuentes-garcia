rm(list=ls())

library(tidyverse)
library(tidytext)

# Victor
wd <- "C:/Users/vfuentesc/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"

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





###################################
### Analysis ######################
###################################