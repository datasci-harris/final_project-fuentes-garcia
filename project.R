rm(list=ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidytext)
library(sf)
library(urbnmapr)

# Victor
#wd <- "C:/Users/vfuentesc/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"

# Fernando
wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"

setwd(wd)

###################################
### Data Wrangling ################
###################################

presidents <- read_csv("1976-2020-president.csv")
glimpse(president_ts)

#NAs

sapply(presidents, function(x) sum(is.na(x)))

#Select important variables
pres_margin <- presidents %>%
  select(year, state, state_po, candidatevotes, 
         totalvotes, party_simplified) %>%
  mutate(margin = round(candidatevotes / totalvotes, 3) * 100)
  

# Urban Institute: https://urbaninstitute.github.io/urbnmapr/
devtools::install_github("UrbanInstitute/urbnmapr")

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





###################################
### Analysis ######################
###################################