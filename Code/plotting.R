rm(list=ls())
options(scipen = 10^6)

library(tidyverse)
library(urbnmapr)
library(sf)
library(viridis)
library(ggmap)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)

# Loading 2020 Elections Results Data by County
election_2020 <- read_csv(file.path("Intermediate", "election_2020_county.csv"))

# Loading Spatial information by County
county_sf <- get_urbn_map("counties", sf = TRUE)

# Merging datasets
results_county <-
  county_sf %>%
  left_join(election_2020, by = c("county_fips" = "GEOID")) 

# Plotting
results_county %>%
  ggplot() +
  geom_sf(aes(fill = gop_margin), size = 0.5) +
  theme_minimal() +
  scale_fill_viridis(option = "magma", direction = 1) +
  theme_nothing(legend = TRUE)

