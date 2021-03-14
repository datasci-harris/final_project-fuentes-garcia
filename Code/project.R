rm(list=ls())
options(scipen = 10^6, digits=2)

library(tidyverse)
library(jsonlite)
library(lubridate)
library(sf)
#library(devtools)
library(urbnmapr)   # Please use devtools::install_github("UrbanInstitute/urbnmapr")

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)

###################################
### Data Wrangling ################
###################################

# Loading data
presidents <-
  read_csv(file.path("Input","1976-2020-president.csv")) %>%
  select(-c(state_fips, state_cen, state_ic, office, writein, version)) 

# Verfying NAs presence
sapply(presidents, function(x) sum(is.na(x)))   
# --> NA's associated to party/candidate names from Non-Democrat and Non-Republican parties

# Constructing winners and their margin by state and year
win_margin <-
  presidents %>%
  mutate(margin = round(candidatevotes / totalvotes, 3) * 100) %>%
  group_by(year, state_po) %>%
  mutate(candidate_win = ifelse(margin == max(margin), 1, 0)) %>%
  ungroup()

# Loading Geometry
states_sf <- get_urbn_map("states", sf = TRUE)

# Joining spatial data Margin Election data
spatial_data <-
  left_join(states_sf, win_margin, by = c("state_abbv" = "state_po"))

# Saving intermediate data for laer using in ShinyApp
saveRDS(spatial_data, file = file.path("Intermediate","panel_elections.RDS"))

# Defining Colors
party_colors <- c("#1A6AFF", "#FF4A43")

# Plot Example
ggplot(data = filter(spatial_data, year == 2020 & candidate_win == 1)) +
  geom_sf(aes(fill = party_simplified)) +
  geom_sf_text(aes(label = state_abbv), size = 2, fontface = "bold", color = "white") +
  labs(title = "Election Results 2020 by State", fill = "") +
  scale_fill_manual(values = party_colors)  +
  theme_bw() + 
  theme(legend.position = c(0.92, 0.1),
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),)
    
    
