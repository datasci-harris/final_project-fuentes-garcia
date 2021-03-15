rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)

##########################################################
# US Presidential Election Results by state, 1976 - 2020 #
##########################################################

college_bystate <-
  read_csv(file.path("Input","1976-2020-electoral-college.csv")) %>%
  rename(year = column_label) %>%
  mutate(state_name = str_replace(state_name, " of ", " Of "))

# Loading data
panel_elections <-
  read_csv(file.path("Input","1976-2020-president.csv")) %>%
  rename(state_lab = state_po, party = party_simplified, absolute = candidatevotes) %>%
  mutate(share = absolute / totalvotes,
         party = str_to_title(party),
         state = str_to_title(state),
         candidate = ifelse(candidate == "MITT, ROMNEY", "ROMNEY, MITT", candidate)) %>%
  filter(party %in% c("Democrat", "Republican")) %>%
  select(year, state, state_lab, candidate, party, share, absolute) %>%
  pivot_longer(cols = c(share, absolute), names_to = "type", values_to = "votes") %>%
  group_by(year, state, state_lab, type) %>%
  mutate(winner = ifelse(votes == max(votes), 1L, 0L)) %>%
  ungroup() %>%
  left_join(college_bystate, by = c("year" = "year", "state" = "state_name"))              # Adding Electoral Votes data


# Saving intermediate data
write_csv(panel_elections, file.path("Intermediate", "1976-2020_panel_elections.csv"))

