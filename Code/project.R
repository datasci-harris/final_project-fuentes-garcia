rm(list=ls())

library(tidyverse)
library(lubridate)
library(sf)
#library(devtools)
library(urbnmapr)   # Please use devtools::install_github("UrbanInstitute/urbnmapr")
library(rvest)

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


# Web scrapping for electoral votes by year
baseurl <- "https://www.archives.gov/electoral-college/"
year <- seq(1976, 2020, by = 4)

url_all <- vector("double", length(year))
names(url_all) <- year

for(i in 1:length(year)){
  url_all[i] <- paste0(baseurl, year[i]) 
}

get <- function(url){
  request <- read_html(url)
  table <- html_table(request, fill = TRUE)
  ev <- table[[2]][2:53,1:2]
  ev[, 1] <- str_replace_all(ev[, 1], "\\*+", "")
  ev[, 1] <- str_replace_all(ev[, 1], " $", "")
  colnames(ev) <- c("state_name", "electoral_votes")  
  return(ev)
} 

df <- bind_rows(map(url_all, get), .id = "column_label")
df$column_label <- as.double(df$column_label)
df <- df %>%
  filter(state_name != "Total")

#Join spatial data with the electoral votes 
spatial_df <- left_join(spatial_data, df, 
                        by = c("state_name", "year" = "column_label"))

sapply(spatial_df, function(x) sum(is.na(x)))

spatial_df <- spatial_df %>%
  filter(!is.na(candidate))

spatial_df[spatial_df$candidate == "MITT, ROMNEY", ] <- spatial_df[spatial_df$candidate == "ROMNEY, MITT", ]

# Saving intermediate data for laer using in ShinyApp
saveRDS(spatial_df, file = "panel_elections.RDS")

