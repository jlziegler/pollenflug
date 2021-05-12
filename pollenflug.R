###########
# © Joshua Ziegler
# May 12, 2021

library(tidyverse)

# import csv
plzDE <- read_csv("zuordnung_plz_ort.csv")

# remove first column
plzDE <- plzDE[,-1]

# add region id (taken from https://opendata.dwd.de/climate_environment/health/alerts/Beschreibung_pollen_s31fg.pdf)
plzDE <-
  plzDE %>%
  mutate(region_id = case_when(str_detect(bundesland, "^Sch") | str_detect(bundesland, "^Ham") ~ 10,
                               str_detect(bundesland, "^Mec") ~ 20,
                               str_detect(bundesland, "^Nie") | str_detect(bundesland, "^Bre") ~ 30,
                               str_detect(bundesland, "^Nor") ~ 40,
                               str_detect(bundesland, "^Bra") | str_detect(bundesland, "^Ber") ~ 50,
                               str_detect(bundesland, "^Sachsen-") ~ 60,
                               str_detect(bundesland, "^Thü") ~ 70,
                               str_detect(bundesland, "Sachsen") ~ 80,
                               str_detect(bundesland, "^Hes") ~ 90,
                               str_detect(bundesland, "^Rhe") | str_detect(bundesland, "^Saa") ~ 100, 
                               str_detect(bundesland, "^Bad") ~ 110,
                               str_detect(bundesland, "^Bay") ~ 120,
                               
    
                              )
         )

############################

# import JSON
pollen_json <-
  jsonlite::fromJSON("https://opendata.dwd.de/climate_environment/health/alerts/s31fg.json")

# create data table with pollen data
pollen_data <- as.data.frame(pollen_json$content)

# types of pollen (just for memorisation):
pollentypes <- 
  c("Beifuss", "Graeser", "Roggen", "Esche", "Birke", "Hasel", "Ambrosia", "Erle")

# 

#######
