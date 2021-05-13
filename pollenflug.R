############################
# © Joshua Ziegler
# May 12, 2021

library(tidyverse)

############################

# import csv (downloaded from https://www.suche-postleitzahl.org/downloads)
plz_DE <- read_csv("zuordnung_plz_ort.csv")

# remove first column
plz_DE <- plz_DE[,-1]

# add region id (taken from https://opendata.dwd.de/climate_environment/health/alerts/Beschreibung_pollen_s31fg.pdf)
plz_DE <-
  plz_DE %>%
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
pollen_data <- jsonlite::flatten(pollen_json$content)

# types of pollen (just for memorisation):
pollentypes <- 
  c("Beifuss", "Graeser", "Roggen", "Esche", "Birke", "Hasel", "Ambrosia", "Erle")

# transform to longer data table
pollen_long <-
  pollen_data %>% 
    pivot_longer(
      cols = starts_with("Pollen"),
      names_to = c("pollentype", "day"),
      names_pattern = "Pollen\\.(.*)\\.(.*)",  # extracted strings in brackets, \\ to escape dots
      values_to = "warninglevel"
    )

############################

# filter by warninglevel
# -1 = missing, 0 = keine Belastung, 0-1 = keine bis geringe Belastung
# 1 = geringe Belastung, 1-2 = geringe bis mittlere Belastung,
# 2 = mittlere Belastung, 2-3 = mittlere bis hohe Belastung, 3 = hohe Belastung

# only mid to high
pollen_mid_high <-
  pollen_long %>%
  filter(warninglevel == "2-3")

# filter PLZ
plz_mid_high <- 
  plz_DE %>%
  filter(region_id %in% pollen_mid_high$region_id)

#######
