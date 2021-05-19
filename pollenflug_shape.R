# map shapefile

library(sf)
library(tidyverse)

# import shapefiles
plz <- read_sf("plz-gebiete.shp/plz-gebiete.shp") # https://www.suche-postleitzahl.org/downloads
pollenflug <- read_sf("Pollenfluggebiete/PollenfluggebietePolygon.shp") # https://maps.dwd.de/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0&filter=false

# Find all 37 intersections
subregion_91 <- st_intersects(pollenflug, plz)[[1]]
subregion_92 <- st_intersects(pollenflug, plz)[[2]]
subregion_101 <- st_intersects(pollenflug, plz)[[3]]
subregion_71 <- st_intersects(pollenflug, plz)[[4]]
subregion_81 <- st_intersects(pollenflug, plz)[[5]]
subregion_112 <- c(st_intersects(pollenflug, plz)[[6]], 
                   st_intersects(pollenflug, plz)[[8]])
subregion_113 <- st_intersects(pollenflug, plz)[[7]]
subregion_123 <- st_intersects(pollenflug, plz)[[9]]
subregion_121 <- c(st_intersects(pollenflug, plz)[[10]], 
                   st_intersects(pollenflug, plz)[[11]])
subregion_122 <- st_intersects(pollenflug, plz)[[12]]
subregion_20 <- c(st_intersects(pollenflug, plz)[[13]], 
                  st_intersects(pollenflug, plz)[[17]])
subregion_82 <- st_intersects(pollenflug, plz)[[14]]
subregion_72 <- st_intersects(pollenflug, plz)[[15]]
subregion_103 <- st_intersects(pollenflug, plz)[[16]]
subregion_61 <- st_intersects(pollenflug, plz)[[18]]
subregion_31 <- c(st_intersects(pollenflug, plz)[[19]], 
                  st_intersects(pollenflug, plz)[[26]],
                  st_intersects(pollenflug, plz)[[29]],
                  st_intersects(pollenflug, plz)[[30]])
subregion_124 <- st_intersects(pollenflug, plz)[[20]]
subregion_111 <- st_intersects(pollenflug, plz)[[21]]
subregion_12 <- c(st_intersects(pollenflug, plz)[[22]], 
                  st_intersects(pollenflug, plz)[[27]], 
                  st_intersects(pollenflug, plz)[[28]],
                  st_intersects(pollenflug, plz)[[33]])
subregion_50 <- st_intersects(pollenflug, plz)[[23]]
subregion_102 <- c(st_intersects(pollenflug, plz)[[24]], 
                   st_intersects(pollenflug, plz)[[37]])
subregion_62 <- st_intersects(pollenflug, plz)[[25]]
subregion_41 <- st_intersects(pollenflug, plz)[[31]]
subregion_11 <- st_intersects(pollenflug, plz)[[32]]
subregion_32 <- st_intersects(pollenflug, plz)[[34]]
subregion_43 <- st_intersects(pollenflug, plz)[[35]]
subregion_42 <- st_intersects(pollenflug, plz)[[36]]

# plz to data frame
plz_DE <- as.data.frame(plz)
# remove geometry
plz_DE <- plz_DE[,-3]

# add column "ID" to plz_DE
plz_DE<- plz_DE %>% mutate(ID = row_number())

# attach subregion ID by row ID, 
# save in separate columns bc of multiple regions per plz (prevent overwriting)
plz_DE <- plz_DE %>% mutate(subreg_101 = case_when(ID %in% subregion_101 ~ 101),
                      subreg_102 = case_when(ID %in% subregion_102 ~ 102),
                      subreg_103 = case_when(ID %in% subregion_103 ~ 103),
                      subreg_11 = case_when(ID %in% subregion_11 ~ 11),
                      subreg_111 = case_when(ID %in% subregion_111 ~ 111),
                      subreg_112 = case_when(ID %in% subregion_112 ~ 112),
                      subreg_113 = case_when(ID %in% subregion_113 ~ 113),
                      subreg_12 = case_when(ID %in% subregion_12 ~ 12),
                      subreg_121 = case_when(ID %in% subregion_121 ~ 121),
                      subreg_122 = case_when(ID %in% subregion_122 ~ 122),
                      subreg_123 = case_when(ID %in% subregion_123 ~ 123),
                      subreg_124 = case_when(ID %in% subregion_124 ~ 124),
                      subreg_20 = case_when(ID %in% subregion_20 ~ 20),
                      subreg_31 = case_when(ID %in% subregion_31 ~ 31),
                      subreg_32 = case_when(ID %in% subregion_32 ~ 32),
                      subreg_41 = case_when(ID %in% subregion_41 ~ 41),
                      subreg_42 = case_when(ID %in% subregion_42 ~ 42),
                      subreg_43 = case_when(ID %in% subregion_43 ~ 43),
                      subreg_50 = case_when(ID %in% subregion_50 ~ 50),
                      subreg_61 = case_when(ID %in% subregion_61 ~ 61),
                      subreg_62 = case_when(ID %in% subregion_62 ~ 62),
                      subreg_71 = case_when(ID %in% subregion_71 ~ 71),
                      subreg_72 = case_when(ID %in% subregion_72 ~ 72),
                      subreg_81 = case_when(ID %in% subregion_81 ~ 81),
                      subreg_82 = case_when(ID %in% subregion_82 ~ 82),
                      subreg_91 = case_when(ID %in% subregion_91 ~ 91),
                      subreg_92 = case_when(ID %in% subregion_92 ~ 92)
                                            )

# find plz without subregion id
plz_DE$na_count <- apply(plz_DE, 1, function(x) sum(is.na(x)))

# only plzs without subregion id (27 = number of subregions)
plz_DE_na <- plz_DE %>% filter(na_count == 27)

# nur 2 Werte mit ausschließlich NA haben keine weitere Row mit ID:
# 57540 -> liegt in Frankreich
# 78266 (row 2674) -> liegt bei Gailingen am Hochrhein (subregion ID 112)
# diesen Wert Hinzufügen:
plz_DE[2674, "subreg_112"] <- 112

# find plz without subregion id again
plz_DE$na_count <- apply(plz_DE, 1, function(x) sum(is.na(x)))

# remove all rows without subregion id
plz_DE <- plz_DE %>% filter(na_count < 27)

# wide to long
plz_DE <- plz_DE %>% 
  pivot_longer(cols = starts_with("subreg"), 
               names_to = "sub", 
               values_to = "subregion_ID")

# drop na and select columns
plz_DE <- 
  plz_DE %>% 
    drop_na() %>%
    select(plz, note, subregion_ID)

# remove identical rows
plz_DE <- distinct(plz_DE)

# save as csv
write.csv(plz_DE, "plz_DE.csv")


######