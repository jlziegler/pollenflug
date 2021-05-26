# pollen_warning
# maps pollen warnings to postal codes

# import JSON
pollen_data <-
  jsonlite::fromJSON("https://opendata.dwd.de/climate_environment/health/alerts/s31fg.json")

# create data table with pollen data
pollen_data <- jsonlite::flatten(pollen_data$content)

# transform to longer data table
pollen_data <-
  tidyr::pivot_longer(
    pollen_data,
    cols = starts_with("Pollen"),
    names_to = c("pollentype", "day"),
    names_pattern = "Pollen\\.(.*)\\.(.*)",  # extracted strings in brackets, \\ to escape dots
    values_to = "warninglevel"
  )

# filter level
filterlevel <- c("2", "2-3", "3")

# filter only tomorrow's data & filterlevel
pollen_filtered <- dplyr::filter(pollen_data, day == "tomorrow" & warninglevel %in% filterlevel)

# read postal code data
# source: plz-gebiete.shp.zip, downloaded from https://www.suche-postleitzahl.org/downloads
plz_DE <- read.csv("plz_DE.csv")[,-1]

# inner join 
pollen_warning <- dplyr::left_join(pollen_filtered, 
                                   plz_DE, 
                                   by = c("partregion_id" = "subregion_ID"))


############################################
# ab hier musst du wahrscheinlich anpassen, es sei denn ich bekomme den zugriff irgendwie

user <- "atu"
password <- "GhR5jvsz89A"
v_date <- Sys.Date()-1 # has to be minus one because file from day before actual day is available only
v_data_plain <- gsub("-", "", v_date)
v_filename <- paste0(v_data_plain,"_000000_IVR_0.csv")
# Example file
# fileUrl <- "https://audiotex-transfer.arcor.de/kunde/atu/ATU_Filialrouting_LSN/cdr/2021-03-21/00_00/20210321_000000_IVR_0.csv"

# build path generically
fileUrl <- paste0("https://audiotex-transfer.arcor.de/kunde/atu/ATU_Filialrouting_LSN/cdr/", v_date, "/00_00/", v_filename)

# store file locally
filename <- here::here("data",v_filename)

# curl file
GET(fileUrl, authenticate(user, password), 
    write_disk(filename), timeout(60))

# read data
tbl_ivr <- read.csv(filename)

# And save to S3
system(paste("aws s3 cp", filename, "s3://mobiviadatabase/emr/shared/ATU/phone-system/Export/vodafone/"))

# remove temporary csv file from data folder
file.remove(filename)

