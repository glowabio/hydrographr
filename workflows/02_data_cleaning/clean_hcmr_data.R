
# script to clean fish data from HCMR

library(data.table)
library(readxl)
library(dplyr)
library(tidyr)

# Set nimbus dir to write out files
nimbus_path <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
setwd(nimbus_path)

# Clean fish data
sp_raw <- read_xlsx("points_original/Fish distributional & traits data (1).xlsx") %>%
    rename(longitude = Longtitude,
           latitude  = Latitude)

# Check for inverted lat/lon
sp <- sp_raw %>%
  mutate(
    inverted_lat_lon = latitude >= 19 & latitude <= 29 &
                        longitude >= 34 & longitude <= 42
  )

# Fix inverted lat/lon
sp <- sp %>%
  mutate(
    latitude_fixed = ifelse(inverted_lat_lon, longitude, latitude),
    longitude_fixed = ifelse(inverted_lat_lon, latitude, longitude)
  ) %>%
  select(-latitude, -longitude, -inverted_lat_lon) %>%
  rename(
    latitude = latitude_fixed,
    longitude = longitude_fixed
  )

# keep the dry or fishless locations separated
dry_fishless <- sp  %>% filter(!is.na(DRY) | !is.na(FISHLESS))

# keep locations with fish presence
sp <- sp %>% filter(is.na(DRY) | is.na(FISHLESS)) %>%
    select(-DRY, -FISHLESS)

# flip to longer format
sp <- sp %>% pivot_longer(
    cols = -c(Sites, latitude, longitude),  # keep site info
    names_to = "species",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  select(-value)

# check species names
sp %>% select(species) %>% arrange(species) %>%  distinct()  %>% pull()

# plot coordinates to check
library(leaflet)
leaflet(sp) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, popup = ~Sites)

sp <- sp %>% filter(latitude>10)


# Write clean version
fwrite(sp, file.path(nimbus_path, "points_cleaned/fish/fish_greece_hcmr.csv"))

sp <- fread(file.path(nimbus_path, "points_cleaned/fish/fish_greece_hcmr.csv"))
# traits sheet
# traits_raw <- read_xlsx("points_original/Fish distributional & traits data (1).xlsx", sheet = "Traits")

sp_to_snap <- sp %>% distinct(Sites,longitude,latitude)

# write points to snap
fwrite(sp_to_snap, file.path(nimbus_path, "points_cleaned/fish/fish_points_to_snap_hcmr.csv"))
