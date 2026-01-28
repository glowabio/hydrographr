
# script to clean fish data from HCMR

library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)

# Set nimbus dir to write out files
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"

# delete
wdir <- "~/Documents/Postdoc/projects/workflow_paper/data"

# Set local working directory
setwd(wdir)



# Clean fish data
sp_raw <- read_xlsx("points_original/fish/Fish distributional & traits data (1).xlsx") %>%
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
leaflet(sp) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~as.numeric(longitude),
                   lat = ~as.numeric(latitude), popup = ~Sites)

sp <- sp %>% filter(latitude>10)


# Write clean version
fwrite(sp, file.path(wdir, "points_cleaned/fish/fish_greece_hcmr.csv"))

sp <- fread(file.path(wdir, "points_cleaned/fish/fish_greece_hcmr.csv"))
# traits sheet
# traits_raw <- read_xlsx("points_original/Fish distributional & traits data (1).xlsx", sheet = "Traits")


# Get unique coordinates to snap
sp_to_snap <- sp %>% distinct(Sites,longitude,latitude)

# write points to snap
fwrite(sp_to_snap, file.path(wdir, "points_cleaned/fish/fish_points_to_snap_hcmr.csv"))
