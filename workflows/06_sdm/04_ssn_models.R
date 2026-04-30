# install.packages("SSNbler") # if not yet installed
library(SSNbler)
library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(leaflet)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("spatial/subbasin/ssn_folder")

subbasin_streams_pruned <- st_read("spatial/subbasin/stream_network_pruned_min3.gpkg")

subbasin_streams_pruned_reproj <- st_transform(subbasin_streams_pruned, crs = 27704)

st_write(subbasin_streams_pruned_reproj, "spatial/subbasin/subbasin_streams_pruned_reproj.gpkg")

fish_subbasin_sf <- fish_subbasin %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_subbasin_sf <- dams_subbasin %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)



# Your stream network (sf linestring object) and observation points (sf point object)
# SSNbler needs: edges with correct topology, sites snapped to network
ssn_assembled2 <- lines_to_lsn(
  streams = subbasin_streams_pruned_reproj,
  lsn_path = "spatial/subbasin/ssn_folder",
  check_topology = FALSE,
  overwrite = TRUE

)


obs <- sites_to_lsn(
  sites = fish_subbasin_sf,
  edges = edges,
  lsn_path = lsn.path,
  file_name = "obs",
  snap_tolerance = 100,
  save_local = TRUE,
  overwrite = TRUE
)

preds <- sites_to_lsn(
  sites = MF_pred1km,
  edges = edges,
  save_local = TRUE,
  lsn_path = lsn.path,
  file_name = "pred1km.gpkg",
  snap_tolerance = 100,
  overwrite = TRUE
)
