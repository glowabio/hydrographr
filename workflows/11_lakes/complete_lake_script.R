### Lake analysis for the workflow paper ###

# load libraries
library(terra)
library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

my_directory <- "/user/projects/vijosa"

#####################
### DATA DOWNLOAD ###
#####################

# download the following Hydrography90m data needed for the lake functions

# for get_lake_intersection():
-------------------------------------------------------
# 1. basin ("basin") raster files
# 2. stream network ("segment") raster files
# 3. flow accumulation ("accumulation") raster files
-------------------------------------------------------

# for get_lake_catchment():
-------------------------------------------------------
# 4. flow direction ("direction") raster files
-------------------------------------------------------

# for land cover analysis:
-------------------------------------------------------
# 5. sub-catchment ("sub_catchment") files
-------------------------------------------------------

# set to relevant tile ID or check with get_tile_id()
tile_id <- c("h20v04")

# select the necessary hydrological variables for downloading
vars_tif <- c("basin", "sub_catchment", "segment", "accumulation", "direction")
# Extend timeout to 1000s to allow uninterrupted downloading
options(timeout = 1000)

# Download the tif.files of the selected variables
download_tiles(variable = vars_tif,
               tile_id = tile_id,
               file_format = "tif",
               download_dir = my_directory)

########################
### EXTRACT LAKE IDS ###
########################

# extract the lakes based on the bounding box of the species occurrence points

# load stream network data
gpkg_data <- st_read(paste0(my_directory, "/data/spatial/vjosa_partial.gpkg"))

# load species data used in the extract_lake_ids() function
species <- fread(paste0(my_directory, "/data/species",
                        "/all_snapped_fish_points_from_sp_list.csv"))

# path to SWOT prior lake database downloaded via https://hydroweb.next.theia-land.fr/
swot_lakes <- paste0(my_directory, "/data/spatial/swot_lakes.gpkg")

# check lake dataset
lakes <- st_read(swot_lakes)
head(lakes)

# identify all lake IDs within the bounding box of the species occurrences data
extract_lake_ids(data = species,
                 lon = "longitude_snapped",
                 lat = "latitude_snapped",
                 bbox = TRUE,
                 var_name = "lake_id",
                 lake_shape = swot_lakes,
                 lake_id_table = my_directory,
                 quiet = TRUE)

##############################################################
### GET LAKE INTERSECTIONS BETWEEN LAKE AND STREAM NETWORK ### 
##############################################################

# stream network raster data
stream <- (paste0(my_directory,
                  "/data/spatial/",
                  "segment.tif"))

# flow accumulation raster data
flow <- (paste0(my_directory,
                "/data/spatial/",
                "accumulation.tif"))

# basin raster data
basins <- (paste0(my_directory,
                  "/data/spatial/",
                  "basin.tif"))

# the Aoos reservoir is within the extracted lake IDs with the ID 2130004632
data <- fread(paste0(my_directory,
                     "/data/lakes",
                     "/lake_id.txt"),
              header = TRUE)

# subset data to lake ID 2130004632
data <- data[data$lake_id == 2130004632, ]

# To run the function we need to have installed GuidosToolbox Workbench MSPA tool
# see also (https://forest.jrc.ec.europa.eu/en/activities/lpa/gtb/) and
# (https://glowabio.github.io/hydrographr/articles/case_study_lake_workflow.html)
# give full path virtual machine linux path
edge <- "/home/USER/GWB1.9.8/GWB"
gwb <- "/home/USER/GWB1.9.8/GWB"

lake_dat <- (paste0(my_directory,
                    "/data/lakes/lake_intersections/"))

lake_intersect_table <- get_lake_intersection(data,
                                              swot_lakes,
                                              lake_name = "swot_lakedatabase_20000101t000000_20991231t235959_20250331t170000_v202_light_eu__lake",
                                              lake_id = "lake_id",
                                              buffer = FALSE, edge, stream, flow,
                                              basins, lake_dat,
                                              n_cores = 1,
                                              quiet = TRUE)

################################
### DELINEATE LAKE CATCHMENT ###
################################

# load intersection table
coord_dat <- fread(paste0(my_directory, "/data/lakes/lake_intersections/coord_lake_2130004632.txt"))
head(coord_dat)

# create index to search for all stream IDs that exist in both the lake intersection point table and stream network
indx <- which(coord_dat$subc_id %in% gpkg_data$subc_id)

# subset the lake intersection table to only the stream IDs present in the stream network
coord_dat <- coord_dat[c(indx),]

# load flow direction raster data
direction <- (paste0(my_directory,
                  "/data/spatial/",
                  "direction_1292502.tif"))

# set path to lake catchment output
catch <- (paste0(my_directory,
                    "/data/lakes/lake_intersections/"))

# delineate lake catchment
get_lake_catchment(coord_dat, direction = direction,
                   flow = "flow_accu_mean",
                   n = 1, lake_basin = catch, quiet = TRUE)

##########################################
### EXTRACT ENV90M LANDCOVER VARIABLES ###
##########################################

# download Env90m landcover data for the years 1992 - 2020

download_landcover_tables(
   base_vars    = "ALL",
   years        = "ALL",
   tile_ids     = tile_id,
   download     = TRUE,
   download_dir = paste0("/env90m"),
   file_format  = "txt",
   delete_zips  = TRUE,
   quiet        = TRUE
)

## combine lake catchment raster with the lake surface raster
# load lake catchment raster
lake_catch <- terra::rast(paste0(my_directory, "/data/lakes/lake_intersections/basin_lake_2130004632_coord_1.tif"))

# load lake raster
lake_rast <- terra::rast(paste0(my_directory, "/data/lakes/lake_intersections/lake_2130004632.tif"))

# merge lake catchment and lake raster
lake_catch <- terra::merge(lake_catch, lake_rast)

# Calculate the lake area
# reproject to LAEA Europe (suitable for Greece)
lake_catch_laea <- terra::project(lake_catch, "EPSG:3035")

lake_catch_area <- terra::expanse(lake_catch_laea, unit = "km", zones = lake_catch_laea)

print(lake_catch_area)

# export merged lake catchment raster
terra::writeRaster(lake_catch,
                   paste0(my_directory, "/data/lakes/lake_intersections/basin_lake_2130004632_merged.tif"),
                   overwrite = TRUE)

## extract the subcatchment IDs of the lake catchment raster data
# load in sub-catchment raster file
subc_raster <- terra::rast(paste0(my_directory, "/data/spatial/sub_catchment_h20v04.tif"))

## align both the lake catchment raster and subcatchment raster data
# crop subcatchment raster to lake catchment raster bounding box (keeps original grid of subcatchment raster data)
lake_catch_crop <- terra::crop(subc_raster, lake_catch)

# replace NA with 0 in lake catchment raster so averaging gives true coverage fractions
lake_catch_binary <- terra::subst(lake_catch, NA, 0)

# resample binary lake catchment raster onto subcatchment grid: each cell gets fraction covered by lake catchment
lake_catch_cover <- terra::resample(lake_catch_binary, lake_catch_crop, method = "average")

# keep only subcatchment cells with >= 90% coverage, trim NA margins from result
lake_catch_crop <- terra::trim(terra::mask(lake_catch_crop, lake_catch_cover >= 0.9, maskvalues = FALSE))

# export cropped catchment raster file
writeRaster(lake_catch_crop, paste0(my_directory, "/data/spatial/subc_id_lake_catchment.tif"), overwrite = TRUE)

# extract the sub-catchment IDs for the lake catchment
lake_catch_ids <- extract_ids(subc_layer = paste0(my_directory, "/data/spatial/subc_id_lake_catchment.tif"))

# export sub-catchment IDs to disk
fwrite(lake_catch_ids, paste0(my_directory, "/subc_IDs.txt"))

# set year and tile ID for the get_predict_table() function
years <- 1992:2020
tile_id <- "h20v04"

# Create name vectors for all ESA CCI landcover classes
lc_classes <- c("c10", "c20", "c30", "c40", "c50", "c60", "c70", "c80", "c90",
                 "c100", "c110", "c120", "c130", "c140", "c150", "c160", "c170",
                 "c180", "c190", "c200", "c210", "c220")
var_names <- as.vector(outer(lc_classes, years, paste, sep = "_"))

# name of the land cover variable
var <- var_names

tb <- get_predict_table(variable = var,
                        statistics = c("mean"),
                        tile_id = tile_id,
                        input_var_path = paste0(my_directory, "/data/env90m/esa_cci_landcover_v2_1_1/"),
                        subcatch_id = file.path(my_directory, "/data/subc_IDs.txt"),
                        out_file_path = paste0(my_directory, "/predictTB.csv"),
                        read = FALSE,
                        overwrite = TRUE,
                        n_cores = 6)

# export the predict table that contains all landcover env90m data for the subcatchments of the whole Aoos lake catchment area
landcover <- fread(paste0(my_directory, "/data/env90m/predictTB.csv"))

# show result of get_predict_table()
head(landcover)

# load in the subcatchment raster file of the lake catchment area
subc_raster <- rast(paste0(my_directory, "/lake_intersection/subc_id_lake_catchment.tif"))
subc_areas <- terra::expanse(subc_raster, unit = "km", zones = subc_raster)
names(subc_areas) <- c("layer", "subc_id", "area_km2")

# load in the land cover proportion per subcatchment
names(landcover)[names(landcover) == "subcID"] <- "subc_id"

# merge areas with land cover table
landcover <- merge(landcover, subc_areas[, c("subc_id", "area_km2")], by = "subc_id")

# multiply each proportion by its subcatchment area to get km²
lc_cols <- grep("^c[0-9]+_", names(landcover))
landcover_area <- landcover[, ..lc_cols] * landcover$area_km2

# sum across all sub-catchments per year
col_sums <- colSums(landcover_area)

# convert to data frame for plotting
lake_land_cover <- enframe(col_sums, name = "id", value = "area_km2")
lake_land_cover <- lake_land_cover %>%
  separate(id, into = c("variable", "year"), sep = "_y") %>%
  mutate(year = as.numeric(year))

# set explicit factor level order c10 -> c220
lake_land_cover$variable <- factor(
  lake_land_cover$variable,
  levels = c("c10", "c20", "c30", "c40", "c50", "c60", "c70", "c80", "c90",
             "c100", "c110", "c120", "c130", "c140", "c150", "c160", "c170",
             "c180", "c190", "c200", "c210", "c220")
)

# Kelly's (1965) 22 colors of maximum contrast — designed for exactly 22 categories
kelly_colors <- setNames(
  c("#E68FAC", "#875692", "#F38400", "#A1CAF1", "#BE0032",
    "#008856", "#848482", "#C2B280", "#F3C300", "#0067A5",
    "#F99379", "#604E97", "#F6A600", "#B3446C", "#DCD300",
    "#882D17", "#8DB600", "#654522", "#E25822", "#2B3D26",
    "#222222", "#7B4173"),
  lc_classes
)

# Plot all landcover classes
ggplot(lake_land_cover, aes(x = year, y = area_km2, color = variable)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = kelly_colors) +
  scale_x_continuous(
    breaks = seq(min(lake_land_cover$year), max(lake_land_cover$year), by = 2)
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  labs(x = "Year", y = "Landcover area (km²)", color = "") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.text = element_text(size = 9)
  ) +
  guides(color = guide_legend(nrow = 2))

# filter lake_land_cover by a single year and sort by area highest to lowest
lake_land_cover_2020 <- lake_land_cover[lake_land_cover$year == 2020, ]
lake_land_cover_2020 <- lake_land_cover_2020[order(lake_land_cover_2020$area_km2, decreasing = TRUE), ]
print(lake_land_cover_2020, n = 22)