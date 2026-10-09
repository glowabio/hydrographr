#############################
### Basic functionalities ###
#############################

## This script runs a few basic functionalities
## to test whether the hydrographr installation
## was successful.
## Merret Buurman, 2026-10-09

library(hydrographr)

### working directory
library(here)
data_dir <- file.path(here(), "data")
wdir <- file.path(here(), "try_out_basics")
if(!dir.exists(data_dir)) dir.create(data_dir)
if(!dir.exists(wdir)) dir.create(wdir)
setwd(wdir)


### test: get_tile_id()
### (this downloads 118 MB)
cat("\nTest: get_tile_id()")
df <- rbind(
  c(longitude = 20.853771, latitude = 40.251642, id=4),
  c(longitude = 19.603788, latitude = 40.387366, id=5)
)
df <- as.data.frame(df)
tile_ids <- get_tile_id(df, lon="longitude", lat="latitude", tempdir="/tmp/hydrographr")
tile_ids
test_passed <- identical(tile_ids, c("h18v04", "h20v02", "h20v04"))
if (test_passed) cat("\nOK\n") else warning("not ok")


### test: get_regional_unit_id()
### (this would download 118 MB, but they were already downloaded above)
cat("\nTest: get_regional_unit_id()")
reg_id <- get_regional_unit_id(df, lon="longitude", lat="latitude", tempdir="/tmp/hydrographr")
reg_id
test_passed <- reg_id == 66
if (test_passed) cat("\nOK\n") else warning("not ok")


### test: download_tiles()
### (this downloads 10 MB)
cat("\nTest: download_tiles()")
tile_ids <- c("h18v04")
download_tiles("basin",
  file_format = "tif",
  tile_id = tile_ids,
  global = FALSE,
  download_dir=data_dir
)
test_passed <- file.exists(file.path(data_dir, "r.watershed/basin_tiles20d/basin_h18v04.tif"))
if (test_passed) cat("\nOK\n") else warning("not ok")


### test: extract_ids()
cat("\nTest: extract_ids()")
basin_ids <- extract_ids(
  df,
  lon="longitude",
  lat="latitude",
  basin_layer=file.path(data_dir, "r.watershed/basin_tiles20d/basin_h18v04.tif")
)
test_passed <- 1292502 %in% basin_ids$basin_id
if (test_passed) cat("\nOK\n") else warning("not ok")


### test: merge_tiles()
### (this downloads 6 MB)
cat("\nTest: merge_tiles()")
download_tiles("basin",
  file_format = "tif",
  tile_id = c("h20v04"),
  global = FALSE,
  download_dir=data_dir
)
tiles_folder <- file.path(data_dir, "r.watershed/basin_tiles20d")
tile_names <- c("basin_h18v04.tif", "basin_h20v04.tif")
cat("\nMerging... this make take some moments...")
merge_tiles(
  tile_dir = tiles_folder,
  tile_names = tile_names,
  out_dir = wdir,
  file_name = paste0("basin_merged.tif"),
  read = FALSE,
  bigtiff = TRUE
)
path_basins_raster <- file.path(wdir, "basin_merged.tif")
test_passed <- file.exists(path_basins_raster)
if (test_passed) cat("\nOK\n") else warning("not ok")

### test: download_test_data()
### (this downloads 38 MB)
download_test_data(download_dir = data_dir)
test_passed1 <- dir.exists(file.path(data_dir, "hydrography90m_test_data"))
test_passed2 <- (length(list.files(file.path(data_dir, "hydrography90m_test_data"))) > 3)
if (test_passed1 && test_passed2) cat("\nOK\n") else warning("not ok")

cat("\nScript finished.\n")
