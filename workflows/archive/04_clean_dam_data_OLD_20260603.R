#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_clean_dam_data.R
#
# Process barrier data from HCMR,
#  keep only true dams, classify into existing vs. planned,
# filter to the target basin, and prepare for snapping.
#
# Workflow:
#   1. Load and clean dam inventory (keep type == "DAM" only)
#   2. Classify existing vs. planned
#   3. Filter to target basin via api_get_ids()
#   4. Visualise
#   5. Data quality checks
#   6. Duplicate analysis
#
# Input:
#   - points_original/dams/dams_sarantaporos_table.csv   (semicolon-delimited;
#       columns: site_id;Name;Name_GR;type;longitude;latitude
#       type takes values FACTORY, EXCLUDE, DAM — only DAM is retained)
#   - config/study_area_params.csv              (BASIN_ID from 01_clean_hcmr_fish.R)
#
# Output:
#   - points_cleaned/dams/dams_all_clean.csv    (basin-filtered DAMs, existing+planned)
#   - points_cleaned/dams/dams_duplicates.csv
#   - points_cleaned/maps/dams_all_sources_overview.html
#   - points_cleaned/maps/dams_duplicates_map.html
#
# LOCATION: workflows/01_data_preparation/04_clean_dam_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(sf)
library(leaflet)
library(htmlwidgets)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("points_original/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/dams",  recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps",  recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Path to the curated dam inventory (semicolon-delimited)
DAM_FILE <- "points_original/dams/dams_sarantaporos_table.csv"

# site_id of the single existing operational dam.
# All other DAMs are treated as planned.
EXISTING_DAM_ID <- "37793"

# ============================================================
# STEP 1: Load and clean dam inventory
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING")
message(paste(rep("=", 80), collapse = ""))

message("\n=== Step 1: Loading dam inventory ===")

if (!file.exists(DAM_FILE)) stop("Dam inventory not found: ", DAM_FILE)

dams_raw <- fread(DAM_FILE, sep = ";")

message("  Rows loaded: ", nrow(dams_raw))
message("  type values: ",
        paste(names(table(dams_raw$type)), table(dams_raw$type),
              sep = "=", collapse = ", "))

# Keep only true dams; drop FACTORY and EXCLUDE
dams_all <- dams_raw %>%
  mutate(site_id = as.character(site_id)) %>%
  filter(type == "DAM") %>%
  select(site_id, Name, longitude, latitude) %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  )

message("  DAM records retained: ", nrow(dams_all))

# ============================================================
# STEP 2: Classify existing vs. planned
# ============================================================

message("\n=== Step 2: Classifying existing vs. planned ===")

dams_all <- dams_all %>%
  mutate(status = if_else(site_id == EXISTING_DAM_ID, "existing", "planned"))

if (!any(dams_all$status == "existing")) {
  warning("Existing dam id ", EXISTING_DAM_ID,
          " not found among DAM records — check EXISTING_DAM_ID.")
}

message("  Existing dams: ", sum(dams_all$status == "existing"))
message("  Planned dams:  ", sum(dams_all$status == "planned"))

# ============================================================
# STEP 3: Filter to target basin
# ============================================================

message("\n=== Step 3: Filtering to target basin ===")

# Load basin ID derived in 01_clean_hcmr_fish.R
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Target basin ID: ", BASIN_ID)

dams_unique_locs <- dams_all %>%
  distinct(site_id, longitude, latitude) %>%
  filter(!is.na(longitude), !is.na(latitude))

message("  Unique dam locations to query: ", nrow(dams_unique_locs))

basin_ids_dams <- api_get_ids(
  points          = dams_unique_locs,
  colname_lon     = "longitude",
  colname_lat     = "latitude",
  colname_site_id = "site_id",
  mode            = "local"
)

dams_all <- dams_all %>%
  left_join(basin_ids_dams) %>%
  filter(basin_id == BASIN_ID) %>%
  select(-basin_id, -subc_id, -reg_id)

message("  Dams in target basin: ", nrow(dams_all))
message("    existing: ", sum(dams_all$status == "existing"))
message("    planned:  ", sum(dams_all$status == "planned"))

fwrite(dams_all, "points_cleaned/dams/dams_all_clean.csv")
message("  Saved: points_cleaned/dams/dams_all_clean.csv")

# ============================================================
# STEP 4: Visualise
# ============================================================

message("\n=== Step 4: Creating visualisation ===")

status_colors <- colorFactor(
  palette = c("existing" = "darkblue", "planned" = "orange"),
  domain  = c("existing", "planned")
)

dams_map <- leaflet(dams_all) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    color       = ~status_colors(status),
    fillColor   = ~status_colors(status),
    radius      = 4,
    fillOpacity = 0.7,
    stroke      = TRUE,
    weight      = 1,
    popup       = ~paste0(
      "<b>Dam ID:</b> ", site_id, "<br>",
      "<b>Name:</b> ",   Name, "<br>",
      "<b>Status:</b> ", status
    )
  ) %>%
  addLayersControl(
    baseGroups = c("CartoDB", "Satellite"),
    options    = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = status_colors,
    values   = ~status,
    title    = "Dam status",
    opacity  = 0.7
  )

saveWidget(dams_map,
           "points_cleaned/maps/dams_all_sources_overview.html",
           selfcontained = TRUE)
save_to_nimbus(dams_map, "points_cleaned/maps/dams_all_sources_overview.html")
message("  Saved: points_cleaned/maps/dams_all_sources_overview.html")

# ============================================================
# STEP 5: Data quality checks
# ============================================================

message("\n=== Step 5: Data quality checks ===")

missing_coords <- dams_all %>% filter(is.na(longitude) | is.na(latitude))
if (nrow(missing_coords) > 0) {
  message("  WARNING: ", nrow(missing_coords), " dams with missing coordinates")
} else {
  message("  No missing coordinates")
}

out_of_bounds <- dams_all %>%
  filter(longitude < 19 | longitude > 28 | latitude < 34 | latitude > 42)
if (nrow(out_of_bounds) > 0) {
  message("  WARNING: ", nrow(out_of_bounds),
          " dams outside Greece bounds — check manually")
} else {
  message("  All coordinates within Greece bounds")
}

# ============================================================
# STEP 6: Duplicate analysis
# ============================================================

message("\n=== Step 6: Duplicate analysis ===")

duplicates_detailed <- dams_all %>%
  group_by(longitude, latitude) %>%
  mutate(n_at_location = n(), is_duplicate = n() > 1) %>%
  ungroup() %>%
  filter(is_duplicate) %>%
  arrange(longitude, latitude)

if (nrow(duplicates_detailed) > 0) {

  message("  Records at duplicate locations: ", nrow(duplicates_detailed))
  message("  Unique duplicate locations: ",
          n_distinct(paste(duplicates_detailed$longitude,
                           duplicates_detailed$latitude)))

  fwrite(duplicates_detailed, "points_cleaned/dams/dams_duplicates.csv")
  message("  Saved: points_cleaned/dams/dams_duplicates.csv")

  duplicates_map <- leaflet(duplicates_detailed) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      lng         = ~longitude,
      lat         = ~latitude,
      color       = "red",
      fillColor   = "red",
      radius      = 6,
      fillOpacity = 0.8,
      stroke      = TRUE,
      weight      = 2,
      popup       = ~paste0(
        "<b style='color:red;'>DUPLICATE LOCATION</b><br>",
        "<b>Records here:</b> ", n_at_location, "<br>",
        "<b>Dam ID:</b> ", site_id, "<br>",
        "<b>Status:</b> ", status
      )
    )

  saveWidget(duplicates_map,
             "points_cleaned/maps/dams_duplicates_map.html",
             selfcontained = TRUE)
  save_to_nimbus(duplicates_map, "points_cleaned/maps/dams_duplicates_map.html")
  message("  Saved: points_cleaned/maps/dams_duplicates_map.html")

} else {
  message("  No duplicate locations found")
}

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nTarget basin ID: ", BASIN_ID)
message("Dams in basin: ", nrow(dams_all),
        "  (existing: ", sum(dams_all$status == "existing"),
        ", planned: ",   sum(dams_all$status == "planned"), ")")
message("\nNext: snapping script")
