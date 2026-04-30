#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_clean_dam_data.R
#
# Process and combine dam data from RAAY and AMBER sources,
# filter to target basin, and prepare for snapping.
#
# Workflow:
#   1. Process RAAY small hydropower plants
#   2. Process AMBER dams
#   3. Combine datasets
#   3b. Filter to target basin via api_get_ids()
#   4. Visualise
#   5. Data quality checks
#   6. Duplicate analysis
#
# Input:
#   - points_original/dams/V_SDI_R_HYDRO*.csv  (RAAY)
#   - points_original/dams/atlas-country-Greece.csv  (AMBER)
#   - config/study_area_params.csv  (BASIN_ID from clean_hcmr_fish.R)
#
# Output:
#   - points_cleaned/dams/dams_all_clean.csv        (basin-filtered, all sources)
#   - points_cleaned/dams/dams_duplicates.csv
#   - points_cleaned/dams/dams_cross_source_duplicates.csv
#   - points_cleaned/maps/dams_all_sources_overview.html
#   - points_cleaned/maps/dams_duplicates_map.html
#
# LOCATION: workflows/01_data_preparation/04_clean_dam_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(sf)
library(leaflet)
library(htmlwidgets)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("points_original/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. PROCESS RAAY SMALL HYDROPOWER PLANTS
# ============================================================================

message("\n=== Step 1: Processing RAAY hydropower data ===")

csv_dir  <- "points_original/dams"
file_map <- c(
  "V_SDI_R_HYDRO12_Installation_Licence.csv" = "IL",
  "V_SDI_R_HYDRO13_Operational_Licence.csv"  = "OL",
  "V_SDI_R_HYDRO11_Production_Licence.csv"   = "PL",
  "V_SDI_R_HYDRO7_Evaluation.csv"            = "E"
)

read_and_process_raay <- function(file_name, phase_code) {
  file_path <- file.path(csv_dir, file_name)
  if (!file.exists(file_path)) {
    warning("File not found: ", file_path)
    return(NULL)
  }
  df <- fread(file_path)
  if ("katastash_code" %in% names(df)) df <- rename(df, status_code = katastash_code)
  if ("geometry" %in% names(df)) {
    df <- df %>%
      mutate(
        geometry = str_remove_all(geometry, "MULTIPOINT|\\(|\\)"),
        geometry = str_trim(geometry)
      ) %>%
      separate_rows(geometry, sep = ",") %>%
      group_by(across(-geometry)) %>%
      slice(1) %>%
      ungroup() %>%
      separate(geometry, into = c("longitude", "latitude"), sep = " ", convert = TRUE)
  }
  if ("part" %in% names(df)) df <- df %>% filter(part == "Y/L")
  df <- mutate(df,
               phase  = phase_code,
               type   = "shp",
               source = "RAAY",
               status = if_else(phase_code == "OL", "existing", "planned"))
  df %>%
    group_by(longitude, latitude) %>%
    arrange(desc(imerominia)) %>%
    slice(1) %>%
    ungroup()
}

raay_dams <- imap_dfr(names(file_map), ~ read_and_process_raay(.x, file_map[[.x]]))

raay_dams <- raay_dams %>%
  group_by(longitude, latitude) %>%
  arrange(desc(imerominia)) %>%
  slice(1) %>%
  ungroup()

if (nrow(raay_dams) == 0) stop("No RAAY data loaded. Check file paths.")

raay_dams <- raay_dams %>%
  select(id1, phase, status_code, longitude, latitude, type, source, status, imerominia) %>%
  rename(date = imerominia)

message("  RAAY hydropower plants: ", nrow(raay_dams))
message("  Phases: ", paste(unique(raay_dams$phase), collapse = ", "))

# ============================================================================
# 2. PROCESS AMBER DAMS
# ============================================================================

message("\n=== Step 2: Processing AMBER dams ===")

amber_dams <- read.csv("points_original/dams/atlas-country-Greece.csv") %>%
  select(GUID, Longitude_WGS84, Latitude_WGS84, type) %>%
  rename(longitude = Longitude_WGS84, latitude = Latitude_WGS84) %>%
  mutate(
    type        = "dam",
    phase       = "amber",
    status_code = "existing",
    source      = "AMBER",
    status      = "existing",
    GUID        = gsub("\\{|\\}", "", GUID)
  ) %>%
  mutate(id1 = sapply(strsplit(as.character(GUID), "-"), `[`, 5), .before = GUID) %>%
  select(id1, phase, status_code, longitude, latitude, type, source, status) %>%
  mutate(date = NA)

message("  AMBER dams: ", nrow(amber_dams))

# ============================================================================
# 3. COMBINE
# ============================================================================

message("\n=== Step 3: Combining datasets ===")

dams_all <- rbind(raay_dams, amber_dams)

message("  Total dams combined: ", nrow(dams_all))
message("  By source:")
print(table(dams_all$source))
message("  RAAY by phase:")
print(table(raay_dams$phase))

# ============================================================================
# 3b. FILTER TO TARGET BASIN
# ============================================================================

message("\n=== Step 3b: Filtering to target basin ===")

# Load basin ID derived in clean_hcmr_fish.R
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Target basin ID: ", BASIN_ID)

# Unique locations for API call
dams_unique_locs <- dams_all %>%
  distinct(id1, longitude, latitude) %>%
  filter(!is.na(longitude), !is.na(latitude))

# Check for duplicate id1 values across sources
dup_ids <- dams_unique_locs %>% group_by(id1) %>% filter(n() > 1)
if (nrow(dup_ids) > 0) {
  message("  WARNING: ", n_distinct(dup_ids$id1),
          " duplicate id1 values across sources — check cross-source IDs")
  print(dup_ids)
}

message("  Unique dam locations to query: ", nrow(dams_unique_locs))

basin_ids_dams <- api_get_ids(
  points          = dams_unique_locs,
  colname_lon     = "longitude",
  colname_lat     = "latitude",
  colname_site_id = "id1",
  mode            = "local"
)

dams_all <- dams_all %>%
  left_join(basin_ids_dams) %>%
  filter(basin_id == BASIN_ID)%>%
  select(-basin_id, -subc_id, -reg_id)

cat("  Dams missing basin_id:", sum(is.na(dams_all$basin_id)), "\n")
message("  Dams in target basin: ", nrow(dams_all))
message("  By source:")
print(table(dams_all$source))
message("  By phase:")
print(table(dams_all$phase))

# Save basin-filtered combined file
fwrite(dams_all, "points_cleaned/dams/dams_all_clean.csv")
message("  Saved: points_cleaned/dams/dams_all_clean.csv")

# ============================================================================
# 4. VISUALISE
# ============================================================================

message("\n=== Step 4: Creating visualisation ===")

source_colors <- colorFactor(
  palette = c("RAAY" = "blue", "AMBER" = "green"),
  domain  = c("RAAY", "AMBER")
)

phase_colors <- colorFactor(
  palette = c("IL" = "lightblue", "OL" = "darkblue",
              "PL" = "purple", "E" = "orange", "amber" = "green"),
  domain  = c("IL", "OL", "PL", "E", "amber")
)

dams_map <- leaflet(dams_all) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    color       = ~source_colors(source),
    fillColor   = ~source_colors(source),
    radius      = 4,
    fillOpacity = 0.7,
    stroke      = TRUE,
    weight      = 1,
    group       = "By Source",
    popup       = ~paste0(
      "<b>Dam ID:</b> ", id1, "<br>",
      "<b>Source:</b> ", source, "<br>",
      "<b>Phase:</b> ",  phase, "<br>",
      "<b>Status:</b> ", status_code, "<br>",
      "<b>Type:</b> ",   type
    )
  ) %>%

  addCircleMarkers(
    data        = dams_all %>% filter(source == "RAAY"),
    lng         = ~longitude,
    lat         = ~latitude,
    color       = ~phase_colors(phase),
    fillColor   = ~phase_colors(phase),
    radius      = 4,
    fillOpacity = 0.7,
    stroke      = TRUE,
    weight      = 1,
    group       = "RAAY by Phase",
    popup       = ~paste0(
      "<b>Dam ID:</b> ", id1, "<br>",
      "<b>Phase:</b> ",  phase, "<br>",
      "<b>Status:</b> ", status_code
    )
  ) %>%

  addLayersControl(
    baseGroups    = c("CartoDB", "Satellite"),
    overlayGroups = c("By Source", "RAAY by Phase"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    pal      = source_colors,
    values   = ~source,
    title    = "Dam source",
    opacity  = 0.7
  ) %>%

  addLegend(
    position = "bottomleft",
    pal      = phase_colors,
    values   = dams_all$phase,
    title    = "Phase",
    opacity  = 0.7
  ) %>%

  hideGroup("RAAY by Phase")

saveWidget(dams_map,
           "points_cleaned/maps/dams_all_sources_overview.html",
           selfcontained = TRUE)
save_to_nimbus(dams_map, "points_cleaned/maps/dams_all_sources_overview.html")
message("  Saved: points_cleaned/maps/dams_all_sources_overview.html")

# ============================================================================
# 5. DATA QUALITY CHECKS
# ============================================================================

message("\n=== Step 5: Data quality checks ===")

missing_coords <- dams_all %>% filter(is.na(longitude) | is.na(latitude))
if (nrow(missing_coords) > 0) {
  message("  WARNING: ", nrow(missing_coords), " dams with missing coordinates")
  print(table(missing_coords$source))
} else {
  message("  No missing coordinates")
}

out_of_bounds <- dams_all %>%
  filter(longitude < 19 | longitude > 28 | latitude < 34 | latitude > 42)
if (nrow(out_of_bounds) > 0) {
  message("  WARNING: ", nrow(out_of_bounds), " dams outside Greece bounds — check manually")
} else {
  message("  All coordinates within Greece bounds")
}

# ============================================================================
# 6. DUPLICATE ANALYSIS
# ============================================================================

message("\n=== Step 6: Duplicate analysis ===")

duplicates_detailed <- dams_all %>%
  group_by(longitude, latitude) %>%
  mutate(n_at_location = n(), is_duplicate = n() > 1) %>%
  ungroup() %>%
  filter(is_duplicate) %>%
  arrange(longitude, latitude, source)

if (nrow(duplicates_detailed) > 0) {

  message("  Records at duplicate locations: ", nrow(duplicates_detailed))
  message("  Unique duplicate locations: ",
          n_distinct(paste(duplicates_detailed$longitude,
                           duplicates_detailed$latitude)))

  duplicate_combos <- duplicates_detailed %>%
    group_by(longitude, latitude) %>%
    summarise(
      sources      = paste(sort(unique(source)), collapse = " + "),
      n_sources    = n_distinct(source),
      total_records = n(),
      .groups = "drop"
    )

  message("  Locations in multiple databases: ", sum(duplicate_combos$n_sources > 1))
  message("  Locations in same database only: ", sum(duplicate_combos$n_sources == 1))

  fwrite(duplicates_detailed, "points_cleaned/dams/dams_duplicates.csv")
  message("  Saved: points_cleaned/dams/dams_duplicates.csv")

  cross_source_dups <- duplicates_detailed %>%
    group_by(longitude, latitude) %>%
    filter(n_distinct(source) > 1) %>%
    arrange(longitude, latitude, source) %>%
    select(longitude, latitude, source, id1, phase, status_code, type) %>%
    ungroup()

  if (nrow(cross_source_dups) > 0) {
    message("  Cross-source duplicates: ", nrow(cross_source_dups))
    fwrite(cross_source_dups,
           "points_cleaned/dams/dams_cross_source_duplicates.csv")
    message("  Saved: points_cleaned/dams/dams_cross_source_duplicates.csv")
  }

  # Visualise duplicates
  dup_source_colors <- colorFactor(
    palette = c("RAAY" = "red", "AMBER" = "orange"),
    domain  = c("RAAY", "AMBER")
  )

  duplicates_map <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      data        = duplicates_detailed,
      lng         = ~longitude,
      lat         = ~latitude,
      color       = ~dup_source_colors(source),
      fillColor   = ~dup_source_colors(source),
      radius      = 6,
      fillOpacity = 0.8,
      stroke      = TRUE,
      weight      = 2,
      popup       = ~paste0(
        "<b style='color:red;'>DUPLICATE LOCATION</b><br>",
        "<b>Records here:</b> ", n_at_location, "<br>",
        "<b>Dam ID:</b> ", id1, "<br>",
        "<b>Source:</b> ", source, "<br>",
        "<b>Phase:</b> ", phase
      ),
      label = ~paste0("Duplicate: ", n_at_location, " records (", source, ")")
    ) %>%
    addLegend(
      position = "bottomright",
      pal      = dup_source_colors,
      values   = duplicates_detailed$source,
      title    = "Duplicate source",
      opacity  = 0.8
    )

  saveWidget(duplicates_map,
             "points_cleaned/maps/dams_duplicates_map.html",
             selfcontained = TRUE)
  save_to_nimbus(duplicates_map, "points_cleaned/maps/dams_duplicates_map.html")
  message("  Saved: points_cleaned/maps/dams_duplicates_map.html")

} else {
  message("  No duplicate locations found")
}

# ============================================================================
# SUMMARY
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nTarget basin ID: ", BASIN_ID)
message("\nDams in basin: ", nrow(dams_all))
message("  RAAY: ", sum(dams_all$source == "RAAY"))
message("  AMBER: ", sum(dams_all$source == "AMBER"))
message("\nFiles created:")
message("  points_cleaned/dams/dams_all_clean.csv")
if (exists("duplicates_detailed") && nrow(duplicates_detailed) > 0) {
  message("  points_cleaned/dams/dams_duplicates.csv")
  if (nrow(cross_source_dups) > 0)
    message("  points_cleaned/dams/dams_cross_source_duplicates.csv")
}
message("  points_cleaned/maps/dams_all_sources_overview.html")
message("\nNext: snapping script")
