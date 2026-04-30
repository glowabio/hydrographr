# GBIF fish downloader for Vjosa basin
library(rgbif)
library(data.table)
library(dplyr)
library(stringr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")

# ── Config ───────────────────────────────────────────────────────────────────

setwd(BASE_DIR)
out_dir      <- file.path(BASE_DIR, "points_original/fish")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

GBIF_USER  <- Sys.getenv("GBIF_USER")
GBIF_PWD   <- Sys.getenv("GBIF_PWD")
GBIF_EMAIL <- Sys.getenv("GBIF_EMAIL")

BASIN_BBOX <- "POLYGON((19.311 39.783, 19.311 40.686, 21.23 40.686, 21.23 39.783, 19.311 39.783))"
MODE       <- "species"   # "order" | "species"

med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

species_list <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>%
  unique() %>%
  stringr::str_replace("_", " ")

taxa <- if (MODE == "order") med_fish_orders else species_list
rank <- if (MODE == "order") "ORDER" else "SPECIES"

# ── Download ─────────────────────────────────────────────────────────────────

submitted <- list()

for (taxon in taxa) {
  cat("\n[", taxon, "]\n")

  # Resolve taxon key
  bb <- tryCatch(name_backbone(name = taxon, rank = rank), error = \(e) NULL)
  if (is.null(bb) || !"usageKey" %in% names(bb)) {
    cat("  no usageKey — skipping\n"); next
  }

  # Check occurrence count
  n <- tryCatch(
    occ_search(taxonKey = bb$usageKey, geometry = BASIN_BBOX, limit = 1)$meta$count,
    error = \(e) 0L
  )
  if (!length(n) || n == 0) { cat("  0 occurrences — skipping\n"); next }
  cat("  ", n, "occurrences found\n")

  # Submit (up to 5 retries)
  for (attempt in seq_len(5)) {
    key <- tryCatch(
      occ_download(
        pred("hasGeospatialIssue", FALSE),
        pred("hasCoordinate", TRUE),
        pred("occurrenceStatus", "PRESENT"),
        pred_not(pred_isnull("eventDate")),
        pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))),
        pred_in("taxonKey", bb$usageKey),
        pred_within(BASIN_BBOX),
        format = "SIMPLE_CSV",
        user = GBIF_USER, pwd = GBIF_PWD, email = GBIF_EMAIL
      ),
      error = \(e) { message("  submit failed (attempt ", attempt, "): ", e$message); NULL }
    )
    if (!is.null(key)) { submitted[[taxon]] <- key; cat("  submitted:", key, "\n"); break }
    Sys.sleep(120)
  }

  # Throttle: pause every 5 submissions
  if (length(submitted) %% 5 == 0) { cat("  [batch pause 60s]\n"); Sys.sleep(60) }
}

# ── Wait, fetch, and combine ─────────────────────────────────────────────────

rows <- lapply(names(submitted), function(taxon) {
  key      <- submitted[[taxon]]
  zip_path <- file.path(out_dir, paste0(key, ".zip"))

  if (!file.exists(zip_path)) {
    cat("\nwaiting for", taxon, "(", key, ")...\n")
    occ_download_wait(key)
    occ_download_get(key, path = out_dir)
  }

  csv_name <- unzip(zip_path, list = TRUE)$Name
  csv_name <- csv_name[grepl("\\.csv$", csv_name)][1]
  if (is.na(csv_name)) { cat("  no CSV in ZIP for", taxon, "\n"); return(NULL) }

  taxon_dir <- file.path(out_dir, gsub("[^A-Za-z0-9_-]", "_", taxon))
  dir.create(taxon_dir, showWarnings = FALSE, recursive = TRUE)

  dt <- tryCatch(
    fread(unzip(zip_path, files = csv_name, exdir = taxon_dir), fill = TRUE, showProgress = FALSE),
    error = \(e) { message("  read error for ", taxon, ": ", e$message); NULL }
  )
  if (is.null(dt)) return(NULL)

  cat("  ", nrow(dt), "rows —", taxon, "\n")
  dt[, taxon_downloaded := taxon][]
})

combined <- rbindlist(rows, fill = TRUE)
out_csv  <- file.path(out_dir, "combined_greece_fish_occurrences_AG_april.csv")
fwrite(combined, out_csv)

# GBIF citation
citaton <- occ_download_meta(key)$doi
# ── Summary ──────────────────────────────────────────────────────────────────

message(
  "\n", strrep("=", 60), "\nGBIF DOWNLOAD COMPLETE\n", strrep("=", 60),
  "\n  Mode:            ", MODE,
  "\n  Taxa downloaded: ", uniqueN(combined$taxon_downloaded),
  "\n  Total rows:      ", nrow(combined),
  "\n  Saved to:        ", out_csv,
  "\n\nNext: 02_clean_gbif_data.R"
)
# GBIF fish downloader for Greece — supports download by ORDER or SPECIES LIST
# Requirements: install.packages(c("rgbif", "sf", "data.table", "dplyr", "readxl", "stringr"))

library(rgbif)
library(sf)
library(data.table)
library(dplyr)
library(readxl)
library(stringr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")

# ── Configuration ────────────────────────────────────────────────────────────

# Set working directory
# Check working directory
BASE_DIR
setwd(BASE_DIR)

download_path <- file.path(BASE_DIR, "points_original/fish")
dir.create(download_path, recursive = TRUE, showWarnings = FALSE)

GBIF_USER  <- Sys.getenv("GBIF_USER")
GBIF_PWD   <- Sys.getenv("GBIF_PWD")
GBIF_EMAIL <- Sys.getenv("GBIF_EMAIL")

vjosa_poly <- "POLYGON((19.311 39.783, 19.311 40.686, 21.23 40.686, 21.23 39.783, 19.311 39.783))"

# ── Taxa to download ─────────────────────────────────────────────────────────

med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

species_list <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>%
  unique() %>%
  stringr::str_replace("_", " ")

# ── MODE — switch here ───────────────────────────────────────────────────────
# "order"   : one GBIF download request per taxonomic order
# "species" : one GBIF download request per species
MODE <- "species"   # <-- change to "species" to use the species list

# ── Helper: safe name_backbone ───────────────────────────────────────────────

safe_name_backbone <- function(name, rank) {
  tryCatch(
    rgbif::name_backbone(name = name, rank = rank),
    error = function(e) {
      message("  -> name_backbone error for ", name, ": ", conditionMessage(e))
      NULL
    }
  )
}

# ── Step 1: Submit download requests ────────────────────────────────────────
# Works for both modes: pass orders + rank="ORDER", or species + rank="SPECIES"

submit_gbif_downloads <- function(taxa, polygon, rank,
                                  batch_size = 5, wait_time = 60) {
  results           <- list()
  submitted_in_batch <- 0

  for (taxon in taxa) {
    cat("\nProcessing", rank, ":", taxon, "...\n")

    bb  <- safe_name_backbone(taxon, rank = rank)
    key <- if (!is.null(bb) && "usageKey" %in% names(bb)) bb$usageKey else NULL
    if (is.null(key)) {
      cat("  -> ERROR: No usageKey for", taxon, "- skipping.\n")
      next
    }

    meta <- tryCatch(
      occ_search(taxonKey = key, geometry = polygon, limit = 1)$meta,
      error = function(e) {
        message("  -> occ_search error for ", taxon, ": ", conditionMessage(e))
        NULL
      }
    )
    n_occ <- if (!is.null(meta) && "count" %in% names(meta)) meta$count else 0
    if (n_occ == 0) {
      cat("  -> No occurrences for", taxon, "- skipping.\n")
      next
    }
    cat("  -> Found", n_occ, "occurrences. Submitting download...\n")

    success  <- FALSE
    attempts <- 0
    while (!success && attempts < 5) {
      attempts <- attempts + 1
      d <- tryCatch(
        occ_download(
          pred("hasGeospatialIssue", FALSE),
          pred("hasCoordinate", TRUE),
          pred("occurrenceStatus", "PRESENT"),
          pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))),
          pred_in("taxonKey", key),
          pred_within(polygon),
          format = "SIMPLE_CSV",
          user = GBIF_USER, pwd = GBIF_PWD, email = GBIF_EMAIL
        ),
        error = function(e) {
          message("  -> Submit failed (attempt ", attempts, ") for ", taxon,
                  ": ", conditionMessage(e))
          NULL
        }
      )

      if (!is.null(d)) {
        success            <- TRUE
        results[[taxon]]   <- d
        submitted_in_batch <- submitted_in_batch + 1
        cat("  -> Submitted. Key:", d, "\n")
      } else {
        cat("  -> Retrying in 120 s...\n")
        Sys.sleep(120)
      }
    }
    if (!success) cat("  -> Failed after retries; skipping", taxon, "\n")

    if (submitted_in_batch >= batch_size) {
      cat("\nBatch of", batch_size, "submitted — waiting", wait_time, "s...\n")
      Sys.sleep(wait_time)
      submitted_in_batch <- 0
    }
  }
  results
}

# ── Step 2: Wait for GBIF to prepare ZIPs and download them ─────────────────

download_zips <- function(submitted_keys, out_dir = download_path) {
  for (taxon in names(submitted_keys)) {
    key      <- submitted_keys[[taxon]]
    zip_path <- file.path(out_dir, paste0(key, ".zip"))

    if (file.exists(zip_path)) {
      cat("ZIP already present for", taxon, "- skipping.\n")
      next
    }

    cat("Waiting for GBIF to finish:", taxon, "(key:", key, ")\n")
    occ_download_wait(key)
    occ_download_get(key, path = out_dir)
    cat("  -> ZIP downloaded for", taxon, "\n")
  }
}

# ── Step 3: Extract CSVs and combine ────────────────────────────────────────

extract_and_combine <- function(submitted_keys, zip_dir = download_path) {
  combined_list <- list()

  for (taxon in names(submitted_keys)) {
    key      <- submitted_keys[[taxon]]
    zip_file <- file.path(zip_dir, paste0(key, ".zip"))

    cat("\n=====================================\n")
    if (!file.exists(zip_file)) {
      cat("ZIP not found for:", taxon, "| expected:", zip_file, "\n")
      next
    }
    cat("Processing:", taxon, "| ZIP:", zip_file, "\n")

    zip_list  <- unzip(zip_file, list = TRUE)$Name
    csv_files <- zip_list[grepl("\\.csv$", zip_list, ignore.case = TRUE)]
    if (length(csv_files) == 0) {
      cat("  -> No CSV found in ZIP\n")
      next
    }

    taxon_dir <- file.path(zip_dir, gsub("[^A-Za-z0-9_\\-]", "_", taxon))
    dir.create(taxon_dir, showWarnings = FALSE, recursive = TRUE)
    extracted_csv <- unzip(zip_file, files = csv_files[1], exdir = taxon_dir)

    df <- tryCatch({
      dt     <- data.table::fread(extracted_csv, fill = TRUE, showProgress = FALSE)
      df_out <- as.data.frame(lapply(dt, as.character), stringsAsFactors = FALSE)
      df_out$taxon_downloaded <- taxon
      df_out
    }, error = function(e) {
      message("  -> Error reading CSV for ", taxon, ": ", conditionMessage(e))
      NULL
    })

    if (!is.null(df)) {
      combined_list[[taxon]] <- df
      cat("  -> Done (rows:", nrow(df), ")\n")
    }
  }

  if (length(combined_list) == 0) {
    cat("No data found in any ZIPs.\n")
    return(NULL)
  }

  combined_df  <- as.data.frame(data.table::rbindlist(combined_list, fill = TRUE),
                                stringsAsFactors = FALSE)
  combined_csv <- file.path(zip_dir, "combined_greece_fish_occurrences.csv")
  data.table::fwrite(combined_df, combined_csv)
  cat("\nCombined CSV saved to:", combined_csv, "\n")
  combined_df
}

# ── Run ──────────────────────────────────────────────────────────────────────

if (MODE == "order") {
  taxa <- med_fish_orders
  rank <- "ORDER"
} else if (MODE == "species") {
  taxa <- species_list
  rank <- "SPECIES"
} else {
  stop("MODE must be either 'order' or 'species'")
}

submitted_keys   <- submit_gbif_downloads(taxa, greece_poly, rank,
                                          batch_size = 5, wait_time = 60)
download_zips(submitted_keys, download_path)
combined_fish_df <- extract_and_combine(submitted_keys, download_path)

# ── Summary ──────────────────────────────────────────────────────────────────

cat(fread(file.path(download_path, "combined_greece_fish_occurrences.csv")) %>% colnames(), sep = "\n")

message("\n", paste(rep("=", 60), collapse = ""))
message("GBIF DOWNLOAD COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("  Mode:              %s", MODE))
message(sprintf("  Total rows:        %d", nrow(combined_fish_df)))
message(sprintf("  Total columns:     %d", ncol(combined_fish_df)))
message(sprintf("  Taxa downloaded:   %d", length(unique(combined_fish_df$taxon_downloaded))))
message("\nNext step: ~/Documents/Postdoc/projects/workflow_paper/scripts/02_clean_gbif_data.R")
# ============================================================================
# CLEAN ALL DAM DATA
# ============================================================================
# Purpose: Process and combine dam data from multiple sources
# Sources:
#   1. RAAY Small Hydropower Plants (CSV)
#   2. AMBER Dams Database (CSV)
# Output: Combined cleaned dam dataset ready for snapping
# ============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(sf)
library(leaflet)
library(htmlwidgets)
# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# ============================================================================
# SETUP PATHS
# ============================================================================

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)


# Create directory structure if needed
dir.create("points_original/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_original/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/dams", recursive = TRUE, showWarnings = FALSE)


# ============================================================================
# 1. PROCESS RAAY SMALL HYDROPOWER PLANTS
# ============================================================================

message("\n=== Processing RAAY Hydropower Data ===")
# Define input directory and file mapping
csv_dir <- "points_original/dams"

file_map <- c(
  "V_SDI_R_HYDRO12_Installation_Licence.csv" = "IL",  # Installation Permit
  "V_SDI_R_HYDRO13_Operational_Licence.csv"  = "OL",  # Operational Permit
  "V_SDI_R_HYDRO11_Production_Licence.csv"   = "PL",  # Production Permit
  "V_SDI_R_HYDRO7_Evaluation.csv"            = "E"   # Evaluation
)

# Function to process each CSV file
read_and_process_raay <- function(file_name, phase_code) {
  file_path <- file.path(csv_dir, file_name)

  # Check if file exists
  if (!file.exists(file_path)) {
    warning(sprintf("File not found: %s", file_path))
    return(NULL)
  }

  df <- fread(file_path)

  # Rename status column if needed
  if ("katastash_code" %in% names(df)) {
    df <- rename(df, status_code = katastash_code)
  }

  # Process geometry column
  if ("geometry" %in% names(df)) {
    df <- df %>%
      mutate(
        geometry = str_remove_all(geometry, "MULTIPOINT|\\(|\\)"),
        geometry = str_trim(geometry)
      ) %>%
      separate_rows(geometry, sep = ",") %>%
      group_by(across(-geometry)) %>%
      slice(1) %>%  # Keep only first coordinate pair per row
      ungroup() %>%
      separate(geometry, into = c("longitude", "latitude"), sep = " ", convert = TRUE)
  }

  # Filter for Y/L part if column exists
  if ("part" %in% names(df)) {
    df <- df %>% filter(part == "Y/L")
  }

  # Add phase and type
  df <- mutate(df,
               phase = phase_code,
               type = "shp",  # small hydropower plant
               source = "RAAY",
               status = if_else(phase_code == "OL", "existing", "planned"))

  # keep only the most recent record per location
    df <- df %>%
      group_by(longitude, latitude) %>%
      arrange(desc(imerominia)) %>%
      slice(1) %>%
      ungroup()

  return(df)
}

# Process all CSV files
raay_dams <- imap_dfr(names(file_map), ~ read_and_process_raay(.x, file_map[[.x]]))

# keep only the most recent record per location
raay_dams <- raay_dams %>%
  group_by(longitude, latitude) %>%
  arrange(desc(imerominia)) %>%
  slice(1) %>%
  ungroup()


# Check if any data was loaded
if (nrow(raay_dams) == 0) {
  stop("No data loaded from CSV files. Check file paths and names.")
}

# Select relevant columns
raay_dams <- raay_dams %>%
  select(id1, phase, status_code, longitude, latitude,
         type, source, status, imerominia) %>%
  rename(date = imerominia)

message(sprintf("RAAY: Processed %d hydropower plants", nrow(raay_dams)))
message(sprintf("  Phases: %s", paste(unique(raay_dams$phase), collapse = ", ")))

# Save individual file
fwrite(raay_dams, "points_cleaned/dams/dams_raay_clean.csv")

# Convert to sf object and save as GPKG
raay_dams_sf <- st_as_sf(raay_dams,
                         coords = c("longitude", "latitude"),
                         crs = 4326,  # WGS84
                         remove = FALSE)  # Keep lon/lat columns
# Save to nimbus
save_to_nimbus(raay_dams_sf, "points_cleaned/dams/dams_raay_clean.gpkg")

# Save locally
st_write(raay_dams_sf, "points_cleaned/dams/dams_raay_clean.gpkg")

message("GPKG file saved: points_cleaned/dams/dams_raay_clean.gpkg")

# ============================================================================
# 2. PROCESS AMBER DAMS
# ============================================================================

message("\n=== Processing AMBER Dams Data ===")

amber_file <- "points_original/dams/atlas-country-Greece.csv"

amber_dams <- read.csv(amber_file) %>%
  select(GUID, Longitude_WGS84, Latitude_WGS84, type) %>%
  rename(
    longitude = Longitude_WGS84,
    latitude = Latitude_WGS84
  ) %>%
  mutate(
    type = "dam",
    phase = "amber",
    status_code = "existing",
    source = "AMBER",
    status = "existing"
  ) %>%
  mutate(GUID = gsub("\\{|\\}", "", GUID)) %>%  # Remove { or } from GUID
  mutate(id1 = sapply(strsplit(as.character(GUID), "-"), `[`, 5), .before = GUID) %>%
  select(id1, phase, status_code, longitude, latitude, type, source, status) %>%
  mutate(date = NA)

message(sprintf("AMBER: Processed %d dams", nrow(amber_dams)))

# Save individual file locally
fwrite(amber_dams, "points_cleaned/dams/dams_amber_clean.csv")

# Convert to sf object and save as GPKG
amber_dams_sf <- st_as_sf(amber_dams,
                          coords = c("longitude", "latitude"),
                          crs = 4326,  # WGS84
                          remove = FALSE)  # Keep lon/lat columns
save_to_nimbus(amber_dams_sf, "points_cleaned/dams/amber_dams_clean.gpkg")

# Save locally
st_write(amber_dams_sf, "points_cleaned/dams/amber_dams_clean.gpkg")


# ============================================================================
# 3. COMBINE ALL DAM DATASETS
# ============================================================================

message("\n=== Combining All Dam Datasets ===")

dams_all <- rbind(
  raay_dams,
  amber_dams
)

# Summary statistics
message(sprintf("\nCombined Dataset Summary:"))
message(sprintf("  Total dams: %d", nrow(dams_all)))
message(sprintf("  By source:"))
for (src in unique(dams_all$source)) {
  n <- sum(dams_all$source == src)
  pct <- round(100 * n / nrow(dams_all), 1)
  message(sprintf("    %s: %d (%.1f%%)", src, n, pct))
}

message(sprintf("\n  RAAY by phase:"))
raay_summary <- dams_all %>%
  filter(source == "RAAY") %>%
  group_by(phase) %>%
  summarise(n = n(), .groups = "drop")
for (i in 1:nrow(raay_summary)) {
  message(sprintf("    %s: %d", raay_summary$phase[i], raay_summary$n[i]))
}

# Save combined file
fwrite(dams_all, "points_cleaned/dams/dams_all_clean.csv")

message("\n✓ All dam data cleaned and saved")

# ============================================================================
# 4. VISUALIZATION: ALL DAM SOURCES
# ============================================================================

message("\n=== Creating Visualization ===")

# Color palette for sources
source_colors <- colorFactor(
  palette = c("RAAY" = "blue", "AMBER" = "green"),
  domain = c("RAAY", "AMBER")
)

# Color palette for RAAY phases
phase_colors <- colorFactor(
  palette = c("IL" = "lightblue", "OL" = "darkblue", "PL" = "purple",
              "E" = "orange"),
  domain = c("IL", "OL", "PL", "E")
)

# Create overview map
dams_map <- leaflet(dams_all) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # All dams colored by source
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~source_colors(source),
    fillColor = ~source_colors(source),
    radius = 4,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "By Source",
    popup = ~paste0(
      "<b>Dam ID: </b>", id1, "<br>",
      "<b>Source: </b>", source, "<br>",
      "<b>Phase: </b>", phase, "<br>",
      "<b>Status: </b>", status_code, "<br>",
      "<b>Type: </b>", type, "<br>",
      "<b>Lon: </b>", round(longitude, 5), "<br>",
      "<b>Lat: </b>", round(latitude, 5)
    )
  ) %>%

  # RAAY dams colored by phase
  addCircleMarkers(
    data = dams_all %>% filter(source == "RAAY"),
    lng = ~longitude,
    lat = ~latitude,
    color = ~phase_colors(phase),
    fillColor = ~phase_colors(phase),
    radius = 4,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "RAAY by Phase",
    popup = ~paste0(
      "<b>Dam ID: </b>", id1, "<br>",
      "<b>Phase: </b>", phase, "<br>",
      "<b>Status: </b>", status_code, "<br>",
      "<b>Lon: </b>", round(longitude, 5), "<br>",
      "<b>Lat: </b>", round(latitude, 5)
    )
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("By Source", "RAAY by Phase"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend for sources
  addLegend(
    position = "bottomright",
    pal = source_colors,
    values = ~source,
    title = "Dam Source",
    opacity = 0.7,
    group = "By Source"
  ) %>%

  # Legend for RAAY phases
  addLegend(
    position = "bottomleft",
    pal = phase_colors,
    values = dams_all %>% filter(source == "RAAY") %>% pull(phase),
    title = "RAAY Phase",
    opacity = 0.7,
    group = "RAAY by Phase"
  ) %>%

  # Hide phase legend initially
  hideGroup("RAAY by Phase")

# Save map
save_to_nimbus(dams_map, "points_cleaned/maps/dams_all_sources_overview.html")

# Save map locally
saveWidget(dams_map, "points_cleaned/maps/dams_all_sources_overview.html")


message("\n✓ Visualization complete")

# ============================================================================
# 5. DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Checks ===")

# Check for missing coordinates
missing_coords <- dams_all %>%
  filter(is.na(longitude) | is.na(latitude))

if (nrow(missing_coords) > 0) {
  message(sprintf("⚠️  WARNING: %d dams have missing coordinates", nrow(missing_coords)))
  message("  Sources affected:")
  print(table(missing_coords$source))
} else {
  message("✓ No missing coordinates")
}

# Check for duplicates
duplicates <- dams_all %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1) %>%
  arrange(longitude, latitude)

if (nrow(duplicates) > 0) {
  message(sprintf("⚠️  WARNING: %d records at duplicate locations", nrow(duplicates)))
  message("  These may be the same dam in multiple databases")
} else {
  message("✓ No duplicate locations")
}

# Check coordinate ranges (Greece roughly: lon 19-28, lat 34-42)
out_of_bounds <- dams_all %>%
  filter(longitude < 19 | longitude > 28 | latitude < 34 | latitude > 42)

if (nrow(out_of_bounds) > 0) {
  message(sprintf("⚠️  WARNING: %d dams outside Greece bounds", nrow(out_of_bounds)))
  message("  Check these manually")
} else {
  message("✓ All coordinates within Greece bounds")
}




# ============================================================================
# 6. DUPLICATE ANALYSIS AND VISUALIZATION
# ============================================================================

message("\n=== Analyzing Duplicates ===")

# Find duplicates with detailed information
duplicates_detailed <- dams_all %>%
  group_by(longitude, latitude) %>%
  mutate(
    n_at_location = n(),
    is_duplicate = n() > 1
  ) %>%
  ungroup() %>%
  filter(is_duplicate) %>%
  arrange(longitude, latitude, source)

if (nrow(duplicates_detailed) > 0) {
  message(sprintf("Found %d records at %d duplicate locations",
                  nrow(duplicates_detailed),
                  n_distinct(duplicates_detailed[, c("longitude", "latitude")])))

  # Statistics by source combination
  duplicate_combos <- duplicates_detailed %>%
    group_by(longitude, latitude) %>%
    summarise(
      sources = paste(sort(unique(source)), collapse = " + "),
      n_sources = n_distinct(source),
      total_records = n(),
      .groups = "drop"
    )

  message("\nDuplicate location breakdown:")
  message(sprintf("  Locations with records from multiple sources: %d",
                  sum(duplicate_combos$n_sources > 1)))
  message(sprintf("  Locations with records from same source: %d",
                  sum(duplicate_combos$n_sources == 1)))

  message("\nBy source combination:")
  combo_summary <- duplicate_combos %>%
    group_by(sources) %>%
    summarise(
      n_locations = n(),
      total_records = sum(total_records),
      .groups = "drop"
    ) %>%
    arrange(desc(n_locations))

  for (i in 1:nrow(combo_summary)) {
    message(sprintf("  %s: %d locations (%d records)",
                    combo_summary$sources[i],
                    combo_summary$n_locations[i],
                    combo_summary$total_records[i]))
  }

  # Save duplicates to CSV
  fwrite(duplicates_detailed, "points_cleaned/dams/dams_duplicates.csv")
  message("\n✓ Duplicates saved to: points_cleaned/dams/dams_duplicates.csv")

  # Create visualization of duplicates
  # Color palette for duplicate visualization
  dup_source_colors <- colorFactor(
    palette = c("RAAY" = "red", "AMBER" = "orange"),
    domain = c("RAAY", "AMBER")
  )

  duplicates_map <- leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

    # Add duplicate points
    addCircleMarkers(
      data = duplicates_detailed,
      lng = ~longitude,
      lat = ~latitude,
      color = ~dup_source_colors(source),
      fillColor = ~dup_source_colors(source),
      radius = 6,
      fillOpacity = 0.8,
      stroke = TRUE,
      weight = 2,
      popup = ~paste0(
        "<b style='color:red;'>DUPLICATE LOCATION</b><br>",
        "<b>Records at this location: </b>", n_at_location, "<br>",
        "<b>Dam ID: </b>", id1, "<br>",
        "<b>Source: </b>", source, "<br>",
        "<b>Phase: </b>", phase, "<br>",
        "<b>Status: </b>", status_code, "<br>",
        "<b>Type: </b>", type, "<br>",
        "<b>Lon: </b>", round(longitude, 6), "<br>",
        "<b>Lat: </b>", round(latitude, 6)
      ),
      label = ~paste0("Duplicate: ", n_at_location, " records (", source, ")")
    ) %>%

    # Add layer controls
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%

    # Add legend
    addLegend(
      position = "bottomright",
      pal = dup_source_colors,
      values = duplicates_detailed$source,
      title = "Duplicate Source",
      opacity = 0.8
    )

  # Save duplicates map to nimbus
  save_to_nimbus(duplicates_map, "points_cleaned/maps/dams_duplicates_map.html")
  message("✓ Duplicate visualization saved: points_cleaned/maps/dams_duplicates_map.html")

  # Save duplicates map locally
  saveWidget(duplicates_map, "points_cleaned/maps/dams_duplicates_map.html")
  message("✓ Duplicate visualization saved: points_cleaned/maps/dams_duplicates_map.html")



  # Create comparison table for cross-source duplicates
  cross_source_dups <- duplicates_detailed %>%
    group_by(longitude, latitude) %>%
    filter(n_distinct(source) > 1) %>%
    arrange(longitude, latitude, source) %>%
    select(longitude, latitude, source, id1, phase, status_code, type) %>%
    ungroup()

  if (nrow(cross_source_dups) > 0) {
    message(sprintf("\n⚠️  Found %d records at locations appearing in multiple databases",
                    nrow(cross_source_dups)))
    fwrite(cross_source_dups, "points_cleaned/dams/dams_cross_source_duplicates.csv")
    message("✓ Cross-source duplicates saved to: points_cleaned/dams_cross_source_duplicates.csv")
  }

} else {
  message("✓ No duplicate locations found")
}


# ============================================================================
# SUMMARY
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message(sprintf("\nFiles created:"))
message(sprintf("  - points_cleaned/dams_raay_clean.csv (%d dams)", nrow(raay_dams)))
message(sprintf("  - points_cleaned/dams_amber_clean.csv (%d dams)", nrow(amber_dams)))
message(sprintf("  - points_cleaned/dams_all_clean.csv (%d dams)", nrow(dams_all)))
message("\nMap created:")
message("  - points_original/maps/dams_all_sources_overview.html")
message("\nAll files copied to Nimbus")
message("\n✓ Ready for snapping!")



# ============================================================================
# CLEAN GBIF FISH OCCURRENCE DATA
# ============================================================================
# Purpose: Clean, spatially validate, and taxonomically check freshwater fish
#          occurrence records from GBIF for Greece
# Input: Raw GBIF CSV (may have malformed rows)
# Output: Cleaned GBIF fish occurrences ready for snapping
# ============================================================================

library(data.table)
library(dplyr)
library(tidyr)
library(CoordinateCleaner)
library(specleanr)
library(rfishbase)
library(readr)
library(leaflet)
library(htmlwidgets)

# Install danubeoccurR if needed
# pak::pak("ytorres-cambas/danubeoccurR")
library(danubeoccurR)

# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# ============================================================================
# SETUP PATHS
# ============================================================================

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# Create directory structure
dir.create("points_original/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_original/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)


# ============================================================================
# 1. READ AND FIX MALFORMED CSV
# ============================================================================

message("\n=== Step 1: Reading and Fixing Raw GBIF Data ===")

file_raw <- file.path(BASE_DIR, "points_original/fish/combined_greece_fish_occurrences.csv")

# Read with fread (handles large files efficiently)
gbif <- fread(file_raw,
              quote = "\"",
              fill = Inf)

message(sprintf("Initial read: %d rows, %d columns", nrow(gbif), ncol(gbif)))

# ============================================================================
# 1a. Remove rows with extra V columns (malformed rows)
# ============================================================================

# Identify V columns (overflow columns from malformed rows)
v_cols <- grep("^V[0-9]+$", names(gbif), value = TRUE)

if (length(v_cols) > 0) {
  message(sprintf("Found %d overflow columns (V*) - removing malformed rows", length(v_cols)))

  # Keep only rows where all V columns are empty
  gbif_tmp <- gbif[rowSums(gbif[, ..v_cols] != "", na.rm = TRUE) == 0]
  gbif_tmp <- gbif_tmp[, !..v_cols]  # Drop V columns

  message(sprintf("Removed %d malformed rows", nrow(gbif) - nrow(gbif_tmp)))
} else {
  gbif_tmp <- gbif
  message("No overflow columns found")
}

# ============================================================================
# 1b. Fix problematic longitude values
# ============================================================================

# Check for unrealistic longitude values
if ("decimalLongitude" %in% names(gbif_tmp)) {
  bad_lons <- gbif_tmp %>%
    filter(!is.na(decimalLongitude), decimalLongitude > 1000)

  if (nrow(bad_lons) > 0) {
    message(sprintf("Found %d rows with invalid longitude (>1000)", nrow(bad_lons)))
    gbif_tmp <- gbif_tmp %>% filter(decimalLongitude < 1000 | is.na(decimalLongitude))
  }
}

# ============================================================================
# 1c. Use readr to catch remaining parsing issues
# ============================================================================

# Save temporary file
temp_file <- tempfile(fileext = ".csv")
fwrite(gbif_tmp, temp_file)

message(sprintf("Saved %d rows to temp file for validation", nrow(gbif_tmp)))

# Read with readr to detect remaining problems
gbif_tmp <- read_csv(temp_file, show_col_types = FALSE)

# Get problematic row numbers
prob_rows <- problems(gbif_tmp)$row

if (length(prob_rows) > 0) {
  message(sprintf("Found %d rows with parsing issues - removing", length(prob_rows)))
  gbif_clean <- gbif_tmp[-prob_rows, ]
} else {
  message("No parsing issues found")
  gbif_clean <- gbif_tmp
}

# Clean up temp file
unlink(temp_file)

# Save the fixed raw file
save_to_nimbus(gbif_clean, "points_original/fish/combined_greece_fish_occurrences_fixed.csv")

# delete?
fwrite(gbif_clean, "points_original/fish/combined_greece_fish_occurrences_fixed.csv")

message(sprintf("\n✓ CSV fixing complete: %d clean rows retained", nrow(gbif_clean)))

# ============================================================================
# 2. INITIAL EXPLORATION
# ============================================================================

message("\n=== Step 2: Initial Exploration ===")

gbif_raw <- fread("points_original/fish/combined_greece_fish_occurrences_fixed.csv")
gbif_raw <- gbif_clean

message(sprintf("Dataset dimensions: %d rows × %d columns", nrow(gbif_raw), ncol(gbif_raw)))
message("\nColumn names:")
print(names(gbif_raw))

# Check for duplicates
duplicates <- gbif_raw %>%
  count(decimalLongitude, decimalLatitude, speciesKey, datasetKey, year) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  message(sprintf("\n⚠️  Found %d duplicate coordinate-species-year combinations", nrow(duplicates)))
} else {
  message("\n✓ No exact duplicates found")
}

# ============================================================================
# 3. COORDINATE TYPE CONVERSION
# ============================================================================


## First filter occurrences only from Greece
gbif_raw <- gbif_raw %>% filter(countryCode=="GR")

message("\n=== Step 3: Converting Coordinates to Numeric ===")

gbif_raw <- gbif_raw %>%
  mutate(
    decimalLatitude  = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude)
  )

# Check coordinate ranges (Greece: lon ~19-28, lat ~34-42)
coord_summary <- gbif_raw %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  summarise(
    min_lon = min(decimalLongitude),
    max_lon = max(decimalLongitude),
    min_lat = min(decimalLatitude),
    max_lat = max(decimalLatitude)
  )

message(sprintf("Longitude range: %.2f to %.2f", coord_summary$min_lon, coord_summary$max_lon))
message(sprintf("Latitude range: %.2f to %.2f", coord_summary$min_lat, coord_summary$max_lat))

# ============================================================================
# 4. METADATA-BASED FILTERING
# ============================================================================

message("\n=== Step 4: Metadata Filtering ===")

n_before <- nrow(gbif_raw)

gbif_metadata_filtered <- gbif_raw %>%
  filter(!is.na(year)) %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999))

n_after <- nrow(gbif_metadata_filtered)
message(sprintf("Removed %d rows (%.1f%%) based on metadata filters",
                n_before - n_after,
                100 * (n_before - n_after) / n_before))

# ============================================================================
# 5. SPATIAL FILTERS (CoordinateCleaner)
# ============================================================================

message("\n=== Step 5: Spatial Filtering with CoordinateCleaner ===")

n_before <- nrow(gbif_metadata_filtered)

gbif_spatial_cleaned <- gbif_metadata_filtered %>%
  cc_cen(buffer = 1000, verbose = TRUE) %>%
  cc_cap(buffer = 1000, verbose = TRUE) %>%
  cc_inst(buffer = 1000, verbose = TRUE) %>%
  distinct(
    decimalLongitude,
    decimalLatitude,
    speciesKey,
    datasetKey,
    year,
    .keep_all = TRUE
  ) %>%
  filter(!is.na(species), species != "")

n_after <- nrow(gbif_spatial_cleaned)
message(sprintf("Removed %d rows (%.1f%%) with spatial filters",
                n_before - n_after,
                100 * (n_before - n_after) / n_before))

# ============================================================================
# 6. TAXONOMIC VALIDATION
# ============================================================================

message("\n=== Step 6: Taxonomic Name Checking ===")
message("This may take several minutes...")

gbif_taxa_checked <- check_species_name(
  data = gbif_spatial_cleaned,
  col_species_name = "species",
  target_accuracy = 90,
  accuracy_decrement = NULL,
  verbose = TRUE,
  manual = TRUE
)

message(sprintf("Taxonomic validation complete: %d species validated",
                length(unique(gbif_taxa_checked$species))))

# ============================================================================
# 7. MANUAL CURATION
# ============================================================================

message("\n=== Step 7: Manual Curation ===")

# Remove specific problematic rows if identified during manual review
# Adjust row numbers based on your manual inspection
# gbif_taxa_curated <- gbif_taxa_checked[-c(5836:5840), ]

# For now, keep all
gbif_taxa_curated <- gbif_taxa_checked

message(sprintf("After manual curation: %d rows", nrow(gbif_taxa_curated)))

# ============================================================================
# 8. FINAL CLEANING AND COLUMN SELECTION
# ============================================================================

message("\n=== Step 8: Final Cleaning ===")

gbif_cleaned <- gbif_taxa_curated %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  select(
    gbifID,
    datasetKey,
    class,
    order,
    family,
    genus,
    species,
    year,
    month,
    day,
    decimalLongitude,
    decimalLatitude
  ) %>%
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  ) %>%
  filter(year > 1980)

message(sprintf("\nFinal cleaned dataset: %d rows", nrow(gbif_cleaned)))

# ============================================================================
# 9. VISUALIZATION
# ============================================================================

message("\n=== Step 9: Creating Visualization ===")

# Create overview map
gbif_map <- leaflet(gbif_cleaned) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(
    lng = ~decimalLongitude,
    lat = ~decimalLatitude,
    radius = 3,
    color = "blue",
    fillColor = "blue",
    fillOpacity = 0.5,
    stroke = FALSE,
    popup = ~paste0(
      "<b>Species:</b> ", species, "<br>",
      "<b>Family:</b> ", family, "<br>",
      "<b>Year:</b> ", year, "<br>",
      "<b>GBIF ID:</b> ", gbifID
    )
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save map
save_to_nimbus(gbif_map, "points_cleaned/maps/gbif_fish_cleaned_overview.html")

# Save locally
saveWidget(gbif_map, "points_cleaned/maps/gbif_fish__cleaned_overview.html")

# ============================================================================
# 10. SAVE CLEANED DATA
# ============================================================================

message("\n=== Step 10: Saving Cleaned Data ===")

# Save to Nimbus
save_to_nimbus(gbif_cleaned, "points_cleaned/fish/fish_gbif_clean.csv")

# Save locally
fwrite(gbif_cleaned, "points_cleaned/fish/fish_gbif_clean.csv")

gbif_cleaned <- fread("points_cleaned/fish/fish_gbif_clean.csv")

# Remove duplicates, keep only coordinates and gbifID to snap
gbif_cleaned_unique <- gbif_cleaned %>%
  distinct(decimalLongitude,decimalLatitude, .keep_all = TRUE)


# GBIF data of fish might include records in the sea that we do not need
# We can test if this is the case, trying to obtain the regional unit ID of the
# occurrences. In case any occurrences are not assigned an ID, it means they fall in the sea
# and we can discard them from further steps. We chose regional unit id because
# it is a higher level of organisation than basin or subcatchment id,
# and thus it is faster to obtain it from the database


system.time(reg_unit_ids <- api_get_local_ids(data = gbif_cleaned_unique,
                                              colname_lon = "decimalLongitude",
                                              colname_lat = "decimalLatitude",
                                              colname_site_id = "gbifID"))


# Keep only occurrences that were assigned regional unit id
# Get site_id of these occurrences
site_ids_keep <- reg_unit_ids %>% filter(!is.na(reg_id)) %>%
  pull(gbifID)

# filter data to keep these site_ids
gbif_cleaned_to_snap <- gbif_cleaned_unique %>%
  filter(gbifID %in% site_ids_keep)

fwrite(gbif_cleaned_to_snap, "points_cleaned/fish/fish_gbif_clean_to_snap.csv")


# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("GBIF DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))

message(sprintf("\nCleaning Summary:"))
message(sprintf("  Original rows: %d", nrow(gbif)))
message(sprintf("  After CSV fixing: %d (%.1f%% retained)",
                nrow(gbif_clean),
                100 * nrow(gbif_clean) / nrow(gbif)))
message(sprintf("  After metadata filters: %d", nrow(gbif_metadata_filtered)))
message(sprintf("  After spatial filters: %d", nrow(gbif_spatial_cleaned)))
message(sprintf("  After taxonomic validation: %d", nrow(gbif_taxa_checked)))
message(sprintf("  Final cleaned: %d (%.1f%% of original)",
                nrow(gbif_cleaned),
                100 * nrow(gbif_cleaned) / nrow(gbif)))

message(sprintf("\nTaxonomic Summary:"))
message(sprintf("  Unique species: %d", length(unique(gbif_cleaned$species))))
message(sprintf("  Unique families: %d", length(unique(gbif_cleaned$family))))
message(sprintf("  Unique genera: %d", length(unique(gbif_cleaned$genus))))

message(sprintf("\nTemporal Coverage:"))
year_range <- range(gbif_cleaned$year, na.rm = TRUE)
message(sprintf("  Year range: %d - %d", year_range[1], year_range[2]))

message(sprintf("\nSpatial Coverage:"))
message(sprintf("  Longitude: %.2f to %.2f",
                min(gbif_cleaned$decimalLongitude),
                max(gbif_cleaned$decimalLongitude)))
message(sprintf("  Latitude: %.2f to %.2f",
                min(gbif_cleaned$decimalLatitude),
                max(gbif_cleaned$decimalLatitude)))

message("\nFiles created:")
message("  - points_original/fish/combined_greece_fish_occurrences_fixed.csv")
message("  - points_cleaned/fish/fish_gbif_clean.csv")
message("  - points_original/maps/gbif_fish_cleaned_overview.html")
message("\n✓ Ready for snapping!")

# ============================================================================
# CLEAN HCMR FISH OCCURRENCE DATA
# ============================================================================
# Purpose: Clean and format freshwater fish occurrence records from HCMR
#          Excel file for Greece (includes Barbus abundance data)
# Input: Raw HCMR Excel file with species presence/absence matrix + Barbus sheet
# Output: Single cleaned HCMR fish occurrence file ready for snapping
# ============================================================================

library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmlwidgets)

# Load helper function
source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")

# ============================================================================
# SETUP PATHS
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HCMR FISH DATA CLEANING")
message(paste(rep("=", 80), collapse = ""))

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)


# Create directory structure
dir.create("points_cleaned/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# PART 1: PROCESS MAIN DATASET (ALL SPECIES)
# ============================================================================

message("\n=== Step 1: Processing Main Dataset (All Species) ===")

# Clean fish data
sp_raw <- read_xlsx("points_original/fish/Fish distributional & traits data (1).xlsx") %>%
  rename(longitude = Longtitude,
         latitude  = Latitude)

message(sprintf("Loaded %d rows and %d columns", nrow(sp_raw), ncol(sp_raw)))

# Check for inverted lat/lon
sp <- sp_raw %>%
  mutate(
    inverted_lat_lon = latitude >= 19 & latitude <= 29 &
      longitude >= 34 & longitude <= 42
  )

n_inverted <- sum(sp$inverted_lat_lon, na.rm = TRUE)
message(sprintf("Found %d rows with inverted lat/lon", n_inverted))

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

message("✓ Coordinates fixed")

# keep the dry or fishless locations separated
dry_fishless <- sp %>% filter(!is.na(DRY) | !is.na(FISHLESS))

message(sprintf("Found %d dry or fishless sites", nrow(dry_fishless)))

if (nrow(dry_fishless) > 0) {
  fwrite(dry_fishless, file.path(BASE_DIR, "points_original/fish/fish_hcmr_dry_fishless_sites.csv"))
  message("✓ Saved dry/fishless sites separately")
}

# keep locations with fish presence
sp <- sp %>% filter(is.na(DRY) | is.na(FISHLESS)) %>%
  select(-DRY, -FISHLESS)

message(sprintf("Retained %d sites with fish presence", length(unique(sp$Sites))))

# flip to longer format
sp <- sp %>% pivot_longer(
  cols = -c(Sites, latitude, longitude),  # keep site info
  names_to = "species",
  values_to = "value"
) %>%
  filter(!is.na(value)) %>%
  select(-value)

message(sprintf("Created %d occurrence records", nrow(sp)))

# check species names
unique_species <- sp %>% select(species) %>% arrange(species) %>% distinct() %>% pull()

message(sprintf("Found %d unique species in main dataset", length(unique_species)))

# Remove invalid latitudes
n_before <- nrow(sp)
sp <- sp %>% filter(latitude > 10)
n_after <- nrow(sp)

if (n_before > n_after) {
  message(sprintf("Removed %d records with latitude <= 10", n_before - n_after))
}

# ============================================================================
# PART 2: PROCESS FISH ABUNDANCE DATA (SARANTAPOROS)
# ============================================================================

message("\n=== Step 2: Processing Fish Abundance Data (Sarantaporos) ===")

# Check if Sarantaporos file exists
sarantaporos_file <- "points_original/fish/Sarantaporos.xlsx"

if (file.exists(sarantaporos_file)) {
  message(sprintf("Found Barbus file: %s", sarantaporos_file))

    # Read and identify species columns (everything except site/coordinate cols)
    raw <- read_xlsx(sarantaporos_file) %>%
      rename(longitude = "Latitude",    # Note: swapped in original file
             latitude  = "Longtitude") %>%
      mutate(site_id = paste0("Sarant_", row_number()))

    coord_cols <- c("site_id", "longitude", "latitude")
    species_cols <- setdiff(names(raw), coord_cols)[-1]

    message(sprintf("Found %d species columns: %s",
                    length(species_cols),
                    paste(species_cols, collapse = ", ")))

    # Pivot to long format, keep only presences
    sarantaporos_long <- raw %>%
      select(all_of(c(coord_cols, species_cols))) %>%
      pivot_longer(
        cols      = all_of(species_cols),
        names_to  = "species",
        values_to = "abundance"
      ) %>%
      filter(!is.na(abundance), abundance > 0) %>%
      mutate(
        Sites   = site_id,
        species = gsub(" ", "_", species)   # match naming convention
      ) %>%
      select(Sites, latitude, longitude, species)

    message(sprintf("Converted to %d presence records across %d species",
                    nrow(sarantaporos_long),
                    n_distinct(sarantaporos_long$species)))

    # Spatial validation
    n_before <- nrow(sarantaporos_long)
    sarantaporos_long <- sarantaporos_long %>%
      filter(
        !is.na(latitude), !is.na(longitude),
        latitude  > 10,
        longitude >= 19, longitude <= 29,
        latitude  >= 34, latitude  <= 42
      )
    n_after <- nrow(sarantaporos_long)

    if (n_before > n_after) {
      message(sprintf("Removed %d records with invalid coordinates", n_before - n_after))
    }

    # Save standalone Sarantaporos file (for reference)
    fwrite(sarantaporos_long,
           file.path(BASE_DIR, "points_cleaned/fish/spdata_sarantaporos_clean.csv"))
    message("✓ Saved standalone Sarantaporos file: spdata_sarantaporos_clean.csv")

    # ============================================================================
    # COMBINE MAIN + SARANTAPOROS
    # ============================================================================

    message("\n=== Step 3: Combining Main and Sarantaporos Datasets ===")

    sp <- sp %>% mutate(latitude  = as.numeric(latitude),
                        longitude = as.numeric(longitude))
    sp_combined <- bind_rows(sp, sarantaporos_long)

    message(sprintf("Combined dataset: %d total records", nrow(sp_combined)))
    message(sprintf("  - Main dataset:        %d records", nrow(sp)))
    message(sprintf("  - Sarantaporos data:   %d records", nrow(sarantaporos_long)))

  } else {
    message(sprintf("⚠️  Sarantaporos file not found: %s", sarantaporos_file))
    message("Using only main dataset")
    sp_combined <- sp
  }

# ============================================================================
# FINAL STEPS: SAVE AND VISUALIZE
# ============================================================================

message("\n=== Step 4: Saving Combined Dataset ===")

# check final species names
unique_species_final <- sp_combined %>%
  select(species) %>%
  arrange(species) %>%
  distinct() %>%
  pull()

message(sprintf("Final dataset contains %d unique species:", length(unique_species_final)))
for (sp_name in unique_species_final) {
  message(sprintf("  - %s", sp_name))
}

# Write clean version
fwrite(sp_combined, file.path(BASE_DIR, "points_cleaned/fish/fish_greece_hcmr.csv"))
message("✓ Saved: points_cleaned/fish/fish_greece_hcmr.csv")

# Re-read to verify
sp_combined <- fread(file.path(BASE_DIR, "points_cleaned/fish/fish_greece_hcmr.csv"))
message(sprintf("Verified: %d rows read back", nrow(sp_combined)))

# Get unique coordinates to snap
sp_to_snap <- sp_combined %>% distinct(Sites, longitude, latitude)

message(sprintf("Unique sites to snap: %d", nrow(sp_to_snap)))

# write points to snap
fwrite(sp_to_snap, file.path(BASE_DIR, "points_cleaned/fish/fish_points_to_snap_hcmr.csv"))
message("✓ Saved: points_cleaned/fish/fish_points_to_snap_hcmr.csv")

# ============================================================================
# VISUALIZATION
# ============================================================================

message("\n=== Step 5: Creating Visualization ===")

# plot coordinates to check
hcmr_map <- leaflet(sp_combined) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~as.numeric(longitude),
    lat = ~as.numeric(latitude),
    popup = ~paste0("<b>Site:</b> ", Sites, "<br><b>Species:</b> ", species),
    radius = 4,
    color = "red",
    fillOpacity = 0.6
  )

# Save map
saveWidget(hcmr_map,
           file.path(BASE_DIR, "points_cleaned/maps/hcmr_fish_overview.html"),
           selfcontained = TRUE)
message("✓ Created interactive map")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HCMR DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))

message("\nSummary Statistics:")
message(sprintf("  Total occurrence records: %d", nrow(sp_combined)))
message(sprintf("  Unique sites: %d", nrow(sp_to_snap)))
message(sprintf("  Dry/fishless sites excluded: %d", nrow(dry_fishless)))

# Species frequency
species_counts <- sp_combined %>% count(species) %>% arrange(desc(n))
message("\nMost common species:")
for (i in 1:min(20, nrow(species_counts))) {
  message(sprintf("  %d. %s: %d occurrences",
                  i,
                  species_counts$species[i],
                  species_counts$n[i]))
}

# Sites summary
sites_summary <- sp_combined %>%
  count(Sites) %>%
  summarise(
    mean_species = mean(n),
    median_species = median(n),
    max_species = max(n)
  )

message("\nSpecies per site:")
message(sprintf("  Mean: %.1f", sites_summary$mean_species))
message(sprintf("  Median: %.0f", sites_summary$median_species))
message(sprintf("  Maximum: %d", sites_summary$max_species))

# Spatial extent
message("\nSpatial Coverage:")
message(sprintf("  Longitude: %.4f to %.4f",
                min(sp_combined$longitude), max(sp_combined$longitude)))
message(sprintf("  Latitude: %.4f to %.4f",
                min(sp_combined$latitude), max(sp_combined$latitude)))

message("\nFiles Created:")
message("  - points_cleaned/fish/fish_greece_hcmr.csv (all species including Sarantaporos)")
message("  - points_cleaned/fish/fish_points_to_snap_hcmr.csv (unique sites)")
message("  - points_cleaned/maps/hcmr_fish_overview.html")
if (nrow(dry_fishless) > 0) {
  message("  - points_original/fish/fish_hcmr_dry_fishless_sites.csv")
}

message("\n✓ Ready for snapping!")
# ============================================================================
# MERGE SNAPPED COORDINATES WITH SPECIES DATA
# ============================================================================
library(hydrographr)
library(data.table)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

message("\n=== Merging Snapped Data with Species Information ===")

# Load original data with species
fish_hcmr <- fread("points_cleaned/fish/fish_greece_hcmr.csv")
fish_gbif <- fread("points_cleaned/fish/fish_gbif_clean.csv")

# Load snapped data
all_snapped <- fread("points_snapped/fish/all_snapped_fish_points.csv")

# Separate HCMR and GBIF from snapped data
hcmr_snapped <- all_snapped %>% filter(source == "HCMR")
gbif_snapped <- all_snapped %>% filter(source == "GBIF")

# Merge HCMR
fish_hcmr_snapped <- fish_hcmr %>%
  left_join(hcmr_snapped, by = c("Sites" = "site_id")) %>%
  rename(
    longitude_original_hcmr = longitude,
    latitude_original_hcmr = latitude
  ) %>% # exclude the 2 points that were not snapped
  filter(!is.na(longitude_snapped))


message(sprintf("HCMR: %d records with species", nrow(fish_hcmr_snapped)))
message(sprintf("  Successfully snapped: %d (%.1f%%)",
                sum(!is.na(fish_hcmr_snapped$subc_id)),
                100 * sum(!is.na(fish_hcmr_snapped$subc_id)) / nrow(fish_hcmr_snapped)))

# Merge GBIF
fish_gbif_snapped <- fish_gbif %>%
  mutate(gbifID=as.character(gbifID)) %>%
  left_join(gbif_snapped, by = c("gbifID" = "site_id")) %>%
  rename(
    decimalLongitude_original_gbif = decimalLongitude,
    decimalLatitude_original_gbif = decimalLatitude
  ) %>% # filter out the points that were not successfully snapped
  filter(!is.na(longitude_snapped)) %>%
  mutate(species = gsub(" ", "_", species))

message(sprintf("GBIF: %d records with species", nrow(fish_gbif_snapped)))
message(sprintf("  Successfully snapped: %d (%.1f%%)",
                sum(!is.na(fish_gbif_snapped$subc_id)),
                100 * sum(!is.na(fish_gbif_snapped$subc_id)) / nrow(fish_gbif_snapped)))

# Save merged files
write.csv(fish_hcmr_snapped, "points_snapped/fish/fish_hcmr_with_species_snapped.csv")
write.csv(fish_gbif_snapped, "points_snapped/fish/fish_gbif_with_species_snapped.csv")

# Optional: Combine both into one file with standardized columns
fish_all_combined <- rbind(
  fish_hcmr_snapped %>%
    select(site_id = Sites, species, longitude_original, latitude_original,
           longitude_snapped, latitude_snapped, subc_id, strahler,
           distance_metres, source) %>%
    mutate(dataset = "HCMR"),

  fish_gbif_snapped %>%
    select(site_id = gbifID, species, genus, family, order,
           longitude_original, latitude_original,
           longitude_snapped, latitude_snapped, subc_id, strahler,
           distance_metres, source, year, month, day) %>%
    mutate(dataset = "GBIF"),
  fill = TRUE
)

write.csv(fish_all_combined, "points_snapped/fish/fish_all_species_snapped.csv")

message("\n✓ Merged files created:")
message("  - points_snapped/fish_hcmr_with_species_snapped.csv")
message("  - points_snapped/fish_gbif_with_species_snapped.csv")
message("  - points_snapped/fish_all_species_snapped.csv")

# ============================================================================
# SNAP ALL DATA TO STREAM NETWORK
# ============================================================================
# Purpose: Snap both HCMR and GBIF data to stream network using async API
# Input: Cleaned CSV files (from step 02)
# Output: Snapped points with network coordinates
# ============================================================================
library(hydrographr)
library(data.table)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)
# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)


# Create snapped points directories
dir.create("points_snapped/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/maps", recursive = TRUE, showWarnings = FALSE)


# Load cleaned data URLs (adjust these to your actual URLs)
hcmr_url <- "https://nimbus.igb-berlin.de/index.php/s/GxakANySoGxGLnm/download/fish_points_to_snap_hcmr.csv"
gbif_url <- "https://nimbus.igb-berlin.de/index.php/s/ceGgy4n75y92gSk/download/fish_gbif_clean_to_snap.csv"


# ============================================================================
# LOAD ORIGINAL DATA
# ============================================================================

message("\n=== Loading Original Data ===")

# Download original HCMR data
hcmr_original <- fread(hcmr_url)
message(sprintf("Original HCMR points: %d", nrow(hcmr_original)))

# Download original GBIF data
gbif_original <- fread(gbif_url)
message(sprintf("Original GBIF points: %d", nrow(gbif_original)))


# Snapping parameters
STRAHLER_SEQ <- c(4, 3, 2)
DISTANCE_THRESHOLD <- 400  # meters


# ============================================================================
# SNAP HCMR DATA
# ============================================================================

# We have survey data of freshwater fish. In this case we can try to snap directly without cross-checking
# if the points are located at the sea, because we know where they were sampled

message("\n=== Snapping HCMR Data ===")

hcmr_snap_result <- api_get_snapped_points_cascade(
  data = hcmr_original,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "Sites",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
)
print(hcmr_snap_result)

# Save
fwrite(hcmr_snap_result, "points_snapped/fish/hcmr_snapped_points.csv")
message(sprintf("HCMR: Snapped %d points", nrow(hcmr_snap_result)))

hcmr_snap_result <- fread("points_snapped/fish/hcmr_snapped_points.csv")

# ============================================================================
# VISUALIZE HCMR SNAPPING
# ============================================================================

message("\n--- Creating HCMR visualization ---")

# Identify which points were successfully snapped
hcmr_snapped_ids <- hcmr_snap_result$Sites
hcmr_original$snapped <- hcmr_original$Sites %in% hcmr_snapped_ids

# Summary
n_snapped <- sum(hcmr_original$snapped)
n_failed <- sum(!hcmr_original$snapped)
message(sprintf("  Successfully snapped: %d", n_snapped))
message(sprintf("  Failed to snap: %d", n_failed))

# Create map
hcmr_map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Original points that were successfully snapped (blue)
  addCircleMarkers(
    data = hcmr_original[hcmr_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = "blue",
    fillColor = "blue",
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Original (Snapped)",
    popup = ~paste0(
      "<b>Original Point (Successfully Snapped)</b><br>",
      "Site ID: ", Sites, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5)
    )
  ) %>%

  # Original points that FAILED to snap (orange)
  addCircleMarkers(
    data = hcmr_original[!hcmr_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = "orange",
    fillColor = "orange",
    radius = 6,
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Original (Failed)",
    popup = ~paste0(
      "<b>⚠️ Original Point (FAILED TO SNAP)</b><br>",
      "Site ID: ", Sites, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5), "<br>",
      "<b style='color:red;'>This point was not successfully snapped</b>"
    )
  ) %>%

  # Snapped points (red)
  addCircleMarkers(
    data = hcmr_snap_result,
    lng = ~longitude_snapped,
    lat = ~latitude_snapped,
    color = "red",
    fillColor = "red",
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Snapped Points",
    popup = ~paste0(
      "<b>Snapped Point</b><br>",
      "Site ID: ", Sites, "<br>",
      "Lon: ", round(longitude_snapped, 5), "<br>",
      "Lat: ", round(latitude_snapped, 5), "<br>",
      "Distance: ", round(distance_metres, 1), " m<br>",
      "Strahler: ", strahler
    )
  ) %>%

  # Add lines connecting original to snapped (only for successfully snapped)
  addPolylines(
    data = lapply(1:nrow(hcmr_snap_result), function(i) {
      st_linestring(matrix(c(
        hcmr_snap_result$longitude_original[i], hcmr_snap_result$latitude_original[i],
        hcmr_snap_result$longitude_snapped[i], hcmr_snap_result$latitude_snapped[i]
      ), ncol = 2, byrow = TRUE))
    }) %>% st_sfc(crs = 4326),
    color = "gray",
    weight = 1,
    opacity = 0.5,
    group = "Snap Lines"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend
  addLegend(
    position = "bottomright",
    colors = c("blue", "orange", "red", "gray"),
    labels = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    title = "HCMR Data",
    opacity = 0.7
  )

hcmr_map

# Save map
save_to_nimbus(hcmr_map, "points_snapped/maps/hcmr_snapping_check.html")

# Save map locally
saveWidget(hcmr_map, "points_snapped/maps/hcmr_snapping_check.html")

# Print snapping statistics
message(sprintf("\nHCMR Snapping Statistics:"))
message(sprintf("  Total points: %d", nrow(hcmr_snap_result)))
message(sprintf("  Mean distance: %.1f m", mean(hcmr_snap_result$distance_metres)))
message(sprintf("  Median distance: %.1f m", median(hcmr_snap_result$distance_metres)))
message(sprintf("  Max distance: %.1f m", max(hcmr_snap_result$distance_metres)))
message(sprintf("  Points within %d m: %d (%.1f%%)",
                DISTANCE_THRESHOLD,
                sum(hcmr_snap_result$distance_metres <= DISTANCE_THRESHOLD),
                100 * sum(hcmr_snap_result$distance_metres <= DISTANCE_THRESHOLD) / nrow(hcmr_snap_result)))

# Save list of failed points
if (n_failed > 0) {
  hcmr_failed <- hcmr_original[!hcmr_original$snapped, ]
  fwrite(hcmr_failed, "points_cleaned/fish/hcmr_failed_to_snap.csv")
  message(sprintf("\nFailed points saved to: points_cleaned/fish/hcmr_failed_to_snap.csv"))
}


# ============================================================================
# SNAP GBIF DATA
# ============================================================================

message("\n=== Snapping GBIF Data ===")

ti <- system.time(gbif_snap_result <- api_get_snapped_points_cascade(
  data = gbif_original,
  colname_lon = "decimalLongitude",
  colname_lat = "decimalLatitude",
  colname_site_id = "gbifID",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
))

# 166 seconds

print(gbif_snap_result)

# Save
min_strahler <- min(STRAHLER_SEQ)
snap_out_path <- paste0("points_snapped/fish/gbif_snapped_points_min_strahler",min_strahler, "_dist_thresh_",DISTANCE_THRESHOLD, ".csv")
fwrite(gbif_snap_result, snap_out_path)

message(sprintf("GBIF: Snapped %d points", nrow(gbif_snap_result)))

gbif_snap_result <- fread(snap_out_path)

## some of the points that failed to snap are in lakes

# ============================================================================
# VISUALIZE GBIF SNAPPING
# ============================================================================

message("\n--- Creating GBIF visualization ---")

# Identify which points were successfully snapped
gbif_snapped_ids <- gbif_snap_result$gbifID
gbif_original$snapped <- gbif_original$gbifID %in% gbif_snapped_ids

# Summary
n_snapped_gbif <- sum(gbif_original$snapped)
n_failed_gbif <- sum(!gbif_original$snapped)
message(sprintf("  Successfully snapped: %d", n_snapped_gbif))
message(sprintf("  Failed to snap: %d", n_failed_gbif))

# Create map
gbif_map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Original points that were successfully snapped (blue)
  addCircleMarkers(
    data = gbif_original[gbif_original$snapped, ],
    lng = ~decimalLongitude,
    lat = ~decimalLatitude,
    color = "blue",
    fillColor = "blue",
    radius = 4,
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    group = "Original (Snapped)",
    popup = ~paste0(
      "<b>Original Point (Successfully Snapped)</b><br>",
      "GBIF ID: ", gbifID, "<br>",
      "Lon: ", round(decimalLongitude, 5), "<br>",
      "Lat: ", round(decimalLatitude, 5)
    )
  ) %>%

  # Original points that FAILED to snap (orange)
  addCircleMarkers(
    data = gbif_original[!gbif_original$snapped, ],
    lng = ~decimalLongitude,
    lat = ~decimalLatitude,
    color = "orange",
    fillColor = "orange",
    radius = 5,
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Original (Failed)",
    popup = ~paste0(
      "<b>⚠️ Original Point (FAILED TO SNAP)</b><br>",
      "GBIF ID: ", gbifID, "<br>",
      "Lon: ", round(decimalLongitude, 5), "<br>",
      "Lat: ", round(decimalLatitude, 5), "<br>",
      "<b style='color:red;'>This point was not successfully snapped</b>"
    )
  ) %>%

  # Snapped points (red)
  addCircleMarkers(
    data = gbif_snap_result,
    lng = ~decimalLongitude_snapped,
    lat = ~decimalLatitude_snapped,
    color = "red",
    fillColor = "red",
    radius = 4,
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    group = "Snapped Points",
    popup = ~paste0(
      "<b>Snapped Point</b><br>",
      "GBIF ID: ", gbifID, "<br>",
      "Lon: ", round(decimalLongitude_snapped, 5), "<br>",
      "Lat: ", round(decimalLatitude_snapped, 5), "<br>",
      "Distance: ", round(distance_metres, 1), " m<br>",
      "Strahler: ", strahler
    )
  ) %>%

  # Add lines connecting original to snapped
  addPolylines(
    data = lapply(1:nrow(gbif_snap_result), function(i) {
      st_linestring(matrix(c(
        gbif_snap_result$decimalLongitude_original[i], gbif_snap_result$decimalLatitude_original[i],
        gbif_snap_result$decimalLongitude_snapped[i], gbif_snap_result$decimalLatitude_snapped[i]
      ), ncol = 2, byrow = TRUE))
    }) %>% st_sfc(crs = 4326),
    color = "gray",
    weight = 1,
    opacity = 0.3,
    group = "Snap Lines"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend
  addLegend(
    position = "bottomright",
    colors = c("blue", "orange", "red", "gray"),
    labels = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    title = "GBIF Data",
    opacity = 0.7
  )

gbif_map

# Save map locally
saveWidget(gbif_map, "points_snapped/maps/gbif_snapping_check.html")

# Save map to nimbus
save_to_nimbus(gbif_map, "points_snapped/maps/gbif_snapping_check.html")

# Print snapping statistics
message(sprintf("\nGBIF Snapping Statistics:"))
message(sprintf("  Original points: %d", nrow(gbif_original)))
message(sprintf("  Successfully snapped: %d (%.1f%%)", n_snapped_gbif, 100 * n_snapped_gbif / nrow(gbif_original)))
message(sprintf("  Failed to snap: %d (%.1f%%)", n_failed_gbif, 100 * n_failed_gbif / nrow(gbif_original)))
if (n_snapped_gbif > 0) {
  message(sprintf("  Mean snap distance: %.1f m", mean(gbif_snap_result$distance_metres)))
  message(sprintf("  Median snap distance: %.1f m", median(gbif_snap_result$distance_metres)))
  message(sprintf("  Max snap distance: %.1f m", max(gbif_snap_result$distance_metres)))
}

# Save list of failed points
if (n_failed_gbif > 0) {
  gbif_failed <- gbif_original[!gbif_original$snapped, ]
  fwrite(gbif_failed, "points_cleaned/fish/gbif_failed_to_snap.csv")
  message(sprintf("\nFailed points saved to: points_cleaned/fish/gbif_failed_to_snap.csv"))
}





# ============================================================================
# COMBINE DATASETS
# ============================================================================

message("\n=== Combining Datasets ===")

hcmr_snap_result$source <- "HCMR"
gbif_snap_result$source <- "GBIF"

# Rename lon, lat, site_id columns of gbif dataset
gbif_snap_result <- gbif_snap_result %>%
  rename(longitude_original = decimalLongitude_original,
         latitude_original = decimalLatitude_original,
         longitude_snapped = decimalLongitude_snapped,
         latitude_snapped = decimalLatitude_snapped,
         site_id = gbifID)

# Rename site_id column of HCMR dataset
hcmr_snap_result <- hcmr_snap_result %>%
  rename(site_id = Sites)

# Get common columns
common_cols <- intersect(names(hcmr_snap_result), names(gbif_snap_result))
all_snapped <- rbind(
  hcmr_snap_result[, ..common_cols],
  gbif_snap_result[, ..common_cols]
)

fwrite(all_snapped, "points_snapped/fish/all_snapped_fish_points.csv")
message(sprintf("Combined: %d total points", nrow(all_snapped)))

# all_snapped <- fread("points_snapped/fish/all_snapped_fish_points.csv")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n=== Snapping Complete ===")
message("\nFinal Statistics:")
message(sprintf("  Total HCMR points: %d", sum(all_snapped$source == "HCMR")))
message(sprintf("  Total GBIF points: %d", sum(all_snapped$source == "GBIF")))
message(sprintf("  Grand total: %d points", nrow(all_snapped)))
message(sprintf("\n  Overall mean snap distance: %.1f m", mean(all_snapped$distance_metres)))
message(sprintf("  Overall median snap distance: %.1f m", median(all_snapped$distance_metres)))

message("\nMaps created:")
message("  - maps/hcmr_snapping_check.html")
message("  - maps/gbif_snapping_check.html")
message("  - maps/combined_snapping_check.html")



# ============================================================================
# SNAP DAMS DATA
# ============================================================================
message("\n=== Snapping Dams Data ===")

# Snapping parameters
STRAHLER_SEQ <- c(4, 3, 2)
DISTANCE_THRESHOLD <- 150  # meters

# Load cleaned dams data URL
dams_url <- "https://nimbus.igb-berlin.de/index.php/s/tagEWdtxTpbRE9g/download/dams_all_clean.csv"

# Load original dams data
dams_original <- fread(dams_url)
message(sprintf("Original dams points: %d", nrow(dams_original)))

# Snap dams data
DISTANCE_THRESHOLD <- 150
dams_snap_result <- api_get_snapped_points_cascade(
  data = dams_original,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "id1",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
)

print(dams_snap_result)


# dams_snap_result <- fread("points_snapped/dams/dams_snapped_points.csv")
dams_snap_result <- dams_snap_result %>%
  left_join(dams_original, by = "id1") %>%
  select(-longitude, -latitude)

# Save
fwrite(dams_snap_result, "points_snapped/dams/dams_snapped_points.csv")
message(sprintf("Dams: Snapped %d points", nrow(dams_snap_result)))

# ============================================================================
# VISUALIZE DAMS SNAPPING
# ============================================================================

message("\n--- Creating Dams visualization ---")

# Identify which points were successfully snapped
dams_snapped_ids <- dams_snap_result$id1
dams_original$snapped <- dams_original$id1 %in% dams_snapped_ids

# Summary
n_snapped_dams <- sum(dams_original$snapped)
n_failed_dams <- sum(!dams_original$snapped)
message(sprintf("  Successfully snapped: %d", n_snapped_dams))
message(sprintf("  Failed to snap: %d", n_failed_dams))

# Color palette for sources
dams_source_colors <- colorFactor(
  palette = c("RAAY" = "blue", "AMBER" = "green"),
  domain = c("RAAY", "AMBER")
)

# Create map
dams_map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Original points that were successfully snapped (colored by source)
  addCircleMarkers(
    data = dams_original[dams_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = ~dams_source_colors(source),
    fillColor = ~dams_source_colors(source),
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Original (Snapped)",
    popup = ~paste0(
      "<b>Original Dam (Successfully Snapped)</b><br>",
      "Dam ID: ", id1, "<br>",
      "Source: ", source, "<br>",
      "Phase: ", phase, "<br>",
      "Status: ", status, "<br>",
      "Type: ", type, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5)
    )
  ) %>%

  # Original points that FAILED to snap (orange)
  addCircleMarkers(
    data = dams_original[!dams_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = "orange",
    fillColor = "orange",
    radius = 6,
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Original (Failed)",
    popup = ~paste0(
      "<b>⚠️ Original Dam (FAILED TO SNAP)</b><br>",
      "Dam ID: ", id1, "<br>",
      "Source: ", source, "<br>",
      "Phase: ", phase, "<br>",
      "Status: ", status, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5), "<br>",
      "<b style='color:red;'>This point was not successfully snapped</b>"
    )
  ) %>%

  # Snapped points (red)
  addCircleMarkers(
    data = dams_snap_result,
    lng = ~longitude_snapped,
    lat = ~latitude_snapped,
    color = "red",
    fillColor = "red",
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Snapped Points",
    popup = ~paste0(
      "<b>Snapped Dam</b><br>",
      "Dam ID: ", id1, "<br>",
      "Lon: ", round(longitude_snapped, 5), "<br>",
      "Lat: ", round(latitude_snapped, 5), "<br>",
      "Distance: ", round(distance_metres, 1), " m<br>",
      "Strahler: ", strahler
    )
  ) %>%

  # Add lines connecting original to snapped
  addPolylines(
    data = lapply(1:nrow(dams_snap_result), function(i) {
      st_linestring(matrix(c(
        dams_snap_result$longitude_original[i], dams_snap_result$latitude_original[i],
        dams_snap_result$longitude_snapped[i], dams_snap_result$latitude_snapped[i]
      ), ncol = 2, byrow = TRUE))
    }) %>% st_sfc(crs = 4326),
    color = "gray",
    weight = 1,
    opacity = 0.5,
    group = "Snap Lines"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend for sources
  addLegend(
    position = "bottomright",
    pal = dams_source_colors,
    values = dams_original$source,
    title = "Dam Source",
    opacity = 0.7
  )

# Save map locally
saveWidget(dams_map, "points_snapped/maps/dams_snapping_check.html")

# Save map to nimbus
save_to_nimbus(dams_map, "points_snapped/maps/dams_snapping_check.html")

# Print snapping statistics
message(sprintf("\nDams Snapping Statistics:"))
message(sprintf("  Original points: %d", nrow(dams_original)))
message(sprintf("  Successfully snapped: %d (%.1f%%)", n_snapped_dams, 100 * n_snapped_dams / nrow(dams_original)))
message(sprintf("  Failed to snap: %d (%.1f%%)", n_failed_dams, 100 * n_failed_dams / nrow(dams_original)))

if (n_snapped_dams > 0) {
  message(sprintf("  Mean snap distance: %.1f m", mean(dams_snap_result$distance_metres)))
  message(sprintf("  Median snap distance: %.1f m", median(dams_snap_result$distance_metres)))
  message(sprintf("  Max snap distance: %.1f m", max(dams_snap_result$distance_metres)))

  # Statistics by source
  message("\n  By source:")
  for (src in unique(dams_original$source)) {
    snapped_from_source <- sum(dams_snap_result$source == src, na.rm = TRUE)
    total_from_source <- sum(dams_original$source == src)
    message(sprintf("    %s: %d/%d snapped (%.1f%%)",
                    src, snapped_from_source, total_from_source,
                    100 * snapped_from_source / total_from_source))
  }

  # Statistics by status
  message("\n  By status:")
  for (stat in unique(dams_original$status)) {
    snapped_from_status <- sum(dams_snap_result$status == stat, na.rm = TRUE)
    total_from_status <- sum(dams_original$status == stat)
    message(sprintf("    %s: %d/%d snapped (%.1f%%)",
                    stat, snapped_from_status, total_from_status,
                    100 * snapped_from_status / total_from_status))
  }
}

# Save list of failed points
if (n_failed_dams > 0) {
  dams_failed <- dams_original[!dams_original$snapped, ]
  fwrite(dams_failed, "points_cleaned/dams/dams_failed_to_snap.csv")
  message(sprintf("\nFailed points saved to: points_cleaned/dams/dams_failed_to_snap.csv"))
}










#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01b_extract_subbasin.R
#
# Extract the Sarantaporos subbasin from the Vjosa/Aoos basin for
# focused connectivity and SDM analysis.
#
# Two outputs:
#   1. Greece-wide occurrences for Vjosa species (for SDM training)
#   2. Sarantaporos-only network, points, and polygon (for prediction
#      and connectivity analysis)
#
# Requires placeholder functions:
#   - api_get_upstream_catchment()
#   - api_get_upstream_stream_segments()
#
# Input:
#   - Snapped fish and dam points (Greece-wide)
#   - IUCN species checklist for Vjosa basin
#   - Vjosa basin stream network (from 01a)
#
# Output:
#   - spatial/sarantaporos/subbasin_polygon.gpkg
#   - spatial/sarantaporos/stream_network.gpkg
#   - points_snapped/sarantaporos/fish_vjosa_species_greece.csv
#     (all Greece occurrences of Vjosa species — for SDM training)
#   - points_snapped/sarantaporos/fish_sarantaporos.csv
#     (only occurrences within Sarantaporos — for connectivity)
#   - points_snapped/sarantaporos/dams_sarantaporos.csv
#     (only dams within Sarantaporos — for connectivity)
#
# LOCATION: workflows/04_spatial_network/01b_extract_subbasin.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(sf)
library(data.table)
library(dplyr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select

# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Sarantaporos outlet point (where it joins the Aoos/Vjosa mainstem)
# We obtained the coordinates on https://aqua.igb-berlin.de/upstream-dev/
OUTLET_LON <- 20.538704
OUTLET_LAT <- 40.113735

# Parent basin
VJOSA_BASIN_ID <- 1292502

# ============================================================
# STEP 1: Get Sarantaporos subbasin polygon and stream network
# ============================================================

message("\n=== Step 1: Extracting Sarantaporos subbasin ===")

dir.create("spatial/sarantaporos", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/sarantaporos", recursive = TRUE, showWarnings = FALSE)

# Get upstream catchment polygon from the outlet point
subbasin_polygon <- api_get_upstream_catchment(
  lon = OUTLET_LON,
  lat = OUTLET_LAT
)

st_write(subbasin_polygon, "spatial/sarantaporos/subbasin_polygon.gpkg")
save_to_nimbus(subbasin_polygon, "spatial/sarantaporos/subbasin_polygon.gpkg")
message("  Subbasin polygon saved")

subbasin_polygon <- st_read("spatial/sarantaporos/subbasin_polygon.gpkg")

# Get upstream stream segments from the outlet point
subbasin_streams <- api_get_stream_segments(
  lon = OUTLET_LON,
  lat = OUTLET_LAT,
  min_strahler = 2,
  upstream = TRUE
)

st_write(subbasin_streams, "spatial/sarantaporos/sarantaporos_stream_network.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_streams, "spatial/sarantaporos/sarantaporos_stream_network.gpkg")
message("  Stream network saved: ", nrow(subbasin_streams), " segments")


# Get subcatchment IDs within the subbasin
sarantaporos_subc_ids <- unique(subbasin_streams$subc_id)
message("  Subcatchments in Sarantaporos: ", length(sarantaporos_subc_ids))

# ============================================================
# STEP 2: Load IUCN species list for Vjosa basin
# ============================================================

message("\n=== Step 2: Loading Vjosa species checklist ===")

vjosa_species <- fread("range_maps/vjosa_species_checklist_iucn.csv") %>%
  filter(basin_id == VJOSA_BASIN_ID) %>%
  pull(species) %>%
  unique()

message("  Species expected in Vjosa (IUCN): ", length(vjosa_species))

# ============================================================
# STEP 3: Filter fish occurrences — two outputs
# ============================================================

message("\n=== Step 3: Filtering fish occurrences ===")

fish_snapped <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# 3a. ALL Greece occurrences for Vjosa species (SDM training)
fish_vjosa_species_greece <- fish_snapped %>%
  filter(species %in% vjosa_species)

fwrite(fish_vjosa_species_greece,
       "points_snapped/sarantaporos/fish_vjosa_species_greece.csv")

message("  Greece-wide occurrences for Vjosa species: ", nrow(fish_vjosa_species_greece))
message("  Species with occurrences: ",
        length(unique(fish_vjosa_species_greece$species)), " / ", length(vjosa_species))

# 3b. Only occurrences within Sarantaporos (connectivity analysis)
fish_sarantaporos <- fish_snapped %>%
  filter(subc_id %in% sarantaporos_subc_ids)

fwrite(fish_sarantaporos,
       "points_snapped/sarantaporos/fish_sarantaporos.csv")

message("  Occurrences within Sarantaporos: ", nrow(fish_sarantaporos))
message("  Species in Sarantaporos: ", length(unique(fish_sarantaporos$species)))

# ============================================================
# STEP 4: Filter dams to Sarantaporos
# ============================================================

message("\n=== Step 4: Filtering dams ===")

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")

dams_sarantaporos <- dams_snapped %>%
  filter(subc_id %in% sarantaporos_subc_ids) %>%
  filter(phase!="R")

fwrite(dams_sarantaporos,
       "points_snapped/sarantaporos/dams_sarantaporos.csv")

message("  Dams in Sarantaporos: ", nrow(dams_sarantaporos))
if (nrow(dams_sarantaporos) > 0) {
  message("  By phase:")
  print(table(dams_sarantaporos$phase))
}

# ============================================================
# STEP 5: Species coverage summary
# ============================================================

message("\n=== Step 5: Species coverage summary ===")

# Compare: IUCN expected vs observed in Sarantaporos vs observed in Greece
species_summary <- data.frame(
  species = vjosa_species
) %>%
  mutate(
    in_sarantaporos = species %in% unique(fish_sarantaporos$species),
    in_greece = species %in% unique(fish_vjosa_species_greece$species),
    n_occ_sarantaporos = sapply(species, function(sp)
      sum(fish_sarantaporos$species == sp)),
    n_occ_greece = sapply(species, function(sp)
      sum(fish_vjosa_species_greece$species == sp))
  )

species_summary %>% arrange(desc(n_occ_sarantaporos))

fwrite(species_summary, "points_snapped/sarantaporos/species_coverage_summary.csv")

cat("\nSpecies coverage:\n")
cat("  IUCN expected in Vjosa:          ", length(vjosa_species), "\n")
cat("  With occurrences in Greece:      ", sum(species_summary$in_greece), "\n")
cat("  With occurrences in Sarantaporos:", sum(species_summary$in_sarantaporos), "\n")
cat("  IUCN-only (no occurrences):      ",
    sum(!species_summary$in_greece), "\n")

# Species in Sarantaporos but not in IUCN list (unexpected)
unexpected <- unique(fish_sarantaporos$species)
unexpected <- unexpected[!unexpected %in% vjosa_species]
if (length(unexpected) > 0) {
  cat("\n  Unexpected species in Sarantaporos (not in IUCN Vjosa list):\n")
  cat("  ", paste(unexpected, collapse = ", "), "\n")
}


### Prune network
subbasin_streams_filtered <- extract_partial_stream_network(
  subbasin_streams,
  all_snapped$subc_id,
  strahler_retain_threshold = 4,
  upstream_buffer = 3      # number of upstream segments to include
)

st_write(all_streams_filtered, "spatial/stream_networks/partial_stream_network.gpkg")




# ============================================================
# SUMMARY
# ============================================================

message("\n=== Sarantaporos Subbasin Extraction Complete ===")
message("\nOutputs:")
message("  spatial/sarantaporos/subbasin_polygon.gpkg")
message("  spatial/sarantaporos/stream_network.gpkg")
message("  points_snapped/sarantaporos/fish_vjosa_species_greece.csv  (SDM training)")
message("  points_snapped/sarantaporos/fish_sarantaporos.csv          (connectivity)")
message("  points_snapped/sarantaporos/dams_sarantaporos.csv          (connectivity)")
message("  points_snapped/sarantaporos/species_coverage_summary.csv")
message("\nNext steps:")
message("  - SDM: use fish_vjosa_species_greece.csv for training,")
message("         predict on Sarantaporos subcatchments")
message("  - Connectivity: use fish_sarantaporos.csv + dams_sarantaporos.csv")
message("         with 02_generate_network_graph.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_extract_subbasin.R
#
# Extract the target subbasin for connectivity and SDM analysis.
# All analysis is scoped to the subbasin (Sarantaporos + Voidomatis),
# defined as all stream segments upstream of the outlet point.
# No full basin download is required.
#
# Workflow:
#   1. Load Greece-wide snapped fish + dams
#   2. Assign basin IDs via api_get_ids() (combined call)
#   3. Extract subbasin polygon and stream network from outlet point
#   4. Filter fish + dams to subbasin subcatchments
#   5. [VISUALISATION] Inspect full network + points, verify outlet
#   6. Prune stream network to reaches with observations + buffer
#   7. [VISUALISATION] Compare full vs pruned network
#   8. Load species checklist + species coverage summary
#   9. Filter fish to target species → SDM training output
#
# Input:
#   - points_snapped/fish/fish_all_species_snapped.csv   (Greece-wide)
#   - points_snapped/dams/dams_snapped_points.csv        (Greece-wide)
#   - range_maps/vjosa_species_checklist_iucn.csv
#
# Output:
#   - points_snapped/all_snapped_with_basins.csv
#   - spatial/subbasin/subbasin_polygon.gpkg
#   - spatial/subbasin/stream_network.gpkg
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - points_snapped/subbasin/subbasin_subc_ids_pruned.csv  (for predict table)
#   - points_snapped/subbasin/fish_subbasin.csv             (connectivity)
#   - points_snapped/subbasin/dams_subbasin.csv             (connectivity)
#   - points_snapped/subbasin/fish_vjosa_species_subbasin.csv (SDM training)
#   - points_snapped/subbasin/species_coverage_summary.csv
#
# LOCATION: workflows/04_spatial_network/01_extract_subbasin.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

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

# ============================================================
# PARAMETERS
# ============================================================

BASIN_ID <- 1292502

# Subbasin outlet point (confluence of Sarantaporos + Voidomatis → Aoos mainstem)
# All stream segments upstream of this point define the subbasin
# Coordinates obtained from https://aqua.igb-berlin.de/upstream-dev/
OUTLET_LON <- 20.5870613
OUTLET_LAT <- 40.0728991

# ============================================================
# STEP 1: Load Greece-wide snapped points
# ============================================================

message("\n=== Step 1: Loading snapped points ===")

fish_snapped <- fread("points_snapped/fish/fish_all_species_snapped.csv")
message("  Fish points loaded: ", nrow(fish_snapped))

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")
message("  Dam points loaded: ", nrow(dams_snapped))

# Harmonise column names
if (!"source" %in% names(dams_snapped)) dams_snapped$source <- "Dams"
if ("id1" %in% names(dams_snapped)) dams_snapped <- dams_snapped %>% rename(site_id = id1)

# Combine for single API call
common_cols <- intersect(names(fish_snapped), names(dams_snapped))
all_snapped <- rbind(
  fish_snapped[, ..common_cols],
  dams_snapped[, ..common_cols]
)

# Deduplicate to unique site locations before API call
# (fish file has multiple rows per site due to multiple species per location)
all_snapped <- all_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

message("  Unique site locations: ", nrow(all_snapped))

# Write out and reload via Nimbus URL
# (required for api_get_ids() which reads from a remote CSV)
fwrite(all_snapped, "points_snapped/all_points_snapped_unique_locations.csv")
all_snapped_csv <- "https://nimbus.igb-berlin.de/index.php/s/7Xx9xANQ4rDLkWk/download/all_points_snapped_unique_locations.csv"
all_snapped <- fread(all_snapped_csv)

# ============================================================
# STEP 2: Assign basin IDs
# ============================================================

message("\n=== Step 2: Assigning basin IDs ===")

basin_ids <- api_get_ids(
  points          = all_snapped,
  colname_lon     = "longitude_snapped",
  colname_lat     = "latitude_snapped",
  colname_site_id = "site_id",
  mode            = "local"
)

all_snapped_with_basins <- left_join(all_snapped, basin_ids, by = "site_id")

# Save full Greece-wide file (useful for reference / future reuse)
fwrite(all_snapped_with_basins, "points_snapped/all_snapped_with_basins.csv")
message("  Saved: points_snapped/all_snapped_with_basins.csv")

cat("  Points missing basin_id:", sum(is.na(all_snapped_with_basins$basin_id)), "\n")

# ============================================================
# STEP 3: Extract subbasin polygon and stream network
# ============================================================

message("\n=== Step 3: Extracting subbasin ===")

dir.create("spatial/subbasin", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/subbasin", recursive = TRUE, showWarnings = FALSE)

# Subbasin polygon upstream of outlet point
subbasin_polygon <- api_get_upstream_catchment(
  lon = OUTLET_LON,
  lat = OUTLET_LAT
)
st_write(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg")
message("  Subbasin polygon saved")

# Full stream network upstream of outlet point
# geometry_only = FALSE ensures subc_id attributes are returned
# Captures both Sarantaporos and Voidomatis tributaries
subbasin_streams <- api_get_stream_segments(
  lon           = OUTLET_LON,
  lat           = OUTLET_LAT,
  upstream      = TRUE,
  geometry_only = FALSE,
  min_strahler  = 2
)

st_write(subbasin_streams, "spatial/subbasin/stream_network.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_streams, "spatial/subbasin/stream_network.gpkg")
message("  Subbasin stream segments: ", nrow(subbasin_streams))

# All subcatchment IDs within subbasin — used to filter points
subbasin_subc_ids <- unique(subbasin_streams$subc_id)
message("  Subcatchments in subbasin: ", length(subbasin_subc_ids))


# ============================================================
# STEP 3b: Download full basin stream network
#
# The full Vjosa basin network is used as the SDM training extent.
# Pseudoabsences will be sampled from basin subcatchments, providing
# a broader environmental background than the subbasin alone.
# The network is pruned to reaches where target species were recorded.
# ============================================================

message("\n=== Step 3b: Downloading full basin stream network ===")

dir.create("spatial/basin", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/basin", recursive = TRUE, showWarnings = FALSE)

basin_streams <- api_get_stream_segments(
  basin_id      = BASIN_ID,
  geometry_only = FALSE,
  min_strahler  = 2
)
basin_streams$basin_id <- BASIN_ID

st_write(basin_streams, "spatial/basin/stream_network.gpkg", delete_dsn = TRUE)
save_to_nimbus(basin_streams, "spatial/basin/stream_network.gpkg")
message("  Basin stream segments: ", nrow(basin_streams))

# All basin subcatchment IDs
basin_subc_ids <- unique(basin_streams$subc_id)
message("  Subcatchments in basin: ", length(basin_subc_ids))

# Fish points within the full basin (target species only)
# Used to drive pruning — retains reaches where SDM species were recorded
fish_basin_species <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(subc_id %in% basin_subc_ids)

# Load species checklist early to filter to target species
# (full checklist load repeated in Step 8 for coverage summary)
vjosa_species_for_pruning <- fread("range_maps/vjosa_species_checklist_iucn.csv") %>%
  filter(basin_id == BASIN_ID) %>%
  pull(species) %>%
  unique()

fish_basin_sdm <- fish_basin_species %>%
  filter(species %in% vjosa_species_for_pruning)

message("  Fish points for basin pruning: ", nrow(fish_basin_sdm),
        " (", n_distinct(fish_basin_sdm$species), " species)")

# Prune basin network to reaches with target species observations
basin_streams_pruned <- extract_partial_stream_network(
  stream                    = basin_streams,
  snapped_subcs             = fish_basin_sdm$subc_id,
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)


st_write(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg")
message("  Pruned basin stream segments: ", nrow(basin_streams_pruned),
        " (from ", nrow(basin_streams), " full basin segments)")

# Save basin pruned subcatchment IDs
# Used for: environmental variable extraction + pseudoabsence sampling
basin_subc_ids_pruned <- unique(basin_streams_pruned$subc_id)
message("  Basin subcatchments after pruning: ", length(basin_subc_ids_pruned),
        " (from ", length(basin_subc_ids), " full basin subcatchments)")

fwrite(
  data.table(subc_id = basin_subc_ids_pruned),
  "points_snapped/basin/basin_subc_ids_pruned.csv"
)



# ============================================================
# STEP 4: Filter fish + dams to subbasin
# ============================================================

message("\n=== Step 4: Filtering points to subbasin ===")

fish_subbasin <- all_snapped_with_basins %>%
  filter(source %in% c("HCMR", "GBIF"), subc_id %in% subbasin_subc_ids)

dams_subbasin <- all_snapped_with_basins %>%
  filter(source %in% c("RAAY", "AMBER"), subc_id %in% subbasin_subc_ids)

message("  Fish points in subbasin: ", nrow(fish_subbasin))
message("  Dams in subbasin: ", nrow(dams_subbasin))
if (nrow(dams_subbasin) > 0) {
  message("  By phase:")
  print(table(dams_subbasin$phase))
}

fwrite(fish_subbasin, "points_snapped/subbasin/fish_subbasin.csv")
fwrite(dams_subbasin, "points_snapped/subbasin/dams_subbasin.csv")

# ============================================================
# STEP 5: Visualise full network + points, verify outlet location
#
# Inspect all snapped fish and dam occurrences within the subbasin
# on the full stream network. The outlet point is marked — verify
# that all points fall within the subbasin boundary before pruning.
# ============================================================

message("\n=== Step 5: Visualising subbasin points ===")

fish_subbasin_sf <- fish_subbasin %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_subbasin_sf <- dams_subbasin %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

map_full <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  # Subbasin boundary polygon
  addPolygons(
    data        = subbasin_polygon,
    color       = "#525252",
    weight      = 2,
    fillOpacity = 0.05,
    group       = "Subbasin boundary"
  ) %>%

  # Full stream network
  addPolylines(
    data    = subbasin_streams,
    color   = "#6baed6",
    weight  = 1.5,
    opacity = 0.8,
    group   = "Stream network"
  ) %>%

  # Fish points — blue circles
  addCircleMarkers(
    data        = fish_subbasin_sf,
    radius      = 4,
    color       = "#2166ac",
    fillColor   = "#2166ac",
    fillOpacity = 0.7,
    stroke      = FALSE,
    label       = ~paste0(site_id, " | ", source),
    group       = "Fish"
  ) %>%

  # Dam points — red circles
  addCircleMarkers(
    data        = dams_subbasin_sf,
    radius      = 5,
    color       = "#d73027",
    fillColor   = "#d73027",
    fillOpacity = 0.8,
    stroke      = FALSE,
    label       = ~site_id,
    group       = "Dams"
  ) %>%

  # Outlet point — labelled marker
  addMarkers(
    lng   = OUTLET_LON,
    lat   = OUTLET_LAT,
    label = "Subbasin outlet — all points upstream of this location"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Stream network", "Fish", "Dams"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#2166ac", "#d73027", "#6baed6"),
    labels   = c("Fish occurrences", "Dams", "Stream network"),
    title    = "Subbasin — full network"
  )

print(map_full)
# Verify that all points fall within the subbasin boundary before proceeding

# ============================================================
# STEP 6: Prune stream network
#
# Retain only reaches where fish or dams were recorded, plus a small
# upstream buffer. This removes uninformative headwater segments and
# reduces the size of the predict table.
# The pruned network subcatchment IDs are saved for use in the
# predict table construction script.
# ============================================================

# Minimum Strahler order for network pruning
MIN_STRAHLER <- 4
UPSTREAM_BUFFER <- 3


message("\n=== Step 6: Pruning stream network ===")

fish_subbasin <- fread("points_snapped/subbasin/fish_subbasin.csv")
dams_subbasin <- fread("points_snapped/subbasin/dams_subbasin.csv")


subbasin_streams_pruned <- extract_partial_stream_network(
  stream                    = subbasin_streams,
  snapped_subcs             = c(fish_subbasin$subc_id, dams_subbasin$subc_id),
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)


st_write(subbasin_streams_pruned, "spatial/subbasin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_streams_pruned, "spatial/subbasin/stream_network_pruned.gpkg")
message("  Pruned stream segments: ", nrow(subbasin_streams_pruned),
        " (from ", nrow(subbasin_streams), " full network segments)")

# Save pruned subcatchment IDs — used by the predict table script
subbasin_subc_ids_pruned <- unique(subbasin_streams_pruned$subc_id)
message("  Subcatchments after pruning: ", length(subbasin_subc_ids_pruned),
        " (from ", length(subbasin_subc_ids), " full subbasin subcatchments)")

fwrite(
  data.table(subc_id = subbasin_subc_ids_pruned),
  "points_snapped/subbasin/subbasin_subc_ids_pruned.csv"
)

# ============================================================
# STEP 7: Visualise full vs pruned network
#
# Compare the full upstream network with the pruned network to verify
# that pruning retains all ecologically relevant reaches while removing
# uninformative headwater segments.
# ============================================================

message("\n=== Step 7: Comparing full vs pruned stream network ===")

subbasin_polygon <- st_read("spatial/subbasin/subbasin_polygon.gpkg")

map_pruning <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  # Subbasin boundary
  addPolygons(
    data        = subbasin_polygon,
    color       = "#525252",
    weight      = 2,
    fillOpacity = 0.05,
    group       = "Subbasin boundary"
  ) %>%

  # Full network — light grey
  addPolylines(
    data    = subbasin_streams,
    color   = "#bdbdbd",
    weight  = 1,
    opacity = 0.8,
    group   = "Full network"
  ) %>%

  # Pruned network — blue
  addPolylines(
    data    = subbasin_streams_pruned,
    color   = "#2166ac",
    weight  = 2,
    opacity = 0.9,
    group   = "Pruned network"
  ) %>%

  # Fish points — to verify all occurrences fall on pruned network
  addCircleMarkers(
    data        = fish_subbasin_sf,
    radius      = 4,
    color       = "#e6550d",
    fillColor   = "#e6550d",
    fillOpacity = 0.7,
    stroke      = FALSE,
    label       = ~paste0(site_id, " | ", source),
    group       = "Fish"
  ) %>%

  # Outlet point
  addMarkers(
    lng   = OUTLET_LON,
    lat   = OUTLET_LAT,
    label = "Subbasin outlet"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Full network", "Pruned network", "Fish"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#bdbdbd", "#2166ac", "#e6550d"),
    labels   = c("Full network", "Pruned network", "Fish occurrences"),
    title    = "Stream network — full vs pruned"
  )

print(map_pruning)
# Verify that all fish occurrence points lie on the pruned network —
# if any point falls only on the grey (full) network, pruning has
# incorrectly removed a reach with observations

# ============================================================
# STEP 8: Species checklist and coverage summary
# ============================================================

message("\n=== Step 8: Species checklist and coverage summary ===")

vjosa_species <- fread("range_maps/vjosa_species_checklist_iucn.csv") %>%
  filter(basin_id == BASIN_ID) %>%
  pull(species) %>%
  unique()

message("  Species expected in basin (IUCN): ", length(vjosa_species))

# Join species info back from full fish file for coverage summary
# (fish_subbasin from all_snapped_with_basins has no species column)
fish_subbasin_species <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(subc_id %in% subbasin_subc_ids)

species_summary <- data.frame(species = vjosa_species) %>%
  mutate(
    in_subbasin    = species %in% unique(fish_subbasin_species$species),
    n_occ_subbasin = sapply(species, function(sp)
      sum(fish_subbasin_species$species == sp))
  ) %>%
  arrange(desc(n_occ_subbasin))

fwrite(species_summary, "points_snapped/subbasin/species_coverage_summary.csv")

cat("\nSpecies coverage:\n")
cat("  IUCN expected in basin:        ", length(vjosa_species), "\n")
cat("  With occurrences in subbasin:  ", sum(species_summary$in_subbasin), "\n")
cat("  IUCN-only (no occurrences):    ", sum(!species_summary$in_subbasin), "\n")

unexpected <- unique(fish_subbasin_species$species)
unexpected <- unexpected[!unexpected %in% vjosa_species]
if (length(unexpected) > 0) {
  cat("\n  Unexpected species in subbasin (not in IUCN list):\n")
  cat("  ", paste(unexpected, collapse = ", "), "\n")
}

# ============================================================
# STEP 9: Filter fish to target species → SDM training output
# ============================================================

message("\n=== Step 9: Filtering to target species for SDM ===")

# Retain only IUCN-listed Vjosa species from the full species file
# Note: includes both HCMR and GBIF records within the subbasin
# GBIF records outside the subbasin are excluded — endemic species
# have no reliable records beyond the Aoos/Vjosa basin
fish_sdm <- fish_subbasin_species %>%
  filter(species %in% vjosa_species)

fwrite(fish_sdm, "points_snapped/subbasin/fish_vjosa_species_subbasin.csv")
message("  SDM training occurrences: ", nrow(fish_sdm),
        " (", n_distinct(fish_sdm$species), " species)")

# ============================================================
# SUMMARY
# ============================================================

message("\n=== Subbasin Extraction Complete ===")
message("\nOutputs:")
message("  points_snapped/all_snapped_with_basins.csv")
message("  spatial/subbasin/subbasin_polygon.gpkg")
message("  spatial/subbasin/stream_network.gpkg")
message("  spatial/subbasin/stream_network_pruned.gpkg")
message("  points_snapped/subbasin/subbasin_subc_ids_pruned.csv       (predict table)")
message("  points_snapped/subbasin/fish_vjosa_species_subbasin.csv    (SDM training)")
message("  points_snapped/subbasin/fish_subbasin.csv                  (connectivity)")
message("  points_snapped/subbasin/dams_subbasin.csv                  (connectivity)")
message("  points_snapped/subbasin/species_coverage_summary.csv")
message("\nNext steps:")
message("  SDM:          use fish_vjosa_species_subbasin.csv for training,")
message("                predict on subbasin_subc_ids_pruned subcatchments")
message("  Connectivity: use fish_subbasin.csv + dams_subbasin.csv")
message("                with 02_generate_network_graph.R")
message("  spatial/basin/stream_network.gpkg")
message("  spatial/basin/stream_network_pruned.gpkg")
message("  points_snapped/basin/basin_subc_ids_pruned.csv          (SDM pseudoabsences)")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_generate_network_graph.R
# Build igraph river network from H90M subcatchments + barriers
# TWO SCENARIOS: current (operational dams) vs future (+ planned dams)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(igraph)
library(riverconn)
library(dplyr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# INPUT FILES
# ============================================================
subcatchments <- st_read("spatial/stream_networks/partial_stream_network.gpkg") %>%
  filter(basin_id == 1292502) # filter for Vjosa basin
barriers <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% subcatchments$subc_id) # keep dams in Vjosa

# ============================================================
# PARAMETERS
# ============================================================
PASS_SHP <- 0.2  # passability of small hydropower plants

# ============================================================
# BARRIER SCENARIOS
# ============================================================

# Current: only operational dams (OL + amber = existing)
barrier_counts_current <- barriers %>%
  filter(phase %in% c("OL", "amber")) %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

# Future: operational + all planned (everything except rejected)
barrier_counts_future <- barriers %>%
  filter(phase != "R") %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

message("Current scenario: ", sum(barrier_counts_current$n_shp), " dams in ",
        nrow(barrier_counts_current), " subcatchments")
message("Future scenario:  ", sum(barrier_counts_future$n_shp), " dams in ",
        nrow(barrier_counts_future), " subcatchments")

# ============================================================
# FUNCTION: Build river graph from subcatchments + barrier counts
# ============================================================
build_river_graph <- function(subcatchments_raw, barrier_counts, pass_shp) {

  # Step 1: Join barrier counts with subcatchments
  sc <- subcatchments_raw %>%
    left_join(barrier_counts, by = "subc_id") %>%
    mutate(n_shp = ifelse(is.na(n_shp), 0, n_shp))

  # Step 2: Compute passability
  sc <- sc %>%
    mutate(pass_tot = pass_shp ^ n_shp)

  # Step 3: Deduplicate
  sc <- sc %>%
    distinct(subc_id, .keep_all = TRUE)

  # Step 4: Build edges
  edges_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, target) %>%
    rename(from = subc_id, to = target) %>%
    mutate(from = as.character(from),
           to   = as.character(to))

  # Step 5: Build vertices
  vertices_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, length, pass_tot, basin_id) %>%
    rename(name = subc_id, length_reach = length) %>%
    mutate(name = as.character(name),
           length_reach = ifelse(is.na(length_reach), 0, length_reach))

  # Step 6: Create igraph (first pass)
  rg <- igraph::graph_from_data_frame(edges_df)

  rg_v_df <- igraph::as_data_frame(rg, "vertices") %>%
    left_join(vertices_df, by = "name") %>%
    mutate(length_reach = ifelse(name == "0" | is.na(length_reach), 1, length_reach),
           pass_tot     = ifelse(name == "0" | is.na(pass_tot),     1, pass_tot),
           basin_id     = ifelse(name == "0" | is.na(basin_id),     NA, basin_id))

  rg_tmp <- igraph::graph_from_data_frame(edges_df, v = rg_v_df)

  # Step 7: Transfer passability from nodes -> edges
  graph_v_df <- igraph::as_data_frame(rg_tmp, "vertices")

  graph_e_df <- igraph::as_data_frame(rg_tmp, "edges") %>%
    left_join(graph_v_df %>% select(name, pass_tot) %>% rename(from = name),
              by = "from") %>%
    rename(d_att_from = pass_tot) %>%
    left_join(graph_v_df %>% rename(to = name) %>% select(to, pass_tot),
              by = "to") %>%
    rename(d_att_to = pass_tot) %>%
    mutate(pass_tot = (d_att_from ^ 0.5) * (d_att_to ^ 0.5),
           pass_u   = sqrt(pass_tot),
           pass_d   = sqrt(pass_tot)) %>%
    select(from, to, pass_u, pass_d)

  # Step 8: Final graph
  rg_final <- igraph::graph_from_data_frame(d = graph_e_df, vertices = graph_v_df)

  V(rg_final)$weight <- 1
  E(rg_final)$pass_u <- ifelse(is.na(E(rg_final)$pass_u), 1, E(rg_final)$pass_u)
  E(rg_final)$pass_d <- ifelse(is.na(E(rg_final)$pass_d), 1, E(rg_final)$pass_d)

  return(rg_final)
}

# ============================================================
# BUILD BOTH GRAPHS
# ============================================================

message("\nBuilding current scenario graph...")
river_graph_current <- build_river_graph(subcatchments, barrier_counts_current, PASS_SHP)
message("Current: ", vcount(river_graph_current), " nodes, ", ecount(river_graph_current), " edges")

message("\nBuilding future scenario graph...")
river_graph_future <- build_river_graph(subcatchments, barrier_counts_future, PASS_SHP)
message("Future:  ", vcount(river_graph_future), " nodes, ", ecount(river_graph_future), " edges")

# ============================================================
# SAVE
# ============================================================
saveRDS(river_graph_current, "spatial/stream_networks/river_graph_current.RDS")
saveRDS(river_graph_future,  "spatial/stream_networks/river_graph_future.RDS")

message("\nBoth graphs saved!")

# ============================================================
# DIAGNOSTICS
# ============================================================
message("\n--- Current scenario ---")
message("Passability summary (edges):")
print(summary(E(river_graph_current)$pass_u))

message("\n--- Future scenario ---")
message("Passability summary (edges):")
print(summary(E(river_graph_future)$pass_u))

# Compare: how many edges have lower passability in future?
pass_current <- E(river_graph_current)$pass_u
pass_future  <- E(river_graph_future)$pass_u
n_affected <- sum(pass_future < pass_current)
message("\nEdges with reduced passability in future: ", n_affected,
        " out of ", length(pass_current))
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_spatial_network/03_overlay_with_basin_names.R
#
# Create lookup table linking Hydrography90m basin IDs to
# Greek river basin names from the EGY (Water District) shapefile.
#
# Method: overlay snapped occurrence points with Greek named basin
# polygons, then assign the most frequent Greek basin name to each
# H90M basin_id.
#
# Input:
#   - H90M basin polygons (basin_polygons.gpkg)
#   - Greek named basins (egy_rb_new_2014.shp)
#   - Snapped occurrence points (all_snapped_with_basins_from_sp_list.csv)
#
# Output:
#   - Basin name lookup table (basin_name_lookup.csv)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================

# H90M basin polygons
basin_polygons <- read_sf("spatial/stream_networks/basin_polygons.gpkg")

# Greek named basins (EGY river basin districts)
greek_basins <- st_read("spatial/egy_rb_new_2014/egy_rb_new_2014.shp") %>%
  st_transform(st_crs(basin_polygons))

# Snapped occurrence points (with basin_id already assigned)
occurrences <- fread("points_snapped/all_snapped_with_basins_from_sp_list.csv")

# ============================================================
# OVERLAY: Point-in-polygon to get Greek basin name per point
# ============================================================

occ_sf <- st_as_sf(occurrences,
                   coords = c("longitude_snapped", "latitude_snapped"),
                   crs = 4326) %>%
  st_transform(st_crs(greek_basins))

occ_sf$greek_basin_name <- greek_basins$name[
  as.integer(st_intersects(occ_sf, greek_basins))
]

# ============================================================
# BUILD LOOKUP TABLE
# Link H90M basin_id to the most frequent Greek basin name
# ============================================================

basin_lookup <- occ_sf %>%
  st_drop_geometry() %>%
  select(basin_id, greek_basin_name) %>%
  filter(!is.na(basin_id), !is.na(greek_basin_name)) %>%
  group_by(basin_id) %>%
  summarize(
    basin_name = names(sort(table(greek_basin_name), decreasing = TRUE))[1],
    n_points = n(),
    .groups = "drop"
  ) %>%
  arrange(basin_name)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_spatial_network/03_overlay_with_basin_names.R
#
# Create lookup table linking Hydrography90m basin IDs to
# Greek river basin names from the EGY (Water District) shapefile.
#
# Method: overlay snapped occurrence points with Greek named basin
# polygons, then assign the most frequent Greek basin name to each
# H90M basin_id.
#
# Input:
#   - H90M basin polygons (basin_polygons.gpkg)
#   - Greek named basins (egy_rb_new_2014.shp)
#   - Snapped occurrence points (all_snapped_with_basins_from_sp_list.csv)
#
# Output:
#   - Basin name lookup table (basin_name_lookup.csv)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================

# H90M basin polygons
basin_polygons <- read_sf("spatial/stream_networks/basin_polygons.gpkg")

# Greek named basins (EGY river basin districts)
greek_basins <- st_read("spatial/egy_rb_new_2014/egy_rb_new_2014.shp") %>%
  st_transform(st_crs(basin_polygons))

# Snapped occurrence points (with basin_id already assigned)
occurrences <- fread("points_snapped/all_snapped_with_basins_from_sp_list.csv")

# ============================================================
# OVERLAY: Point-in-polygon to get Greek basin name per point
# ============================================================

occ_sf <- st_as_sf(occurrences,
                   coords = c("longitude_snapped", "latitude_snapped"),
                   crs = 4326) %>%
  st_transform(st_crs(greek_basins))

occ_sf$greek_basin_name <- greek_basins$name[
  as.integer(st_intersects(occ_sf, greek_basins))
]

# ============================================================
# BUILD LOOKUP TABLE
# Link H90M basin_id to the most frequent Greek basin name
# ============================================================

# picks the most frequent Greek basin name among all points in that H90M basin.
# This is more robust for basins near boundaries where some points might fall in
# a neighboring Greek basin. With n_points we can see how many points supported
# each match.
basin_lookup <- occ_sf %>%
  st_drop_geometry() %>%
  select(basin_id, greek_basin_name) %>%
  filter(!is.na(basin_id), !is.na(greek_basin_name)) %>%
  group_by(basin_id) %>%
  summarize(
    basin_name = names(sort(table(greek_basin_name), decreasing = TRUE))[1],
    n_points = n(),
    .groups = "drop"
  ) %>%
  arrange(basin_name)

# ============================================================
# SAVE
# ============================================================

fwrite(basin_lookup, "spatial/basin_name_lookup.csv")

message("Basin name lookup saved: ", nrow(basin_lookup), " basins matched")
cat("\n")
print(basin_lookup)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_iucn_range_map_overlay.R
#
# Crop IUCN freshwater fish range maps to the study area and overlay
# with Hydrography90m basin polygons to determine which species are
# expected in which basins based on expert range maps.
#
# Input:
#   - Basin polygons (basin_polygons.gpkg)
#   - IUCN freshwater fish range maps (FW_FISH_PART1-3.shp)
#
# Output:
#   - Cropped range maps per IUCN file (range_maps/fish_greece_*.gpkg)
#   - Basin × species intersection table with % basin coverage
#     (range_maps/iucn_basins_intersect.csv)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)
library(hydrographr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
MIN_BASIN_COVERAGE <- 5  # minimum % of basin covered by range map to include

# ============================================================
# READ INPUTS
# ============================================================

basin_polygons <- st_read("spatial/stream_networks/basin_polygons.gpkg")
bbox <- as.numeric(st_bbox(basin_polygons))

# IUCN freshwater fish range maps (global, split into 3 parts)
fish_files <- list.files("range_maps/FW_FISH", pattern = "\\.shp$", full.names = TRUE)

message("Basin polygons: ", nrow(basin_polygons), " basins")
message("IUCN files: ", length(fish_files))

# ============================================================
# STEP 1: Crop IUCN range maps to study area
# ============================================================

out_dir <- "range_maps"

for (f in fish_files) {
  out_name <- paste0("fish_greece_",
                     tools::file_path_sans_ext(basename(f)), ".gpkg")
  message("Cropping: ", basename(f))
  crop_vector_to_extent(
    vector_layer  = f,
    bounding_box  = bbox,
    out_dir       = out_dir,
    file_name     = out_name,
    read          = FALSE,
    quiet         = FALSE
  )
}

# ============================================================
# STEP 2: Read and merge cropped files
# ============================================================

cropped_files <- list.files(out_dir, pattern = "fish_greece_.*\\.gpkg$",
                            full.names = TRUE)
fish_greece <- bind_rows(lapply(cropped_files, st_read, quiet = TRUE))

message("Total IUCN polygons in study area: ", nrow(fish_greece))
message("Unique species: ", length(unique(fish_greece$sci_name)))

# ============================================================
# STEP 3: Transform to equal-area projection for area calculations
# ============================================================

fish_eq <- fish_greece %>%
  st_transform(6933) %>%
  st_make_valid()

basin_eq <- basin_polygons %>%
  st_transform(6933) %>%
  st_make_valid()

basin_eq$basin_area <- st_area(basin_eq)

# ============================================================
# STEP 4: Overlay range maps with basins per species
# ============================================================

species_list <- unique(fish_eq$sci_name)
results <- tibble(basin_id = integer(),
                  species = character(),
                  perc_basin_covered = numeric())

for (sp in species_list) {
  message("Processing: ", sp)

  sp_poly <- fish_eq %>% filter(sci_name == sp)
  if (nrow(sp_poly) == 0) next

  # Intersect species range with basins
  ov <- try(st_intersection(basin_eq, sp_poly), silent = TRUE)
  if (inherits(ov, "try-error") || nrow(ov) == 0) next

  # Compute overlap area and percentage of basin covered
  ov$overlap_area <- st_area(ov)

  df <- ov %>%
    st_drop_geometry() %>%
    left_join(basin_eq %>%
                st_drop_geometry() %>%
                select(basin_id, basin_area),
              by = "basin_id") %>%
    mutate(
      perc_basin_covered = 100 * as.numeric(overlap_area) /
        as.numeric(basin_area.y),
      species = sp
    ) %>%
    filter(perc_basin_covered >= MIN_BASIN_COVERAGE) %>%
    select(basin_id, species, perc_basin_covered)

  results <- bind_rows(results, df)
}

# ============================================================
# SAVE
# ============================================================

fwrite(results, "range_maps/iucn_basins_intersect.csv")
save_to_nimbus(results, filename = paste0(NIMBUS_DIR, "/range_maps/iucn_basins_intersect.csv"))

message("\nDone! ", nrow(results), " species x basin combinations")
message("Species with range overlap: ", length(unique(results$species)))
message("Basins with species: ", length(unique(results$basin_id)))


## Filter species occurring in Vjosa
results <- fread("range_maps/iucn_basins_intersect.csv")  %>%
  mutate(species = gsub(" ", "_", species))

species_subset <- results %>%
  filter(basin_id == 1292502) %>%
  pull(species)

results_subset <- results %>%
  filter(species %in% species_subset)
fwrite(results_subset, "range_maps/vjosa_species_checklist_iucn.csv")


sp_snapped_subset <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(species %in% species_subset)

fwrite(sp_snapped_subset, "points_snapped/fish/occurr_for_sdm.csv")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_pci_visualizations.R
# Visualize PCI results: current vs future dam scenarios
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(data.table)
library(ggrepel)
library(leaflet)
library(sf)
library(htmlwidgets)
library(rnaturalearth)


# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================

fi_summary <- read.table("connectivity/pci/fi_summary_subhabitats.txt", header = TRUE)

basin_lookup <- fread("spatial/basin_name_lookup.csv") %>%
  mutate(basin_id = as.integer(basin_id))

fi_summary <- fi_summary %>%
  left_join(basin_lookup, by = "basin_id")

fish_dis_class <- fread("traits/fish_dis_class.txt")

# Join traits for coloring/faceting
fi_plot <- fi_summary %>%
  left_join(fish_dis_class %>%
              rename(species = species_name) %>%
              select(species, Migration_label, Caudal_fin_label,
                     dispersal_prob, max_TL),
            by = "species")


basin_polygons <- read_sf("spatial/stream_networks/basin_polygons.gpkg")

# ============================================================
# SCATTERPLOT: PCI current vs PCI future
# ============================================================

# Label only the most impacted points (top FI)
fi_plot <- fi_plot %>%
  mutate(label = ifelse(FI > 25,
                        paste0(gsub("_", " ", species), "\n(FI=", round(FI, 0), "%)"),
                        NA))

p1 <- ggplot(fi_plot, aes(x = PCI_current, y = PCI_future)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  geom_point(aes(color = Migration_label), size = 2, alpha = 0.6) +
  geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 15,
                  segment.color = "grey60", segment.size = 0.3,
                  na.rm = TRUE) +
  scale_color_manual(
    values = c("Non-migratory" = "#2196F3",
               "Potamodromous" = "#FF9800",
               "Long_distance" = "#F44336")
  ) +
  labs(x = "PCI (current dams)",
       y = "PCI (current + planned dams)",
       color = "Migration type") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "right")

p1
ggsave("connectivity/pci/pci_current_vs_future.png",
       p1, width = 10, height = 8, dpi = 300)

cat("Plot saved: connectivity/pci/pci_current_vs_future.png\n")




# ============================================================
# TOP IMPACTED SPECIES × BASIN COMBINATIONS
# ============================================================

# Add readable basin names if available, otherwise use basin_id
# You can replace this with actual river names if you have a lookup table
fi_plot_top <- fi_summary %>%
  filter(FI > 0) %>%
  left_join(fish_dis_class %>%
              select(species, Migration_label),
            by = "species") %>%
  mutate(
    species_label = gsub("_", " ", species),
    # Combine species + basin for unique labels
    combo_label =paste0(species_label, " — ", basin_name)
  ) %>%
  arrange(desc(FI)) %>%
  head(25)


# Reorder factor by FI for plotting
fi_plot_top <- fi_plot_top %>%
  mutate(combo_label = fct_reorder(combo_label, FI))

p_top <- ggplot(fi_plot_top, aes(x = FI, y = combo_label, fill = Migration_label)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(
    values = c("Non-migratory" = "#2196F3",
               "Potamodromous" = "#FF9800",
               "Long_distance" = "#F44336")
  ) +
  labs(x = "Fragmentation Index (%)",
       y = NULL,
       fill = "Migration type",
       title = "Top 25 most impacted species × basin combinations",
       subtitle = "% connectivity loss from planned dams") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic", size = 9))

p_top

ggsave("connectivity/pci/top_impacted_species.png",
       p_top, width = 10, height = 8, dpi = 300)

cat("Plot saved: connectivity/pci/top_impacted_species.png\n")


# ============================================================
# LEAFLET MAP: Mean FI by basin
# ============================================================

# Aggregate FI by basin
fi_by_basin <- fi_summary %>%
  group_by(basin_name, basin_id) %>%
  summarize(
    mean_FI = mean(FI, na.rm = TRUE),
    n_species = n(),
    n_impacted = sum(FI > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Join with spatial data and transform to WGS84
basins_plot <- basin_polygons %>%
  left_join(fi_by_basin, by = "basin_id") %>%
  st_transform(4326)

# Color palette
pal_fill <- colorNumeric("YlOrRd", domain = basins_plot$mean_FI, na.color = "grey90")

# Build map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  addPolygons(
    data = basins_plot,
    fillColor = ~pal_fill(mean_FI),
    fillOpacity = 0.7,
    color = "grey40",
    weight = 0.5,
    popup = ~paste0(
      "<b>Basin: </b>", basin_name, "<br>",
      "<b>Mean FI: </b>", round(mean_FI, 1), "%<br>",
      "<b>Species assessed: </b>", n_species, "<br>",
      "<b>Species impacted: </b>", n_impacted
    )
  ) %>%

  addLegend(
    position = "bottomright",
    pal = pal_fill,
    values = basins_plot$mean_FI,
    title = "Mean FI (%)",
    na.label = "No data"
  ) %>%

  addLayersControl(
    baseGroups = c("Light", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

m
saveWidget(m, "connectivity/pci/fi_map_by_basin.html", selfcontained = TRUE)
cat("Map saved: connectivity/pci/fi_map_by_basin.html\n")



#################
# same map but with ggplot
# ============================================================

# STATIC MAP: Mean FI by basin (ggplot)

# ============================================================



# Aggregate FI by basin

fi_by_basin <- fi_summary %>%
  group_by(basin_name, basin_id) %>%
  summarize(
    mean_FI = mean(FI, na.rm = TRUE),
    n_species = n(),
    n_impacted = sum(FI > 0, na.rm = TRUE),
    .groups = "drop"

  )

# Join with spatial data

basins_plot <- basin_polygons %>%
  left_join(fi_by_basin, by = "basin_id") %>%
  st_transform(4326)


# Get country boundaries
greece <- ne_countries(scale = 50, country = "Greece", returnclass = "sf")
neighbors <- ne_countries(scale = 50,
                          country = c("Turkey", "Bulgaria", "North Macedonia",
                                      "Albania", "Italy"),
                          returnclass = "sf")

# Get bounding box of your basins for map limits
bbox <- st_bbox(basins_plot)

p_map <- ggplot() +
  geom_sf(data = neighbors, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = greece, fill = "grey85", color = "grey50", linewidth = 0.3) +
  geom_sf(data = basins_plot, aes(fill = mean_FI), color = "grey40", linewidth = 0.2) +
  scale_fill_gradient(low = "#FFF3E0", high = "#BF360C", name = "Mean FI (%)",
                      na.value = "grey90") +
  coord_sf(xlim = c(bbox["xmin"] - 0.5, bbox["xmax"] + 0.5),
           ylim = c(bbox["ymin"] - 0.5, bbox["ymax"] + 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 8))
p_map

ggsave("connectivity/pci/fi_map_by_basin_static.png",
       p_map, width = 10, height = 8, dpi = 300)

#######################



# ============================================================
# BAR CHART: Species affected vs unaffected per basin
# ============================================================
# Aggregate by basin NAME (merge multiple basin_ids)
fi_by_basin <- fi_summary %>%
  group_by(basin_name) %>%
  summarize(
    n_impacted = sum(FI > 0, na.rm = TRUE),
    n_unaffected = sum(FI == 0, na.rm = TRUE),
    n_total = n(),
    pct_impacted = round(100 * sum(FI > 0, na.rm = TRUE) / n(), 0),
    .groups = "drop"
  ) %>%
  filter(n_impacted > 0)

fi_by_basin_long <- fi_by_basin %>%
  pivot_longer(cols = c(n_impacted, n_unaffected),
               names_to = "status",
               values_to = "n_species") %>%
  mutate(
    status = ifelse(status == "n_impacted", "Affected", "Unaffected"),
    basin_name = factor(basin_name,
                        levels = fi_by_basin %>% arrange(n_impacted) %>% pull(basin_name))
  )

p_affected <- ggplot(fi_by_basin_long, aes(x = basin_name, y = n_species, fill = status)) +
  geom_col(alpha = 0.8) +
  geom_text(data = fi_by_basin %>%
              mutate(basin_name = factor(basin_name,
                                         levels = fi_by_basin %>% arrange(n_impacted) %>% pull(basin_name))),
            aes(x = basin_name, y = n_total, fill = NULL,
                label = paste0(pct_impacted, "%")),
            hjust = -0.2, size = 3) +
  scale_fill_manual(values = c("Affected" = "#E07A5A", "Unaffected" = "#E8E0D5"),
                    name = NULL) +
  coord_flip() +
  labs(x = NULL,
       y = "Number of species",
       title = "Species affected by planned dams per river basin") +
  theme_bw() +
  theme(legend.position = "top")

p_affected

ggsave("connectivity/pci/species_affected_per_basin.png",
       p_affected, width = 8, height = 6, dpi = 300)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_pci_calculation.R
# Calculate PCI for current vs future dam scenarios + Fragmentation Index
# ADAPTED FOR MULTIPLE DISCONNECTED BASINS
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(riverconn)
library(data.table)
library(dplyr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/pci_sparse.R")


# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2   # species must occupy >= this many subcatchments

# ============================================================
# READ INPUTS
# ============================================================

river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

# Convert length_reach from meters to tens of km
# (matching Mekong study convention; ensures param^distance
# produces meaningful values with dispersal_prob range 0.3-0.9)
V(river_graph_current)$length_reach <- V(river_graph_current)$length_reach / 10000
V(river_graph_future)$length_reach  <- V(river_graph_future)$length_reach / 10000

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  filter(!is.na(dispersal_prob))

# ============================================================
# EXTRACT BASIN_ID FROM GRAPH (same topology for both scenarios)
# ============================================================
node_basins <- data.frame(
  name = V(river_graph_current)$name,
  basin_id = V(river_graph_current)$basin_id
) %>%
  filter(!is.na(basin_id))

message("Network has ", length(unique(node_basins$basin_id)), " basins")

# ============================================================
# PREPARE SPECIES LIST
# ============================================================
fish_distribution <- occurrences %>%
  select(species, subc_id) %>%
  distinct() %>%
  group_by(species) %>%
  filter(n() >= MIN_SUBCATCHMENTS) %>%
  ungroup()

fish_species <- fish_distribution %>%
  filter(species %in% fish_dis_class$species) %>%
  pull(species) %>%
  unique()

message(length(fish_species), " species will be processed")

# ============================================================
# HELPER: Attach species presence weights to a graph
# ============================================================
attach_presence <- function(g, presence_df) {
  graph_from_data_frame(
    as_data_frame(g, "edges"),
    v = as_data_frame(g, "vertices") %>%
      select(-weight) %>%
      left_join(presence_df, by = "name") %>%
      mutate(weight = ifelse(is.na(weight), 0, weight))
  )
}

# ============================================================
# PROCESS ALL SPECIES
# ============================================================

catchment_pci_list    <- list()
start_time <- Sys.time()

for (name_loop in fish_species) {

  # Species presence
  subcatchments_presence <- fish_distribution %>%
    filter(species == name_loop) %>%
    mutate(name = as.character(subc_id)) %>%
    select(name) %>%
    distinct() %>%
    mutate(weight = 1)

  # Which basins?
  species_basins <- node_basins %>%
    filter(name %in% subcatchments_presence$name) %>%
    pull(basin_id) %>%
    unique()

  if (length(species_basins) == 0) next

  # Dispersal parameter
  dispersal_exp <- fish_dis_class %>%
    filter(species == name_loop) %>%
    pull(dispersal_prob)

  # Loop over basins
  for (basin_loop in species_basins) {

    # Extract basin nodes
    basin_nodes <- node_basins %>%
      filter(basin_id == basin_loop) %>%
      pull(name)

    # Species presence in this basin
    basin_presence <- subcatchments_presence %>%
      filter(name %in% basin_nodes)

    if (nrow(basin_presence) < MIN_SUBCATCHMENTS) next

    # Extract basin subgraphs for both scenarios
    basin_graph_current <- induced_subgraph(river_graph_current, basin_nodes)
    basin_graph_future  <- induced_subgraph(river_graph_future, basin_nodes)

    # Attach presence to both
    graph_current <- attach_presence(basin_graph_current, basin_presence)
    graph_future  <- attach_presence(basin_graph_future, basin_presence)

    # Calculate PCI — CURRENT scenario
    pci_current <- pci_sparse(
      graph_current,
      weight     = "weight",
      field_B    = "length_reach",
      param      = dispersal_exp,
      index_type = "full"
    ) %>%
      mutate(species  = name_loop,
             basin_id = basin_loop,
             scenario = "current")

    # Calculate PCI — FUTURE scenario
    pci_future <- pci_sparse(
      graph_future,
      weight     = "weight",
      field_B    = "length_reach",
      param      = dispersal_exp,
      index_type = "full"
    ) %>%
      mutate(species  = name_loop,
             basin_id = basin_loop,
             scenario = "future")

    # Fragmentation Index: % connectivity loss from planned dams
    fi_val <- (pci_current$index - pci_future$index) / pci_current$index * 100

    result_key <- paste0(name_loop, "_basin", basin_loop)

    catchment_pci_list[[result_key]] <- bind_rows(pci_current, pci_future) %>%
      mutate(FI = ifelse(scenario == "future", fi_val, NA))


    # Cleanup
    rm(basin_graph_current, basin_graph_future, graph_current, graph_future,
       pci_current, pci_future, basin_presence)

  } # End basin loop

  # Progress
  sp_idx <- which(fish_species == name_loop)
  if (sp_idx %% 10 == 0) {
    message("Processed ", sp_idx, " / ", length(fish_species), " species")
    gc(verbose = FALSE)
  }

} # End species loop

end_time <- Sys.time()
elapsed_time <- end_time - start_time
message("Processing complete!")
message("Time elapsed: ", round(elapsed_time, 2), " ", units(elapsed_time))

# ============================================================
# SAVE RESULTS
# ============================================================

# one PCI value per species per basin.
# It's a single number summarizing overall connectivity across
# the entire basin for that species. This is what we use for
# the Fragmentation Index (FI) — comparing current vs future
# at the whole-basin level.
catchment_pci_full    <- do.call(rbind, catchment_pci_list)

dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)
saveRDS(catchment_pci_full,    "connectivity/pci/catchment_pci_full.RDS")
# catchment_pci_full <- readRDS("connectivity/pci/catchment_pci_full.RDS")
# ============================================================
# SUMMARY TABLE
# ============================================================

fi_summary <- catchment_pci_full %>%
  pivot_wider(
    id_cols = c(species, basin_id),
    names_from = scenario,
    values_from = c(index, FI)
  ) %>%
  rename(
    PCI_current = index_current,
    PCI_future  = index_future,
    FI = FI_future
  ) %>%
  select(species, basin_id, PCI_current, PCI_future, FI) %>%
  arrange(desc(FI))

write.table(fi_summary, "connectivity/pci/fi_summary.txt",
            row.names = FALSE, quote = FALSE, sep = "\t")

message("\nDone! ", nrow(fi_summary), " species x basin combinations")

# ============================================================
# QUICK DIAGNOSTICS
# ============================================================

cat("\n========== RESULTS SUMMARY ==========\n")

cat("\nCatchment PCI by scenario:\n")
catchment_pci_full %>%
  group_by(scenario) %>%
  summarize(
    n = n(),
    min_pci = round(min(index, na.rm = TRUE), 3),
    median_pci = round(median(index, na.rm = TRUE), 3),
    max_pci = round(max(index, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>% print()

cat("\nFragmentation Index (connectivity loss from planned dams):\n")
fi_summary %>%
  summarize(
    n_total      = n(),
    n_FI_zero    = sum(FI == 0, na.rm = TRUE),
    n_FI_above0  = sum(FI > 0, na.rm = TRUE),
    n_FI_above10 = sum(FI > 10, na.rm = TRUE),
    n_FI_above25 = sum(FI > 25, na.rm = TRUE),
    median_FI    = round(median(FI, na.rm = TRUE), 1),
    max_FI       = round(max(FI, na.rm = TRUE), 1)
  ) %>% print()

cat("\nTop 10 most impacted species x basin:\n")
fi_summary %>% head(10) %>% print()

# ============================================================
# CHECK VJOSA
# ============================================================
cat("\n========== VJOSA BASIN (1292502) ==========\n")
fi_summary %>% filter(basin_id == 1292502) %>% print()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_pci_calculation.R
# Calculate PCI for current vs future dam scenarios + Fragmentation Index
# ADAPTED FOR SPECIES HABITAT SUBGRAPHS (occurrence-based, not basin-based)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(riverconn)
library(data.table)
library(dplyr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/pci_sparse.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/05_connectivity/get_subgraph_between_points.R")

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2   # species must occupy >= this many subcatchments
UPSTREAM_BUFFER   <- 3   # reaches to extend upstream from occurrence points

# ============================================================
# READ INPUTS
# ============================================================

river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

# Convert length_reach from meters to tens of km
V(river_graph_current)$length_reach <- V(river_graph_current)$length_reach / 10000
V(river_graph_future)$length_reach  <- V(river_graph_future)$length_reach  / 10000

occurrences    <- fread("points_snapped/fish/fish_all_species_snapped.csv")
fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  filter(!is.na(dispersal_prob))

# ============================================================
# EXTRACT BASIN_ID FROM GRAPH
# (kept for output labelling — still useful to know which basin
#  a species' subgraph falls in)
# ============================================================
node_basins <- data.frame(
  name     = V(river_graph_current)$name,
  basin_id = V(river_graph_current)$basin_id
) %>%
  filter(!is.na(basin_id))

message("Network has ", length(unique(node_basins$basin_id)), " basins")

# ============================================================
# PREPARE SPECIES LIST
# ============================================================
fish_distribution <- occurrences %>%
  select(species, subc_id) %>%
  distinct() %>%
  group_by(species) %>%
  filter(n() >= MIN_SUBCATCHMENTS) %>%
  ungroup()

fish_species <- fish_distribution %>%
  filter(species %in% fish_dis_class$species) %>%
  pull(species) %>%
  unique()

message(length(fish_species), " species will be processed")

# ============================================================
# HELPER: Attach species presence weights to a graph
# ============================================================
attach_presence <- function(g, presence_df) {
  graph_from_data_frame(
    as_data_frame(g, "edges"),
    vertices = as_data_frame(g, "vertices") %>%
      select(-weight) %>%
      left_join(presence_df, by = "name") %>%
      mutate(weight = ifelse(is.na(weight), 0, weight))
  )
}

# ============================================================
# PRE-COMPUTE SPECIES HABITAT SUBGRAPH NODE SETS
# (topology from river_graph_current; same nodes used for future)
# ============================================================
message("Pre-computing species habitat subgraphs...")

species_habitat_nodes <- lapply(fish_species, function(sp) {
  sp_reach_ids <- fish_distribution %>%
    filter(species == sp) %>%
    pull(subc_id)

  sg <- get_subgraph_between_points(
    graph             = river_graph_current,
    species_reach_ids = sp_reach_ids,
    upstream_buffer   = UPSTREAM_BUFFER
  )

  if (is.null(sg)) return(NULL)
  igraph::V(sg)$name
})
names(species_habitat_nodes) <- fish_species

n_null <- sum(sapply(species_habitat_nodes, is.null))
message("  Subgraphs built: ", length(fish_species) - n_null,
        " | Failed (NULL): ", n_null)

# ============================================================
# PROCESS ALL SPECIES
# ============================================================

catchment_pci_list <- list()
start_time <- Sys.time()

for (name_loop in fish_species) {

  habitat_nodes <- species_habitat_nodes[[name_loop]]
  if (is.null(habitat_nodes)) next

  # Species presence (node weights)
  subcatchments_presence <- fish_distribution %>%
    filter(species == name_loop) %>%
    mutate(name = as.character(subc_id)) %>%
    select(name) %>%
    distinct() %>%
    mutate(weight = 1)

  # Label output with basin_id(s) — informational only
  # A species may span multiple basins; collapse to string
  species_basins <- node_basins %>%
    filter(name %in% subcatchments_presence$name) %>%
    pull(basin_id) %>%
    unique()

  if (length(species_basins) == 0) next

  # Extract habitat subgraphs for both scenarios from same node set
  habitat_graph_current <- induced_subgraph(
    river_graph_current,
    V(river_graph_current)[V(river_graph_current)$name %in% habitat_nodes]
  )
  habitat_graph_future <- induced_subgraph(
    river_graph_future,
    V(river_graph_future)[V(river_graph_future)$name %in% habitat_nodes]
  )

  # Attach presence weights
  graph_current <- attach_presence(habitat_graph_current, subcatchments_presence)
  graph_future  <- attach_presence(habitat_graph_future,  subcatchments_presence)

  # Calculate PCI — CURRENT scenario
  pci_current <- pci_sparse(
    graph_current,
    weight     = "weight",
    field_B    = "length_reach",
    param      = fish_dis_class %>% filter(species_name == name_loop) %>% pull(dispersal_prob),
    index_type = "full"
  ) %>%
    mutate(species  = name_loop,
           basin_id = paste(species_basins, collapse = ";"),
           scenario = "current")

  # Calculate PCI — FUTURE scenario
  pci_future <- pci_sparse(
    graph_future,
    weight     = "weight",
    field_B    = "length_reach",
    param      = fish_dis_class %>% filter(species_name == name_loop) %>% pull(dispersal_prob),
    index_type = "full"
  ) %>%
    mutate(species  = name_loop,
           basin_id = paste(species_basins, collapse = ";"),
           scenario = "future")

  # Fragmentation Index
  fi_val <- (pci_current$index - pci_future$index) / pci_current$index * 100

  catchment_pci_list[[name_loop]] <- bind_rows(pci_current, pci_future) %>%
    mutate(FI = ifelse(scenario == "future", fi_val, NA))

  # Cleanup
  rm(habitat_graph_current, habitat_graph_future, graph_current, graph_future,
     pci_current, pci_future)

  # Progress
  sp_idx <- which(fish_species == name_loop)
  if (sp_idx %% 10 == 0) {
    message("Processed ", sp_idx, " / ", length(fish_species), " species")
    gc(verbose = FALSE)
  }

} # End species loop

end_time <- Sys.time()
elapsed_time <- end_time - start_time
message("Processing complete!")
message("Time elapsed: ", round(elapsed_time, 2), " ", units(elapsed_time))

# ============================================================
# SAVE RESULTS
# ============================================================
catchment_pci_full <- do.call(rbind, catchment_pci_list)

dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)
saveRDS(catchment_pci_full, "connectivity/pci/catchment_pci_full_subhabitats.RDS")

# ============================================================
# SUMMARY TABLE
# ============================================================
fi_summary <- catchment_pci_full %>%
  pivot_wider(
    id_cols     = c(species, basin_id),
    names_from  = scenario,
    values_from = c(index, FI)
  ) %>%
  rename(
    PCI_current = index_current,
    PCI_future  = index_future,
    FI          = FI_future
  ) %>%
  select(species, basin_id, PCI_current, PCI_future, FI) %>%
  arrange(desc(FI))

write.table(fi_summary, "connectivity/pci/fi_summary_subhabitats.txt",
            row.names = FALSE, quote = FALSE, sep = "\t")

message("\nDone! ", nrow(fi_summary), " species processed")

# ============================================================
# QUICK DIAGNOSTICS
# ============================================================
cat("\n========== RESULTS SUMMARY ==========\n")

cat("\nPCI by scenario:\n")
catchment_pci_full %>%
  group_by(scenario) %>%
  summarize(
    n          = n(),
    min_pci    = round(min(index, na.rm = TRUE), 3),
    median_pci = round(median(index, na.rm = TRUE), 3),
    max_pci    = round(max(index, na.rm = TRUE), 3),
    .groups    = "drop"
  ) %>% print()

cat("\nFragmentation Index (connectivity loss from planned dams):\n")
fi_summary %>%
  summarize(
    n_total      = n(),
    n_FI_zero    = sum(FI == 0,  na.rm = TRUE),
    n_FI_above0  = sum(FI > 0,   na.rm = TRUE),
    n_FI_above10 = sum(FI > 10,  na.rm = TRUE),
    n_FI_above25 = sum(FI > 25,  na.rm = TRUE),
    median_FI    = round(median(FI, na.rm = TRUE), 1),
    max_FI       = round(max(FI,    na.rm = TRUE), 1)
  ) %>% print()

cat("\nTop 10 most impacted species:\n")
fi_summary %>% head(10) %>% print()

# ============================================================
# CHECK VJOSA
# ============================================================
cat("\n========== VJOSA BASIN (1292502) ==========\n")
fi_summary %>% filter(grepl("1292502", basin_id)) %>% print()
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02 -- fragmentation_ALL_species.R
# Calculate network fragmentation metrics for ALL species in ALL basins
# WITH SCENARIO ANALYSIS (existing vs existing+planned dams)
# Parallelized for efficiency
#
# SCENARIOS:
#   1. Current state (existing dams only)
#   2. Future scenario (existing + planned dams)
#
# INPUT:
#   - river_graph.RDS
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - fragmentation_all_scenarios.csv (all combinations, both scenarios)
#   - fragmentation_by_species.csv (species summaries)
#   - fragmentation_by_basin.csv (basin summaries)
#   - scenario_impact_summary.csv (additional impact of planned dams)
#
# LOCATION: workflows/05_connectivity/02_fragmentation_ALL_species.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# Create output directories
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

MIN_OCCURRENCES <- 2   # Minimum species occurrences in a basin to analyze
N_CORES <- detectCores() - 1  # Parallel processing cores

message("=== NETWORK FRAGMENTATION ANALYSIS - ALL SPECIES & BASINS WITH SCENARIOS ===")
message("Date: ", Sys.Date())
message("Cores: ", N_CORES)

# ============================================================
# STEP 1: Load river network
# ============================================================

message("\n[1/7] Loading river network graph...")
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

message("  Network loaded: ",
        vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# Extract node data
node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id,
  length_reach = V(river_graph)$length_reach
) %>%
  filter(!is.na(basin_id))

# ============================================================
# STEP 2: Load fish occurrences and dams
# ============================================================

message("\n[2/7] Loading fish and dam data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")
dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter dams by status
dams_existing <- dams_all %>%
  filter(status == "existing")

dams_planned <- dams_all %>%
  filter(status == "planned") %>%
  filter(phase != "R")  # Exclude rejected dams

dams_all_future <- dams_all %>%
  filter(phase != "R") %>%
  filter(status %in% c("existing", "planned"))

# Add basin_id to dams
dams_existing <- dams_existing %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

dams_planned <- dams_planned %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

dams_all_future <- dams_all_future %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

message("  Fish occurrences: ", nrow(fish_all))
message("  Species: ", length(unique(fish_all$species)))
message("  Existing dams: ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))
message("  Total future dams: ", nrow(dams_all_future))

# ============================================================
# STEP 3: Identify species × basin combinations to analyze
# ============================================================

message("\n[3/7] Identifying species × basin combinations...")

# Count occurrences per species per basin
species_basin_counts <- fish_all %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id), basin_id != "") %>%
  group_by(species, basin_id) %>%
  summarise(
    n_occurrences = n(),
    .groups = "drop"
  ) %>%
  filter(n_occurrences >= MIN_OCCURRENCES)

# Keep only combinations where basin has dams (existing OR planned)
basins_with_dams <- unique(c(
  dams_existing$basin_id,
  dams_planned$basin_id
)) %>% as.character()

species_basin_counts <- species_basin_counts %>%
  filter(basin_id %in% basins_with_dams)

message("  Basins with dams: ", length(basins_with_dams))
message("  Total combinations to analyze: ", nrow(species_basin_counts))
message("  Species: ", length(unique(species_basin_counts$species)))
message("  Basins: ", length(unique(species_basin_counts$basin_id)))

# ============================================================
# STEP 4: Define analysis function for one species × basin × scenario
# ============================================================

analyze_species_basin_scenario <- function(species_name, basin_id_val,
                                           river_graph, node_data,
                                           fish_all, dams_existing, dams_all_future) {

  tryCatch({

    # Extract basin subgraph
    basin_nodes <- node_data %>%
      filter(basin_id == basin_id_val) %>%
      pull(name)

    if (length(basin_nodes) == 0) {
      return(NULL)
    }

    basin_graph <- induced_subgraph(river_graph, basin_nodes)

    # Get species occurrences
    species_occurrences <- fish_all %>%
      filter(species == species_name,
             as.character(subc_id) %in% basin_nodes) %>%
      distinct(subc_id, .keep_all = TRUE)

    n_occurrences <- nrow(species_occurrences)

    if (n_occurrences < 2) {
      return(NULL)
    }

    species_nodes <- as.character(species_occurrences$subc_id)
    species_node_indices <- which(V(basin_graph)$name %in% species_nodes)

    # Get dams in this basin
    dams_basin_existing <- dams_existing %>%
      filter(as.character(basin_id) == basin_id_val)

    dams_basin_future <- dams_all_future %>%
      filter(as.character(basin_id) == basin_id_val)

    # Calculate original (unfragmented) distance matrix
    distance_matrix_original <- distances(
      basin_graph,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_graph)$length_reach,
      mode = "all"
    )

    # ------------------------------------------------------------
    # SCENARIO 1: Existing dams
    # ------------------------------------------------------------

    if (nrow(dams_basin_existing) > 0) {
      # Fragment by existing dams
      dam_nodes_existing <- unique(as.character(dams_basin_existing$subc_id))
      dam_nodes_in_graph_existing <- dam_nodes_existing[dam_nodes_existing %in% V(basin_graph)$name]

      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids_existing <- which(edge_df$from %in% dam_nodes_in_graph_existing |
                                       edge_df$to %in% dam_nodes_in_graph_existing)

      if (length(dam_edge_ids_existing) > 0) {
        basin_fragmented_existing <- delete_edges(basin_graph, dam_edge_ids_existing)
      } else {
        basin_fragmented_existing <- basin_graph
      }
    } else {
      basin_fragmented_existing <- basin_graph
      dam_edge_ids_existing <- integer(0)
    }

    # Calculate distances in existing scenario
    distance_matrix_existing <- distances(
      basin_fragmented_existing,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_fragmented_existing)$length_reach,
      mode = "all"
    )

    # Analyze pairs - existing scenario
    results_existing <- analyze_pairs(
      distance_matrix_original,
      distance_matrix_existing,
      species_nodes
    )

    # ------------------------------------------------------------
    # SCENARIO 2: Future (existing + planned dams)
    # ------------------------------------------------------------

    if (nrow(dams_basin_future) > 0) {
      # Fragment by all future dams
      dam_nodes_future <- unique(as.character(dams_basin_future$subc_id))
      dam_nodes_in_graph_future <- dam_nodes_future[dam_nodes_future %in% V(basin_graph)$name]

      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids_future <- which(edge_df$from %in% dam_nodes_in_graph_future |
                                     edge_df$to %in% dam_nodes_in_graph_future)

      if (length(dam_edge_ids_future) > 0) {
        basin_fragmented_future <- delete_edges(basin_graph, dam_edge_ids_future)
      } else {
        basin_fragmented_future <- basin_graph
      }
    } else {
      basin_fragmented_future <- basin_graph
      dam_edge_ids_future <- integer(0)
    }

    # Calculate distances in future scenario
    distance_matrix_future <- distances(
      basin_fragmented_future,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_fragmented_future)$length_reach,
      mode = "all"
    )

    # Analyze pairs - future scenario
    results_future <- analyze_pairs(
      distance_matrix_original,
      distance_matrix_future,
      species_nodes
    )

    # ------------------------------------------------------------
    # Combine results
    # ------------------------------------------------------------

    data.frame(
      species = species_name,
      basin_id = basin_id_val,
      n_occurrences = n_occurrences,

      # Existing scenario
      n_dams_existing = nrow(dams_basin_existing),
      n_dam_edges_removed_existing = length(dam_edge_ids_existing),
      total_pairs = results_existing$n_total,
      disconnected_pairs_existing = results_existing$n_disconnected,
      percent_lost_existing = round(100 * results_existing$n_disconnected / results_existing$n_total, 2),

      # Future scenario
      n_dams_future = nrow(dams_basin_future),
      n_dam_edges_removed_future = length(dam_edge_ids_future),
      disconnected_pairs_future = results_future$n_disconnected,
      percent_lost_future = round(100 * results_future$n_disconnected / results_future$n_total, 2),

      # Additional impact of planned dams
      n_dams_planned = nrow(dams_basin_future) - nrow(dams_basin_existing),
      additional_pairs_disconnected = results_future$n_disconnected - results_existing$n_disconnected,
      additional_percent_lost = round(
        (100 * results_future$n_disconnected / results_future$n_total) -
          (100 * results_existing$n_disconnected / results_existing$n_total), 2
      ),

      # Distance statistics (existing scenario)
      mean_dist_connected_existing = results_existing$mean_dist_connected,
      mean_dist_disconnected_existing = results_existing$mean_dist_disconnected,
      t_test_p_existing = results_existing$t_test_p,

      # Distance statistics (future scenario)
      mean_dist_connected_future = results_future$mean_dist_connected,
      mean_dist_disconnected_future = results_future$mean_dist_disconnected,
      t_test_p_future = results_future$t_test_p,

      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    # Return error info
    data.frame(
      species = species_name,
      basin_id = basin_id_val,
      error = TRUE,
      error_message = as.character(e),
      stringsAsFactors = FALSE
    )
  })
}

# Helper function to analyze pairs
analyze_pairs <- function(dist_original, dist_fragmented, species_nodes) {

  n_total <- 0
  n_disconnected <- 0

  dist_connected <- c()
  dist_disconnected_original <- c()

  for (i in 1:(length(species_nodes) - 1)) {
    for (j = (i + 1):length(species_nodes)) {

      n_total <- n_total + 1

      dist_orig <- dist_original[i, j]
      dist_frag <- dist_fragmented[i, j]

      if (is.finite(dist_orig) && !is.finite(dist_frag)) {
        # Newly disconnected
        n_disconnected <- n_disconnected + 1
        dist_disconnected_original <- c(dist_disconnected_original, dist_orig)
      } else if (is.finite(dist_frag)) {
        # Still connected
        dist_connected <- c(dist_connected, dist_frag)
      }
    }
  }

  # Statistical test - ONLY if BOTH groups have data
  t_test_p <- NA
  if (length(dist_connected) > 1 && length(dist_disconnected_original) > 1) {
    t_result <- tryCatch(
      t.test(dist_connected, dist_disconnected_original),
      error = function(e) NULL
    )
    if (!is.null(t_result)) {
      t_test_p <- round(t_result$p.value, 4)
    }
  }

  list(
    n_total = n_total,
    n_disconnected = n_disconnected,
    n_connected = n_total - n_disconnected,  # ADD THIS
    mean_dist_connected = if (length(dist_connected) > 0) round(mean(dist_connected), 1) else NA,
    mean_dist_disconnected = if (length(dist_disconnected_original) > 0) {
      round(mean(dist_disconnected_original), 1)
    } else {
      NA
    },
    t_test_p = t_test_p,
    test_feasible = length(dist_connected) > 1 && length(dist_disconnected_original) > 1  # ADD THIS
  )
}

# ============================================================
# STEP 5: Run parallel analysis
# ============================================================

message("\n[4/7] Running parallel scenario analysis...")
message("  Processing ", nrow(species_basin_counts), " combinations...")

# Set up parallel cluster
cl <- makeCluster(N_CORES, type = "FORK")
registerDoParallel(cl)

start_time <- Sys.time()

# Run analysis in parallel
results <- foreach(
  i = 1:nrow(species_basin_counts),
  .packages = c("igraph", "dplyr"),
  .combine = rbind,
  .errorhandling = "pass"
) %dopar% {

  if (i %% 50 == 0) {
    cat(sprintf("Processed %d / %d combinations\n", i, nrow(species_basin_counts)))
  }

  row <- species_basin_counts[i, ]

  analyze_species_basin_scenario(
    species_name = row$species,
    basin_id_val = row$basin_id,
    river_graph = river_graph,
    node_data = node_data,
    fish_all = fish_all,
    dams_existing = dams_existing,
    dams_all_future = dams_all_future
  )
}

stopCluster(cl)

end_time <- Sys.time()
elapsed <- end_time - start_time

message("  Analysis complete!")
message("  Time elapsed: ", round(elapsed, 2), " ", units(elapsed))

# ============================================================
# STEP 6: Process and save results
# ============================================================

message("\n[5/7] Processing results...")

# Filter out errors
if ("error" %in% colnames(results)) {
  errors <- results %>% filter(error == TRUE)
  if (nrow(errors) > 0) {
    message("  WARNING: ", nrow(errors), " combinations failed")
    fwrite(errors, "connectivity/fragmentation_errors.csv")
  }
  results <- results %>% filter(is.na(error) | error == FALSE)
}

results <- results %>% select(-matches("^error"))

message("  Successfully analyzed: ", nrow(results), " combinations")

# Save full results
fwrite(results, "connectivity/fragmentation_all_scenarios.csv")
message("  Saved: connectivity/fragmentation_all_scenarios.csv")

# ============================================================
# STEP 7: Create summaries
# ============================================================

message("\n[6/7] Creating summary statistics...")

# Overall summary
overall_summary <- results %>%
  summarise(
    n_combinations = n(),
    n_species = n_distinct(species),
    n_basins = n_distinct(basin_id),

    # Existing scenario
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    median_loss_existing = median(percent_lost_existing, na.rm = TRUE),

    # Future scenario
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    median_loss_future = median(percent_lost_future, na.rm = TRUE),

    # Impact of planned dams
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    n_with_planned_impact = sum(additional_pairs_disconnected > 0, na.rm = TRUE),

    total_dams_existing = sum(n_dams_existing, na.rm = TRUE),
    total_dams_planned = sum(n_dams_planned, na.rm = TRUE)
  )

print(overall_summary)

fwrite(overall_summary, "connectivity/scenario_impact_summary.csv")

# Species summary
species_summary <- results %>%
  group_by(species) %>%
  summarise(
    n_basins = n(),
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    max_loss_existing = max(percent_lost_existing, na.rm = TRUE),
    max_loss_future = max(percent_lost_future, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_loss_future))

fwrite(species_summary, "connectivity/fragmentation_by_species.csv")

# Basin summary
basin_summary <- results %>%
  group_by(basin_id) %>%
  summarise(
    n_species = n(),
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    total_dams_existing = first(n_dams_existing),
    total_dams_planned = first(n_dams_planned),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_loss_future))

fwrite(basin_summary, "connectivity/fragmentation_by_basin.csv")

# ============================================================
# STEP 8: Create visualizations
# ============================================================

message("\n[7/7] Creating visualizations...")

png("connectivity/scenario_comparison_greece.png",
    width = 14, height = 6, units = "in", res = 300)

par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

# Panel A: Distribution comparison
hist(results$percent_lost_existing,
     breaks = 30, col = rgb(0.99, 0.71, 0.38, 0.5),
     border = "white",
     main = "Connectivity Loss Distribution",
     xlab = "Connectivity loss (%)",
     ylab = "Number of combinations",
     xlim = c(0, 100),
     cex.lab = 1.3, cex.main = 1.5)

hist(results$percent_lost_future,
     breaks = 30, col = rgb(0.99, 0.55, 0.38, 0.5),
     border = "white", add = TRUE)

legend("topright",
       c("Existing dams", "Existing + Planned"),
       fill = c(rgb(0.99, 0.71, 0.38, 0.5), rgb(0.99, 0.55, 0.38, 0.5)),
       border = "white", bty = "n")

# Panel B: Additional impact
barplot(c(
  overall_summary$mean_loss_existing,
  overall_summary$mean_loss_future
),
names.arg = c("Current", "Future"),
col = c("#FDB462", "#FC8D62"),
border = "white",
main = "Mean Connectivity Loss",
ylab = "Mean loss (%)",
ylim = c(0, max(c(overall_summary$mean_loss_existing, overall_summary$mean_loss_future)) * 1.2),
cex.lab = 1.3, cex.main = 1.5)

text(0.7, overall_summary$mean_loss_existing + 2,
     paste0(round(overall_summary$mean_loss_existing, 1), "%"),
     cex = 1.2, font = 2)

text(1.9, overall_summary$mean_loss_future + 2,
     paste0(round(overall_summary$mean_loss_future, 1), "%"),
     cex = 1.2, font = 2)

# Panel C: Most affected basins
top_basins <- basin_summary %>%
  head(15) %>%
  arrange(mean_loss_future)

barplot(top_basins$mean_loss_future,
        names.arg = top_basins$basin_id,
        horiz = TRUE,
        col = "#FC8D62",
        border = "white",
        main = "Top 15 Most Fragmented Basins",
        xlab = "Mean connectivity loss (%)",
        cex.lab = 1.3, cex.main = 1.5,
        cex.names = 0.8,
        las = 1)

dev.off()

message("  Saved: connectivity/scenario_comparison_greece.png")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== SCENARIO ANALYSIS COMPLETE ===")
message("Output files:")
message("  • connectivity/fragmentation_all_scenarios.csv")
message("  • connectivity/fragmentation_by_species.csv")
message("  • connectivity/fragmentation_by_basin.csv")
message("  • connectivity/scenario_impact_summary.csv")
message("  • connectivity/scenario_comparison_greece.png")
message("\nKey findings:")
message("  • ", nrow(results), " species × basin combinations analyzed")
message("  • ", overall_summary$n_species, " species across ", overall_summary$n_basins, " basins")
message("  • Current (existing dams): ", round(overall_summary$mean_loss_existing, 1), "% mean loss")
message("  • Future (+ planned): ", round(overall_summary$mean_loss_future, 1), "% mean loss")
message("  • Additional impact: +", round(overall_summary$mean_additional_loss, 1), "% from planned dams")
message("  • ", overall_summary$n_with_planned_impact, " combinations affected by planned dams")
message("\nAnalysis completed: ", Sys.time())
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02 -- fragmentation_scenarios.R
# Calculate network fragmentation metrics for 2 dam scenarios
# Uses igraph (consistent with PCI workflow)
#
# SCENARIOS:
#   1. Current state (existing dams only)
#   2. Future scenario (existing + planned dams)
#
# INPUT:
#   - river_graph.RDS
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - Scenario comparison figures (2-panel plots)
#   - Summary statistics for both scenarios
#
# LOCATION: workflows/05_connectivity/02_fragmentation_scenarios.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(ggspatial)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# Create output directory
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502
EXAMPLE_SPECIES <- "Barbus_prespensis"

message("=== NETWORK FRAGMENTATION SCENARIOS ANALYSIS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load river network
# ============================================================

message("\n[1/9] Loading river network graph...")
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

message("  Network loaded: ",
        vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# ============================================================
# STEP 2: Extract target basin
# ============================================================

message("\n[2/9] Extracting target basin...")

# Extract basin info from nodes
node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id,
  length_reach = V(river_graph)$length_reach
) %>%
  filter(!is.na(basin_id))

# Get nodes in target basin
basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

# Extract subgraph
basin_graph <- induced_subgraph(river_graph, basin_nodes)

message("  Basin extracted:")
message("    Reaches: ", vcount(basin_graph))

# ============================================================
# STEP 3: Load fish occurrences
# ============================================================

message("\n[3/9] Loading fish occurrence data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# Filter to target basin
fish_basin <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

# Extract example species
species_occurrences <- fish_basin %>%
  filter(species == EXAMPLE_SPECIES |
           species == gsub("_", " ", EXAMPLE_SPECIES)) %>%
  distinct(subc_id, .keep_all = TRUE)

n_occurrences <- nrow(species_occurrences)
message("  Species occurrences: ", n_occurrences)

# Get species node indices
species_nodes <- as.character(species_occurrences$subc_id)
species_nodes <- species_nodes[species_nodes %in% V(basin_graph)$name]
species_node_indices <- which(V(basin_graph)$name %in% species_nodes)

max_connections <- (length(species_nodes) * (length(species_nodes) - 1)) / 2
message("  Maximum possible connections: ", max_connections)

# ============================================================
# STEP 4: Load and categorize dams
# ============================================================

message("\n[4/9] Loading dam data...")

dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter to target basin
dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

message("  Total dams in basin: ", nrow(dams_basin))

# Separate by status
dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

dams_all_future <- dams_basin %>%
  filter(phase != "R") %>%
  filter(status %in% c("existing", "planned"))

message("  Existing dams (current): ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))
message("  Total future dams: ", nrow(dams_all_future))

# ============================================================
# STEP 5: Define fragmentation function
# ============================================================

calculate_fragmentation <- function(basin_graph, species_node_indices,
                                    dam_subcatchments = NULL,
                                    scenario_name = "Baseline") {

  # Fragment network if dams provided
  if (!is.null(dam_subcatchments) && length(dam_subcatchments) > 0) {

    dam_nodes_in_graph <- dam_subcatchments[dam_subcatchments %in% V(basin_graph)$name]

    if (length(dam_nodes_in_graph) > 0) {
      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids <- which(edge_df$from %in% dam_nodes_in_graph |
                              edge_df$to %in% dam_nodes_in_graph)

      if (length(dam_edge_ids) > 0) {
        fragmented_graph <- delete_edges(basin_graph, dam_edge_ids)
        n_edges_removed <- length(dam_edge_ids)
      } else {
        fragmented_graph <- basin_graph
        n_edges_removed <- 0
      }
    } else {
      fragmented_graph <- basin_graph
      n_edges_removed <- 0
    }
  } else {
    fragmented_graph <- basin_graph
    n_edges_removed <- 0
  }

  # Calculate distances
  distance_matrix <- distances(
    fragmented_graph,
    v = species_node_indices,
    to = species_node_indices,
    weights = E(fragmented_graph)$length_reach,
    mode = "all"
  )

  # Analyze pairwise connections
  n_total <- 0
  n_connected <- 0
  n_disconnected <- 0

  dist_connected_vals <- c()
  dist_disconnected_original <- c()

  # Also need original (unfragmented) distances for comparison
  distance_matrix_original <- distances(
    basin_graph,
    v = species_node_indices,
    to = species_node_indices,
    weights = E(basin_graph)$length_reach,
    mode = "all"
  )

  for (i in 1:(nrow(distance_matrix) - 1)) {
    for (j in (i + 1):ncol(distance_matrix)) {

      n_total <- n_total + 1
      dist_current <- distance_matrix[i, j]
      dist_original <- distance_matrix_original[i, j]

      if (is.finite(dist_current)) {
        n_connected <- n_connected + 1
        dist_connected_vals <- c(dist_connected_vals, dist_current)
      } else {
        n_disconnected <- n_disconnected + 1
        dist_disconnected_original <- c(dist_disconnected_original, dist_original)
      }
    }
  }

  # Component analysis
  comp <- components(fragmented_graph)

  # Statistical test
  t_test_p <- NA
  if (length(dist_connected_vals) > 1 && length(dist_disconnected_original) > 1) {
    t_result <- tryCatch(
      t.test(dist_connected_vals, dist_disconnected_original),
      error = function(e) NULL
    )
    if (!is.null(t_result)) {
      t_test_p <- t_result$p.value
    }
  }

  # Return results
  list(
    scenario = scenario_name,
    n_total = n_total,
    n_connected = n_connected,
    n_disconnected = n_disconnected,
    percent_lost = round(100 * n_disconnected / n_total, 2),
    dist_connected = dist_connected_vals,
    dist_disconnected_original = dist_disconnected_original,
    mean_dist_connected = ifelse(length(dist_connected_vals) > 0,
                                 mean(dist_connected_vals), NA),
    mean_dist_disconnected = ifelse(length(dist_disconnected_original) > 0,
                                    mean(dist_disconnected_original), NA),
    t_test_p = t_test_p,
    n_components = comp$no,
    n_edges_removed = n_edges_removed,
    graph = fragmented_graph
  )
}

# ============================================================
# STEP 6: Run both scenarios
# ============================================================

message("\n[5/9] Calculating fragmentation for both scenarios...")

# Scenario 1: Current state (existing dams)
message("  Scenario 1: Current state (existing dams)")
scenario_current <- calculate_fragmentation(
  basin_graph,
  species_node_indices,
  dam_subcatchments = unique(as.character(dams_existing$subc_id)),
  scenario_name = "Current"
)

# Scenario 2: Future state (existing + planned dams)
message("  Scenario 2: Future state (existing + planned dams)")
scenario_future <- calculate_fragmentation(
  basin_graph,
  species_node_indices,
  dam_subcatchments = unique(as.character(dams_all_future$subc_id)),
  scenario_name = "Future"
)

# ============================================================
# STEP 7: Create summary comparison
# ============================================================

message("\n[6/9] Creating scenario comparison summary...")

summary_df <- data.frame(
  Scenario = c("Current (existing dams)", "Future (existing + planned)"),
  Total_pairs = c(scenario_current$n_total, scenario_future$n_total),
  Connected_pairs = c(scenario_current$n_connected, scenario_future$n_connected),
  Disconnected_pairs = c(scenario_current$n_disconnected, scenario_future$n_disconnected),
  Percent_lost = c(scenario_current$percent_lost, scenario_future$percent_lost),
  N_components = c(scenario_current$n_components, scenario_future$n_components),
  N_dams = c(nrow(dams_existing), nrow(dams_all_future)),
  Mean_dist_connected = c(scenario_current$mean_dist_connected,
                          scenario_future$mean_dist_connected),
  Mean_dist_disconnected = c(scenario_current$mean_dist_disconnected,
                             scenario_future$mean_dist_disconnected)
)

print(summary_df)

write.csv(summary_df,
          "connectivity/scenario_comparison_summary.csv",
          row.names = FALSE)

# Calculate additional impact of planned dams
additional_disconnected <- scenario_future$n_disconnected - scenario_current$n_disconnected
additional_percent <- scenario_future$percent_lost - scenario_current$percent_lost

message("\nAdditional impact of planned dams:")
message("  Additional pairs disconnected: ", additional_disconnected)
message("  Additional connectivity loss: ", round(additional_percent, 1), " percentage points")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== SCENARIO ANALYSIS COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nScenario Comparison:")
message("  CURRENT STATE (existing dams):")
message("     - Dams: ", nrow(dams_existing))
message("     - Disconnected pairs: ", scenario_current$n_disconnected, " / ", scenario_current$n_total)
message("     - Connectivity loss: ", scenario_current$percent_lost, "%")
message("     - Network components: ", scenario_current$n_components)
message("")
message("  FUTURE STATE (existing + planned dams):")
message("     - Dams: ", nrow(dams_all_future), " (+", nrow(dams_planned), " planned)")
message("     - Disconnected pairs: ", scenario_future$n_disconnected, " / ", scenario_future$n_total)
message("     - Connectivity loss: ", scenario_future$percent_lost, "%")
message("     - Network components: ", scenario_future$n_components)
message("")
message("  ADDITIONAL IMPACT OF PLANNED DAMS:")
message("     - Additional pairs disconnected: +", additional_disconnected)
message("     - Additional connectivity loss: +", round(additional_percent, 1), " percentage points")
message("     - Additional components created: +",
        scenario_future$n_components - scenario_current$n_components)
message("\nConservation message:")
message("  Preventing construction of ", nrow(dams_planned), " planned dams would avoid")
message("  disconnecting ", additional_disconnected, " additional population pairs.")
message("\nOutput Files:")
message("  • connectivity/scenario_comparison_summary.csv")
message("  • connectivity/scenario_comparison_bars.png")
message("  • connectivity/Figure_Scenarios_MEE.png")
message("  • connectivity/scenario_comparison_map.html")
message("  • connectivity/Figure_Scenarios_caption.txt")
message("\nAnalysis complete: ", Sys.time())
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03 -- dam_impact_zones.R
# Calculate affected habitat within impact zones around dams
# impact zone = upstream/downstream buffer zones from each dam
#
# CONCEPT:
#   - Each dam affects habitat within a certain radius (up/downstream)
#   - impact zone = river segments within these radii
#   - Quantifies total habitat length affected by dams
#
# INPUT:
#   - river_graph.RDS (from script 04_spatial_network/02_generate_network_graph.R)
#   - partial_stream_network.gpkg (spatial network)
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - impact_zone_by_dam.gpkg (spatial impact zones)
#   - impact_zone_summary.csv (affected lengths by dam/scenario)
#   - impact_zone_map.html (interactive map)
#
# LOCATION: workflows/05_connectivity/023_dam_impact_zones.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(lwgeom)
library(leaflet)
library(htmlwidgets)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# Create output directory
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502  # Same basin as fragmentation analysis
EXAMPLE_SPECIES <- "Barbus_prespensis"

# impact zone radii (meters)
UPSTREAM_RADIUS <- 500    # Distance upstream affected by dam
DOWNSTREAM_RADIUS <- 2000  # Distance downstream affected by dam

message("=== DAM IMPACT ZONE ANALYSIS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Upstream radius: ", UPSTREAM_RADIUS, " m")
message("Downstream radius: ", DOWNSTREAM_RADIUS, " m")
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load river network (spatial version for impact zones)
# ============================================================

message("\n[1/7] Loading spatial river network...")

stream_network <- st_read("spatial/stream_networks/partial_stream_network.gpkg",
                          quiet = TRUE)

# Filter to target basin
# First need to get basin_id from graph or node data
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id
) %>%
  filter(!is.na(basin_id))

basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

# Filter spatial network to basin
basin_streams <- stream_network %>%
  filter(subc_id %in% basin_nodes)

message("  Basin streams: ", nrow(basin_streams))
message("  Total length: ", round(sum(basin_streams$length) / 1000, 1), " km")

# ============================================================
# STEP 2: Load dams and categorize
# ============================================================

message("\n[2/7] Loading dam data...")

dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter to target basin
dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

# Separate by status
dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

message("  Existing dams: ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))

# ============================================================
# STEP 3: Load species occurrences
# ============================================================

message("\n[3/7] Loading species occurrence data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# Filter to target basin and species
species_occurrences <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes) %>%
  filter(species == EXAMPLE_SPECIES |
           species == gsub("_", " ", EXAMPLE_SPECIES))

message("  Species occurrences: ", nrow(species_occurrences))

# ============================================================
# STEP 4: Calculate impact zones for existing dams
# ============================================================

message("\n[4/7] Calculating impact zones for existing dams...")

# Convert existing dams to sf points
dams_existing_sf <- dams_existing %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"),
           crs = 4326)

# Calculate impact zone using hydrographr function
message("  Running get_buffer_along_the_network()...")

impact_zone_existing <- get_buffer_along_the_network(
  lines_sf = basin_streams,
  start_points = dams_existing_sf,
  up_radius = UPSTREAM_RADIUS,
  down_radius = DOWNSTREAM_RADIUS
)

message("  Impact zone segments: ", nrow(impact_zone_existing))

# Add dam metadata
impact_zone_existing <- impact_zone_existing %>%
  mutate(dam_status = "existing")

# ============================================================
# STEP 5: Calculate impact zones for planned dams
# ============================================================

message("\n[5/7] Calculating impact zones for planned dams...")

if (nrow(dams_planned) > 0) {

  # Convert planned dams to sf points
  dams_planned_sf <- dams_planned %>%
    st_as_sf(coords = c("longitude_snapped", "latitude_snapped"),
             crs = 4326)

  impact_zone_planned <- get_buffer_along_the_network(
    lines_sf = basin_streams,
    start_points = dams_planned_sf,
    up_radius = UPSTREAM_RADIUS,
    down_radius = DOWNSTREAM_RADIUS
  )

  message("  Impact zone segments: ", nrow(impact_zone_planned))

  # Add dam metadata
  impact_zone_planned <- impact_zone_planned %>%
    mutate(dam_status = "planned")

  # Combine both scenarios
  impact_zone_all <- bind_rows(
    impact_zone_existing,
    impact_zone_planned
  )

} else {
  message("  No planned dams - skipping")
  impact_zone_all <- impact_zone_existing
}

# Save impact zones
st_write(impact_zone_all,
         "connectivity/impact_zone_by_dam.gpkg",
         delete_dsn = TRUE)

message("  Saved: connectivity/impact_zone_by_dam.gpkg")

# ============================================================
# STEP 6: Calculate affected habitat statistics
# ============================================================

message("\n[6/7] Calculating affected habitat statistics...")

# Total length of basin
total_basin_length <- sum(basin_streams$length, na.rm = TRUE)

# Affected length by existing dams
affected_length_existing <- impact_zone_existing %>%
  st_drop_geometry() %>%
  summarise(total_length = sum(length, na.rm = TRUE)) %>%
  pull(total_length)

# Affected length by all dams (existing + planned)
affected_length_all <- impact_zone_all %>%
  st_drop_geometry() %>%
  summarise(total_length = sum(length, na.rm = TRUE)) %>%
  pull(total_length)

# Calculate proportions
prop_existing <- affected_length_existing / total_basin_length
prop_all <- affected_length_all / total_basin_length

# Create summary
impact_zone_summary <- data.frame(
  basin_id = TARGET_BASIN_ID,
  species = EXAMPLE_SPECIES,
  upstream_radius_m = UPSTREAM_RADIUS,
  downstream_radius_m = DOWNSTREAM_RADIUS,
  total_basin_length_m = total_basin_length,
  n_dams_existing = nrow(dams_existing),
  n_dams_planned = nrow(dams_planned),
  affected_length_existing_m = affected_length_existing,
  affected_length_all_m = affected_length_all,
  proportion_affected_existing = round(prop_existing, 4),
  proportion_affected_all = round(prop_all, 4),
  additional_affected_m = affected_length_all - affected_length_existing,
  additional_proportion = round(prop_all - prop_existing, 4)
)

print(impact_zone_summary)

write.csv(impact_zone_summary,
          "connectivity/impact_zone_summary.csv",
          row.names = FALSE)

message("\nSummary Statistics:")
message("  Total basin length: ", round(total_basin_length / 1000, 1), " km")
message("  Affected by existing dams: ", round(affected_length_existing / 1000, 1),
        " km (", round(100 * prop_existing, 1), "%)")
message("  Affected by all dams: ", round(affected_length_all / 1000, 1),
        " km (", round(100 * prop_all, 1), "%)")
message("  Additional impact of planned dams: ",
        round((affected_length_all - affected_length_existing) / 1000, 1), " km")


# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== IMPACT ZONE ANALYSIS COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nImpact Zone Parameters:")
message("  Upstream radius: ", UPSTREAM_RADIUS, " m")
message("  Downstream radius: ", DOWNSTREAM_RADIUS, " m")
message("\nHabitat Impact:")
message("  Total basin length: ", round(total_basin_length / 1000, 1), " km")
message("  Affected by existing dams: ", round(affected_length_existing / 1000, 1),
        " km (", round(100 * prop_existing, 1), "%)")
message("  Additional impact if planned dams built: ",
        round((affected_length_all - affected_length_existing) / 1000, 1), " km")
message("  Total affected (existing + planned): ", round(affected_length_all / 1000, 1),
        " km (", round(100 * prop_all, 1), "%)")
message("\nOutput Files:")
message("  • connectivity/impact_zone_by_dam.gpkg")
message("  • connectivity/impact_zone_summary.csv")
message("  • connectivity/impact_zone_map.html")
message("\nAnalysis complete: ", Sys.time())
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03 -- combined_visualizations.R
# Create publication-ready figures combining fragmentation and impact zone
# Reads analysis outputs from scripts 01 and 02
#
# READS:
#   - connectivity/scenario_comparison_summary.csv (from script 01)
#   - connectivity/impact_zone_summary.csv (from script 02)
#   - connectivity/impact_zone_by_dam.gpkg (from script 02)
#   - spatial/stream_networks/partial_stream_network.gpkg
#   - points_snapped/dams/dams_snapped_points.csv
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - spatial/stream_networks/river_graph.RDS
#
# CREATES:
#   - connectivity/Figure_Combined_MEE.png (4-panel combined figure)
#   - connectivity/Figure_Combined_MEE.tiff (for publication)
#   - connectivity/combined_impact_map.html (interactive map)
#   - connectivity/combined_summary_table.csv (integrated metrics)
#   - connectivity/Figure_Combined_caption.txt (figure caption)
#   - connectivity/scenario_comparison_bars.png (from script 01)
#   - connectivity/Figure_Scenarios_MEE.png (from script 01)
#   - connectivity/scenario_comparison_map.html (from script 01)
#
# LOCATION: workflows/04_network_analysis/03_combined_visualizations.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(igraph)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502
EXAMPLE_SPECIES <- "Barbus_prespensis"

message("=== COMBINED IMPACT VISUALIZATIONS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load analysis results
# ============================================================

message("\n[1/6] Loading analysis results...")

# Fragmentation results (from script 01)
fragmentation_summary <- read.csv("connectivity/scenario_comparison_summary.csv")

# Impact zone results (from script 02)
impact_zone_summary <- read.csv("connectivity/impact_zone_summary.csv")

# Impact zone spatial data (from script 02)
impact_zone_all <- st_read("connectivity/impact_zone_by_dam.gpkg", quiet = TRUE)

message("  Fragmentation data loaded")
message("  Impact zone data loaded")

# ============================================================
# STEP 2: Load spatial data for maps
# ============================================================

message("\n[2/6] Loading spatial data...")

# River network
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id
) %>%
  filter(!is.na(basin_id))

basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

stream_spatial <- st_read("spatial/stream_networks/partial_stream_network.gpkg", quiet = TRUE)

basin_streams <- stream_spatial %>%
  filter(subc_id %in% basin_nodes) %>%
  st_transform(4326)

# Dams
dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

dams_existing_sf <- dams_existing %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_planned_sf <- dams_planned %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Species
fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

species_occurrences <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes) %>%
  filter(species == EXAMPLE_SPECIES | species == gsub("_", " ", EXAMPLE_SPECIES))

species_sf <- species_occurrences %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Impact zones
impact_zone_existing <- impact_zone_all %>%
  filter(dam_status == "existing")

impact_zone_planned <- impact_zone_all %>%
  filter(dam_status == "planned")

# Dam-affected streams
dam_existing_streams <- basin_streams %>%
  filter(subc_id %in% unique(as.character(dams_existing$subc_id)))

dam_planned_streams <- basin_streams %>%
  filter(subc_id %in% unique(as.character(dams_planned$subc_id)))

message("  Spatial data prepared")

# ============================================================
# STEP 3: Create combined summary table
# ============================================================

message("\n[3/6] Creating combined summary table...")

combined_summary <- data.frame(
  Basin_ID = TARGET_BASIN_ID,
  Species = EXAMPLE_SPECIES,

  # Existing Dams Scenario
  Existing_Dams_N = fragmentation_summary$N_dams[1],
  Existing_Habitat_Affected_km = round(impact_zone_summary$affected_length_existing_m / 1000, 2),
  Existing_Habitat_Affected_Pct = round(100 * impact_zone_summary$proportion_affected_existing, 1),
  Existing_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[1],
  Existing_Connectivity_Loss_Pct = fragmentation_summary$Percent_lost[1],
  Existing_Network_Components = fragmentation_summary$N_components[1],

  # Future Scenario (Existing + Planned)
  Future_Dams_N = fragmentation_summary$N_dams[2],
  Future_Habitat_Affected_km = round(impact_zone_summary$affected_length_all_m / 1000, 2),
  Future_Habitat_Affected_Pct = round(100 * impact_zone_summary$proportion_affected_all, 1),
  Future_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[2],
  Future_Connectivity_Loss_Pct = fragmentation_summary$Percent_lost[2],
  Future_Network_Components = fragmentation_summary$N_components[2],

  # Additional Impact of Planned Dams
  Additional_Habitat_Affected_km = round(impact_zone_summary$additional_affected_m / 1000, 2),
  Additional_Habitat_Affected_Pct = round(100 * impact_zone_summary$additional_proportion, 1),
  Additional_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[2] -
    fragmentation_summary$Disconnected_pairs[1],
  Additional_Connectivity_Loss_Pct = round(fragmentation_summary$Percent_lost[2] -
                                             fragmentation_summary$Percent_lost[1], 1)
)

# Transpose for readability
combined_summary_t <- t(combined_summary)
colnames(combined_summary_t) <- "Value"

write.csv(combined_summary_t,
          "connectivity/combined_summary_table.csv",
          row.names = TRUE)

print(combined_summary_t)

message("  Saved: connectivity/combined_summary_table.csv")

# ============================================================
# STEP 4: Create combined MEE figure (4 panels)
# ============================================================

message("\n[4/6] Creating combined MEE figure...")

png("connectivity/Figure_Combined_MEE.png",
    width = 173, height = 140, units = "mm", res = 300)

layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

# Extract values for plotting
habitat_affected <- c(
  100 * impact_zone_summary$proportion_affected_existing,
  100 * impact_zone_summary$proportion_affected_all
)

conn_loss <- c(
  fragmentation_summary$Percent_lost[1],
  fragmentation_summary$Percent_lost[2]
)

pairs_lost <- c(
  fragmentation_summary$Disconnected_pairs[1],
  fragmentation_summary$Disconnected_pairs[2]
)

additional_metrics <- c(
  fragmentation_summary$Percent_lost[2] - fragmentation_summary$Percent_lost[1],
  100 * impact_zone_summary$additional_proportion
)

# ------------------------------------------------------------
# Panel A: Connectivity Loss
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(conn_loss,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Connectivity loss (%)",
        main = "(a) Population Connectivity",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(conn_loss) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     conn_loss + max(conn_loss) * 0.05,
     paste0(round(conn_loss, 1), "%"),
     cex = 1.2,
     font = 2)

mtext(paste(nrow(dams_existing), "dams          ",
            nrow(dams_existing) + nrow(dams_planned), "dams"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# ------------------------------------------------------------
# Panel B: Habitat Affected
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(habitat_affected,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Habitat affected (%)",
        main = "(b) Habitat Impact",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(habitat_affected) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     habitat_affected + max(habitat_affected) * 0.05,
     paste0(round(habitat_affected, 1), "%"),
     cex = 1.2,
     font = 2)

mtext(paste(round(impact_zone_summary$affected_length_existing_m/1000, 1), "km          ",
            round(impact_zone_summary$affected_length_all_m/1000, 1), "km"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# ------------------------------------------------------------
# Panel C: Disconnected Pairs
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(pairs_lost,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Disconnected pairs",
        main = "(c) Population Isolation",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(pairs_lost) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost,
     cex = 1.2,
     font = 2)

# ------------------------------------------------------------
# Panel D: Additional Impact Comparison
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(additional_metrics,
        names.arg = c("Connectivity\nloss", "Habitat\naffected"),
        col = "#FC8D62",
        ylab = "Additional impact (% points)",
        main = "(d) Planned Dams Impact",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.0,
        cex.axis = 1.0,
        ylim = c(0, max(additional_metrics) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     additional_metrics + max(additional_metrics) * 0.05,
     paste0("+", round(additional_metrics, 1), "%"),
     cex = 1.2,
     font = 2,
     col = "darkred")

dev.off()

message("  Saved: connectivity/Figure_Combined_MEE.png")

# Create TIFF version
tiff("connectivity/Figure_Combined_MEE.tiff",
     width = 173, height = 140, units = "mm", res = 300,
     compression = "lzw")

# Repeat same plotting code
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

# Panel A
par(mar = c(5, 4.5, 3, 0.5))
barplot(conn_loss, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Connectivity loss (%)",
        main = "(a) Population Connectivity", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(conn_loss) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), conn_loss + max(conn_loss) * 0.05,
     paste0(round(conn_loss, 1), "%"), cex = 1.2, font = 2)
mtext(paste(nrow(dams_existing), "dams          ",
            nrow(dams_existing) + nrow(dams_planned), "dams"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# Panel B
par(mar = c(5, 4.5, 3, 0.5))
barplot(habitat_affected, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Habitat affected (%)",
        main = "(b) Habitat Impact", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(habitat_affected) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), habitat_affected + max(habitat_affected) * 0.05,
     paste0(round(habitat_affected, 1), "%"), cex = 1.2, font = 2)
mtext(paste(round(impact_zone_summary$affected_length_existing_m/1000, 1), "km          ",
            round(impact_zone_summary$affected_length_all_m/1000, 1), "km"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# Panel C
par(mar = c(5, 4.5, 3, 0.5))
barplot(pairs_lost, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Disconnected pairs",
        main = "(c) Population Isolation", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(pairs_lost) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost, cex = 1.2, font = 2)

# Panel D
par(mar = c(5, 4.5, 3, 0.5))
barplot(additional_metrics, names.arg = c("Connectivity\nloss", "Habitat\naffected"),
        col = "#FC8D62", ylab = "Additional impact (% points)",
        main = "(d) Planned Dams Impact", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.0, cex.axis = 1.0, ylim = c(0, max(additional_metrics) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), additional_metrics + max(additional_metrics) * 0.05,
     paste0("+", round(additional_metrics, 1), "%"),
     cex = 1.2, font = 2, col = "darkred")

dev.off()

message("  Saved: connectivity/Figure_Combined_MEE.tiff")

# ============================================================
# Create combined figure caption
# ============================================================

caption_combined <- paste0(
  "Integrated dam impact analysis on ", EXAMPLE_SPECIES, " in basin ", TARGET_BASIN_ID,
  " combining population connectivity and habitat degradation metrics under current and future dam scenarios. ",

  "(a) Population connectivity loss: Dams disconnect ", fragmentation_summary$Percent_lost[1], "% ",
  "of population pairs under current conditions (", nrow(dams_existing), " existing dams), ",
  "increasing to ", fragmentation_summary$Percent_lost[2], "% if all ", nrow(dams_planned),
  " planned dams are constructed (", nrow(dams_existing) + nrow(dams_planned), " total dams). ",

  "(b) Habitat impact: Dam footprints (", service_area_summary$upstream_radius_m, " m upstream, ",
  service_area_summary$downstream_radius_m, " m downstream) affect ",
  round(100 * service_area_summary$proportion_affected_existing, 1), "% of total habitat ",
  "(", round(service_area_summary$affected_length_existing_m / 1000, 1), " km) under current conditions, ",
  "increasing to ", round(100 * service_area_summary$proportion_affected_all, 1), "% ",
  "(", round(service_area_summary$affected_length_all_m / 1000, 1), " km) with planned dam construction. ",

  "(c) Population isolation: Number of disconnected population pairs increases from ",
  pairs_lost[1], " (current) to ", pairs_lost[2], " (future), representing ",
  pairs_lost[2] - pairs_lost[1], " additional isolated populations. ",

  "(d) Cumulative impact of planned dams: Construction of ", nrow(dams_planned), " planned dams would cause ",
  "an additional ", round(additional_metrics[1], 1), " percentage point loss in connectivity and ",
  round(additional_metrics[2], 1), " percentage point increase in habitat degradation. ",

  "The combined analysis reveals compound effects: planned dams would simultaneously increase ",
  "both direct habitat loss (physical footprint) and functional habitat loss (population isolation). ",
  "Dam footprints represent immediate physical impacts within service areas, while connectivity loss ",
  "represents network-scale functional fragmentation that can extend far beyond dam locations. ",
  "This dual impact threatens metapopulation viability through both reduced habitat quantity and ",
  "compromised population connectivity essential for genetic exchange and recolonization."
)

writeLines(caption_combined, "connectivity/Figure_Combined_caption.txt")

message("  Saved: connectivity/Figure_Combined_caption.txt")





# ============================================================
# STEP 5: Create standalone fragmentation figures (from script 01)
# ============================================================

message("\n[5/6] Creating standalone fragmentation figures...")

# Figure: Side-by-side comparison bars
png("connectivity/scenario_comparison_bars.png",
    width = 14, height = 6, units = "in", res = 300)

par(mfrow = c(1, 3), mar = c(6, 5, 4, 2))

# Panel A: Connectivity loss
barplot(conn_loss,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Connectivity loss (%)",
        main = "(a) Connectivity Loss",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(conn_loss) * 1.25), border = "white")
text(c(0.7, 1.9), conn_loss + max(conn_loss) * 0.05,
     paste0(conn_loss, "%"), cex = 1.4, font = 2)

# Panel B: Network components
barplot(fragmentation_summary$N_components,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Network components",
        main = "(b) Network Fragmentation",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(fragmentation_summary$N_components) * 1.25), border = "white")
text(c(0.7, 1.9), fragmentation_summary$N_components + max(fragmentation_summary$N_components) * 0.05,
     fragmentation_summary$N_components, cex = 1.4, font = 2)

# Panel C: Disconnected pairs
barplot(pairs_lost,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Disconnected pairs",
        main = "(c) Lost Connections",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(pairs_lost) * 1.25), border = "white")
text(c(0.7, 1.9), pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost, cex = 1.4, font = 2)

dev.off()

message("  Saved: connectivity/scenario_comparison_bars.png")

# ============================================================
# STEP 6: Create combined interactive map
# ============================================================

message("\n[6/6] Creating combined interactive map...")
# Separate impact zones
impact_zone_existing_only <- impact_zone_all %>%
  filter(dam_status == "existing")

impact_zone_planned_only <- impact_zone_all %>%
  filter(dam_status == "planned")

# Create map
service_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  # Original network
  addPolylines(
    data = basin_streams,
    color = "#1f78b4",
    weight = 2,
    opacity = 0.6,
    group = "River Network",
    popup = ~paste0("Subcatchment: ", subc_id, "<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Impact zone - existing dams
  addPolylines(
    data = impact_zone_existing_only,
    color = "#d7191c",      # strong red = existing impact
    weight = 4,
    opacity = 0.8,
    dashArray = "5,5",
    group = "Impact Zone (Existing)",
    popup = ~paste0("Affected by existing dam<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Impact zone - planned dams
  addPolylines(
    data = impact_zone_planned_only,
    color = "#fdae61",      # amber = future/planned impact
    weight = 4,
    opacity = 0.8,
    dashArray = "10,5",
    group = "Impact Zone (Planned)",
    popup = ~paste0("Affected by planned dam<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Existing dams
  addCircleMarkers(
    data = dams_existing_sf,
    radius = 6,
    color = "#252525",      # near-black border
    fillColor = "#636363",  # mid-grey fill = existing structure
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 2,
    group = "Existing Dams",
    popup = ~paste0("Dam: ", ifelse(is.na(id1), "Unnamed", id1), "<br>",
                    "Status: Existing")
  ) %>%
  # Planned dams
  addCircleMarkers(
    data = if(nrow(dams_planned) > 0) dams_planned_sf else NULL,
    radius = 6,
    color = "#252525",      # same border as existing
    fillColor = "#bdbdbd",  # light grey fill = planned (lighter = not yet built)
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Planned Dams",
    popup = ~paste0("Dam: ", ifelse(is.na(id1), "Unnamed", id1), "<br>",
                    "Status: Planned")
  ) %>%
  # Species occurrences
  addCircleMarkers(
    data = species_sf,
    radius = 5,
    color = "#1b7837",      # dark green border
    fillColor = "#7fbf7b",  # mid green fill = biological
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    group = EXAMPLE_SPECIES,
    popup = ~paste0("Species: ", species, "<br>",
                    "Site: ", site_id)
  ) %>%
  # Layer controls
  addLayersControl(
    baseGroups = c("Light", "Satellite"),
    overlayGroups = c(
      "River Network",
      "Impact Zone (Existing)",
      "Impact Zone (Planned)",
      "Existing Dams",
      "Planned Dams",
      EXAMPLE_SPECIES
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Legend
  addLegend(
    position = "bottomright",
    colors = c("#1f78b4", "#d7191c", "#fdae61", "#636363", "#bdbdbd", "#7fbf7b"),
    labels = c(
      "River Network",
      paste0("Impact Zone (Existing): ", round(affected_length_existing / 1000, 1), " km"),
      paste0("Additional (Planned): ",
             round((affected_length_all - affected_length_existing) / 1000, 1), " km"),
      "Existing Dams",
      "Planned Dams",
      EXAMPLE_SPECIES
    ),
    opacity = 0.8,
    title = "Legend"
  ) %>%
  addScaleBar(position = "bottomleft") %>%
# Title
  addControl(
    html = paste0(
      "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
      "<h3 style='margin: 0; color: #333;'>Dam Impact Zones - Basin ", TARGET_BASIN_ID, "</h3>",
      "<p style='margin: 5px 0 0 0; color: #666;'>",
      "<strong>Species:</strong> ", EXAMPLE_SPECIES, "<br>",
      "<strong>Impact zone:</strong> ", UPSTREAM_RADIUS, " m up, ", DOWNSTREAM_RADIUS, " m down<br>",
      "<strong>Existing dams:</strong> ", nrow(dams_existing),
      " (", round(100 * prop_existing, 1), "% habitat affected)<br>",
      "<strong>+ Planned:</strong> ", nrow(dams_existing) + nrow(dams_planned),  # FIXED
      " total (", round(100 * prop_all, 1), "% habitat affected)",
      "</p></div>"
    ),
    position = "topright"
  )

service_map

# Save map
# save_to_nimbus(service_map,
#                "connectivity/impact_zone_map.html")
saveWidget(service_map,
               "connectivity/impact_zone_map.html")

message("  Saved: connectivity/impact_zone_map.html")


# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== VISUALIZATION CREATION COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nFigures Created:")
message("  • connectivity/Figure_Combined_MEE.png (4-panel combined)")
message("  • connectivity/Figure_Combined_MEE.tiff (for publication)")
message("  • connectivity/scenario_comparison_bars.png (3-panel fragmentation)")
message("  • connectivity/combined_impact_map.html (interactive map)")
message("\nData Products:")
message("  • connectivity/combined_summary_table.csv")
message("  • connectivity/Figure_Combined_caption.txt")
message("\nKey Findings:")
message("  Current Impact:")
message("    - Connectivity lost: ", fragmentation_summary$Percent_lost[1], "%")
message("    - Habitat affected: ", round(100 * impact_zone_summary$proportion_affected_existing, 1), "%")
message("  Future Impact:")
message("    - Connectivity lost: ", fragmentation_summary$Percent_lost[2], "%")
message("    - Habitat affected: ", round(100 * impact_zone_summary$proportion_affected_all, 1), "%")
message("  Additional Impact of Planned Dams:")
message("    - +", round(additional_metrics[1], 1), "% connectivity loss")
message("    - +", round(additional_metrics[2], 1), "% habitat affected")
message("\nAnalysis complete: ", Sys.time())
get_subgraph_between_points <- function(graph, species_reach_ids, upstream_buffer = 3) {

  species_reach_ids <- as.character(species_reach_ids)

  # Keep only IDs that exist in graph
  occurrence_nodes <- species_reach_ids[species_reach_ids %in% igraph::V(graph)$name]

  if (length(occurrence_nodes) == 0) {
    warning("No occurrence nodes found in graph")
    return(NULL)
  }

  if (length(occurrence_nodes) == 1) {
    # Only one node — just take upstream buffer from it
    upstream <- igraph::ego(graph, order = upstream_buffer,
                            nodes = occurrence_nodes, mode = "in")
    return(igraph::induced_subgraph(graph, unique(unlist(upstream))))
  }

  # 1. Find all shortest paths between all pairs of occurrence nodes
  #    Use mode = "all" to traverse regardless of edge direction
  all_path_nodes <- c()

  for (i in seq_along(occurrence_nodes)) {
    paths <- suppressWarnings(igraph::shortest_paths(
      graph   = graph,
      from    = occurrence_nodes[i],
      to      = occurrence_nodes[-i],
      mode    = "all",       # undirected traversal — follows river network topology
      output  = "vpath"
    )
    )$vpath

    path_nodes <- unlist(lapply(paths, as.integer))
    all_path_nodes <- c(all_path_nodes, path_nodes)
  }

  all_path_nodes <- unique(all_path_nodes)

  # 2. Upstream buffer from every occurrence node
  upstream_nodes <- igraph::ego(
    graph = graph,
    order = upstream_buffer,
    nodes = occurrence_nodes,
    mode  = "in"
  )
  upstream_node_ids <- unique(unlist(lapply(upstream_nodes, as.integer)))

  # 3. Combine and return subgraph
  all_node_ids <- unique(c(all_path_nodes, upstream_node_ids))
  igraph::induced_subgraph(graph, all_node_ids)
}

# ###################### test
# library(sf)
# library(igraph)
# library(dplyr)
#
#
#
# output_dir <- "connectivity/species_subnetworks"
# dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
#
#
# test_ids <- occurrences |> filter(species == "Barbus_prespensis") |> pull(subc_id)
# test_sub <- get_subgraph_between_points(river_graph_current, test_ids)
# igraph::vcount(test_sub)  # should be much smaller than 131029
#
#
#
# # Clean species names for filenames (replace spaces with underscores)
# species_list <- unique(occurrences$species)
#
# for (sp in species_list) {
#
#   sp_filename <- gsub(" ", "_", sp)
#
#   # --- 1. Species occurrence points ---
#   sp_points <- occurrences |>
#     filter(species == sp) |>
#     st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)
#
#   st_write(
#     sp_points,
#     file.path(output_dir, paste0(sp_filename, "_occurrences.gpkg")),
#     delete_dsn = TRUE,
#     quiet = TRUE
#   )
#
#   # --- 2. Species subnetwork ---
#   sp_reach_ids <- occurrences |>
#     filter(species == sp) |>
#     pull(subc_id)
#
#   sp_subgraph <- get_subgraph_between_points(
#     graph             = river_graph_current,
#     species_reach_ids = sp_reach_ids,
#     upstream_buffer   = 3
#   )
#
#   sp_subc_ids <- as.integer(igraph::V(sp_subgraph)$name)
#
#   sp_network <- subcatchments |>
#     filter(subc_id %in% sp_subc_ids)
#
#   st_write(
#     sp_network,
#     file.path(output_dir, paste0(sp_filename, "_subnetwork.gpkg")),
#     delete_dsn = TRUE,
#     quiet = TRUE
#   )
#
#   message("Written: ", sp_filename)
# }
#
# subcs <- occurrences %>% filter(species=="Barbus_prespensis") %>% pull(subc_id)
# sub<-get_subgraph_between_points(
#   graph             = river_graph_current,
#   species_reach_ids = subcs,
#   upstream_buffer   = 3
# )
#
#
# species_subgraphs <- occurrences |>
#   split(~species) |>
#   lapply(function(df) {
#     get_subgraph_between_points(
#       graph             = river_graph_current,
#       species_reach_ids = df$subc_id,
#       upstream_buffer   = 3
#     )
#   })
library(sf)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)


basin_file <- "/home/grigoropoulou/Documents/Postdoc/projects/hydrographr_use_case_greece/r.watershed/basin_tiles20d/basin_greece.gpkg"
basins <- read_geopackage(basin_file, import_as = "sf",subc_id = basin_summary$basin_id,
                          name = "ID")


# Get basin centroids from stream network
basin_centroids <- basins %>%
  group_by(ID) %>%
  summarize(geometry = st_union(geom)) %>%
  st_centroid() %>%
  st_transform(4326)

# Join with results
basin_map_data <- basin_summary %>%
  left_join(
    basin_centroids %>% st_drop_geometry() %>%
      mutate(lon = st_coordinates(basin_centroids)[,1],
             lat = st_coordinates(basin_centroids)[,2]),
    by = c("basin_id" = "ID")
  )





# CRITICAL FIX: Single color palette using the MAXIMUM value across BOTH scenarios
max_loss <- max(c(basin_summary$mean_loss_existing,
                  basin_summary$mean_loss_future), na.rm = TRUE)
# Create THREE layers:
# 1. Existing (baseline)
# 2. Future (total impact)
# 3. Change (additional impact from planned)

# Color palette for change (diverging)
pal_change <- colorNumeric(
  palette = "RdYlGn",
  domain = c(0, max(basin_summary$mean_additional_loss, na.rm = TRUE)),
  reverse = TRUE  # Red = high additional loss
)

# Color palette for absolute loss (consistent)
pal_abs <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0, max_loss)
)

greece_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  # Layer 1: Existing scenario (yellow-red)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_abs(mean_loss_existing),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Current State",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Existing dams:</b> ", total_dams_existing, "<br>",
      "<b>Current loss:</b> ", round(mean_loss_existing, 1), "%"
    )
  ) %>%

  # Layer 2: Future scenario (same scale)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_abs(mean_loss_future),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Future State",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Total dams:</b> ", total_dams_existing + total_dams_planned,
      " (+", total_dams_planned, " planned)<br>",
      "<b>Future loss:</b> ", round(mean_loss_future, 1), "%"
    )
  ) %>%

  # Layer 3: Additional impact (different palette!)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_change(mean_additional_loss),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    group = "Additional Impact (Planned Dams)",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Planned dams:</b> ", total_dams_planned, "<br>",
      "<b>Additional loss:</b> <span style='color:red'>+",
      round(mean_additional_loss, 1), "%</span><br>",
      "<b>From:</b> ", round(mean_loss_existing, 1),
      "% → ", round(mean_loss_future, 1), "%"
    )
  ) %>%

  # Legend for Current/Future
  addLegend(
    "bottomright",
    pal = pal_abs,
    values = c(0, max_loss),
    title = "Connectivity loss (%)<br>(Current & Future)",
    opacity = 0.7,
    group = c("Current State", "Future State")
  ) %>%

  # Legend for Additional Impact
  addLegend(
    "bottomleft",
    pal = pal_change,
    values = c(0, max(basin_summary$mean_additional_loss, na.rm = TRUE)),
    title = "Additional impact<br>from planned dams (%)",
    opacity = 0.7,
    group = "Additional Impact (Planned Dams)"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("Current State", "Future State", "Additional Impact (Planned Dams)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Info box
  addControl(
    html = paste0(
      "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
      "<h4 style='margin: 0 0 10px 0;'>Dam Impact Scenarios - Greece</h4>",
      "<p style='margin: 0; font-size: 12px;'>",
      "<b>Current:</b> ", sum(basin_summary$total_dams_existing), " existing dams<br>",
      "<b>Future:</b> +", sum(basin_summary$total_dams_planned), " planned dams<br>",
      "<b>Circle size</b> = number of species<br>",
      "<b>Circle color</b> = connectivity loss",
      "</p></div>"
    ),
    position = "topright"
  )

saveWidget(greece_map, "connectivity/connectivity_loss_greece.html")
save_to_nimbus(greece_map, "connectivity/greece_scenario_map.html")



# install.packages("fwtraits")
library(fwtraits)
library(tidyverse)
library(data.table)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- BASE_DIR
setwd(BASE_DIR)

# ============================================================
# STEP 1: Authenticate with freshwaterecology.info
# ============================================================

# First time: register at https://www.freshwaterecology.info/
# and request an API key from the database managers.
# Store your API key in .Renviron:
#   fw_setapikey()
#   Add line: API_KEY=your_api_key_here
#   Then restart R

fw_be4ustart()
fw_setapikey()  # will prompt for API key or read from .Renviron

# ============================================================
# STEP 2: Check what parameters are available for fish
# ============================================================

# Get the database guide — lists all organism groups and parameters
db_guide <- fw_dbguide()

# Filter to fish parameters only
fish_params <- db_guide %>%
  filter(grepl("fi", organismgroup, ignore.case = TRUE))

cat("Available fish parameters:", nrow(fish_params), "\n")
print(fish_params %>% select(parameters_cleaned, DataType) %>% distinct())

# ============================================================
# STEP 3: Prepare species list
# ============================================================

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# fwtraits expects "Genus species" format (with space)
species_list <- data.frame(
  scientificName = gsub("_", " ", unique(occurrences$species))
)

cat("Species to query:", nrow(species_list), "\n")

# ============================================================
# STEP 4: Fetch traits — key ecological parameters
# ============================================================

# Parameters relevant for connectivity/fragmentation analysis:
# - rheophily habitat: flow preference (rheophilic vs limnophilic)
# - migration: migratory behaviour
# - spawning habitat: reproductive strategy
# - feeding diet adult: trophic guild
# - temperature tolerance: thermal sensitivity
# - body size: max body length

# Fetch traits
fish_traits_fwe <- fw_fetchdata(
  data = species_list,
  ecoparams = c(
    "rheophily habitat",
    "migration",
    "spawning habitat",
    "feeding diet adult",
    "temperature tolerance",
    "body size"
  ),
  taxonomic_column = "scientificName",
  organismgroup = "fi"
)

cat("\nTraits retrieved for", length(unique(fish_traits_fwe$taxasearched$clean)), "species\n")

# ============================================================
# STEP 5: Check coverage — which species have data?
# ============================================================

# How many species have each trait?
trait_coverage <- fish_traits_fwe$ecodata %>%
  group_by(Parameter) %>%
  summarize(
    n_species_with_data = sum(!is.na(value)),
    n_species_total = n(),
    pct_coverage = round(100 * sum(!is.na(value)) / n(), 1),
    .groups = "drop"
  )

cat("\nTrait coverage:\n")
print(trait_coverage)

# Which species have NO data at all?
species_with_data <- fish_traits_fwe %>%
  filter(!is.na(value)) %>%
  pull(scientificName) %>%
  unique()

species_missing <- setdiff(species_list$scientificName, species_with_data)
cat("\nSpecies with no data in freshwaterecology.info:", length(species_missing), "\n")
if (length(species_missing) > 0) print(species_missing)

# ============================================================
# STEP 6: Also try fetching ALL available fish parameters
# ============================================================

# Get all parameter names for fish
all_fish_params <- fish_params$parameter_cleaned

# Fetch everything (may take a while)
fish_traits_all <- fw_fetchdata(
  data = species_list,
  ecoparams = all_fish_params,
  taxonomic_column = "scientificName",
  organismgroup = "fi"
)

# Summarize what we got
all_coverage <- fish_traits_all %>%
  filter(!is.na(value)) %>%
  group_by(parameter) %>%
  summarize(n_species = n_distinct(scientificName), .groups = "drop") %>%
  arrange(desc(n_species))

cat("\nAll parameters with data (sorted by coverage):\n")
print(all_coverage, n = 30)

# ============================================================
# STEP 7: Save
# ============================================================

fwrite(fish_traits_fwe, "traits/fwtraits_fish_greece.csv")
fwrite(fish_traits_all, "traits/fwtraits_fish_greece_all.csv")
fwrite(as.data.frame(trait_coverage), "traits/fwtraits_coverage_summary.csv")

cat("\nSaved:\n")
cat("  traits/fwtraits_fish_greece.csv (selected parameters)\n")
cat("  traits/fwtraits_fish_greece_all.csv (all parameters)\n")
cat("  traits/fwtraits_coverage_summary.csv\n")





migration <- fw_fetchdata(data = 'Abramis brama',
                          organismgroup = 'fi',
                          ecoparams = 'migration',
                          cachefolder = 'cache',
                          warn = TRUE,
                          inform = TRUE,
                          details = TRUE)#the species spelling is checked
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_dispersal_classification.R
# Estimate species dispersal distances using fishmove,
# bin into quantile classes, assign dispersal probability (PD)
#
# Strategy:
#   1. Run fishmove(L, AR, SO, T) for 11 species with known aspect ratio
#   2. Fit model: log(sigma_mob) ~ log(TL) + sqrt(SO) to predict dispersal
#      for species without AR data (R² = 0.985)
#   3. Classify all species into 7 dispersal classes (PD = 0.3 to 0.9)
#
# References:
#   Radinger & Wolter (2014) Patterns and predictors of fish dispersal in rivers
#   Baldan et al. (2022) riverconn: an R package to assess river connectivity
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(readxl)
library(data.table)
# pak:pak("cran/fishmove")
library(fishmove)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
T_REF          <- 3650   # 10-year dispersal window (population-level; Mekong study)
SO_DEFAULT     <- 5      # default stream order for species without occurrence data
N_DISP_CLASSES <- 7      # number of dispersal classes
PD_MIN         <- 0.3    # minimum dispersal probability
PD_MAX         <- 0.9    # maximum dispersal probability


# ============================================================
# STEP 1: Read trait data
# ============================================================

fish_traits <- read_xlsx(
  "points_original/fish/Fish distributional & traits data (1).xlsx",
  sheet = "Traits"
) %>%
  dplyr::rename(species = Species) %>%
  dplyr::select(species, max_TL, Caudal_fin, Migration) %>%
  dplyr::mutate(
    Caudal_fin_label = case_when(
      Caudal_fin == 1 ~ "Rounded",
      Caudal_fin == 2 ~ "Truncated",
      Caudal_fin == 3 ~ "Emarginate",
      Caudal_fin == 4 ~ "Forked",
      Caudal_fin == 5 ~ "Heterocercal",
      Caudal_fin == 6 ~ "Pointed",
      TRUE ~ "Unknown"
    ),
    Migration_label = case_when(
      Migration == 0 ~ "Non-migratory",
      Migration == 1 ~ "Potamodromous",
      Migration == 2 ~ "Long_distance",
      TRUE ~ "Unknown"
    )
  )

message("Trait data: ", nrow(fish_traits), " species")


# ============================================================
# STEP 2: Get species-specific stream order from occurrences
# ============================================================

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

species_stream_order <- occurrences %>%
  group_by(species) %>%
  dplyr::summarise(
    max_SO = max(strahler, na.rm = TRUE),
    n_obs  = n(),
    .groups = "drop"
  )

fish_traits <- fish_traits %>%
  left_join(species_stream_order, by = "species")

# Fill missing stream order with default
missing_SO <- fish_traits %>% filter(is.na(max_SO))
if (nrow(missing_SO) > 0) {
  message("WARNING: ", nrow(missing_SO), " species have no stream order data. Using default SO=", SO_DEFAULT)
  fish_traits <- fish_traits %>%
    mutate(max_SO = ifelse(is.na(max_SO), SO_DEFAULT, max_SO))
}


# ============================================================
# STEP 3: Read aspect ratio data and average duplicates
# ============================================================

ar_data <- read.csv("traits/AspectRatioData.csv") %>%
  group_by(species) %>%
  summarize(AR = mean(AR, na.rm = TRUE), .groups = "drop") %>%
  mutate(species = gsub(" ", "_", species))

# Handle name mismatch
ar_data <- ar_data %>%
  mutate(species = ifelse(species == "Gasterosteus_aculeatus",
                          "Gasterosteus_gymnurus", species))

message("AR data for ", nrow(ar_data), " species")


# ============================================================
# STEP 4: Run fishmove for calibration species (with AR)
# ============================================================

calibration <- fish_traits %>%
  inner_join(ar_data, by = "species")

message("Calibration species (with TL + AR): ", nrow(calibration))

fm_results <- data.frame()

for (i in 1:nrow(calibration)) {
  sp    <- calibration$species[i]
  tl_mm <- calibration$max_TL[i] * 10  # fishmove expects mm
  ar    <- calibration$AR[i]
  so    <- calibration$max_SO[i]

  cat("  fishmove:", sp, "(L=", tl_mm, "mm, AR=", round(ar, 2), ", SO=", so, ")\n")

  tryCatch({
    fm <- fishmove(L = tl_mm, AR = ar, SO = so, T = T_REF,
                   rep = 200, seed = 42)

    fm_results <- rbind(fm_results, data.frame(
      species      = sp,
      sigma_mob_m  = fm$pred.fishmove["fit", "sigma_mob", , , , ]
    ))
  }, error = function(e) cat("    ERROR:", conditionMessage(e), "\n"))
}


# ============================================================
# STEP 5: Fit predictive model for species without AR
# Model: log(sigma_mob) ~ log(TL) + sqrt(SO)
# This mirrors fishmove's internal regression structure minus AR
# ============================================================

cal_data <- fm_results %>%
  left_join(fish_traits, by = "species") %>%
  mutate(log_sigma_mob = log(sigma_mob_m),
         log_TL  = log(max_TL),
         sqrt_SO = sqrt(max_SO))

best_model <- lm(log_sigma_mob ~ log_TL + sqrt_SO, data = cal_data)

cat("\n--- Predictive model summary ---\n")
print(summary(best_model))


# ============================================================
# STEP 6: Predict dispersal for ALL species
# ============================================================

predict_data <- fish_traits %>%
  mutate(log_TL  = log(max_TL),
         sqrt_SO = sqrt(max_SO))

predictions <- predict(best_model, newdata = predict_data,
                       interval = "prediction", level = 0.95)

fish_traits <- fish_traits %>%
  mutate(
    sigma_mob_predicted = exp(predictions[, "fit"]),
    sigma_mob_lwr       = exp(predictions[, "lwr"]),
    sigma_mob_upr       = exp(predictions[, "upr"]),
    # Use fishmove value for calibration species, prediction for the rest
    sigma_mob_final = ifelse(
      species %in% fm_results$species,
      fm_results$sigma_mob_m[match(species, fm_results$species)],
      sigma_mob_predicted
    ),
    source = ifelse(species %in% fm_results$species,
                    "AR available", "AR unavailable")
  )


# ============================================================
# STEP 7: Bin into dispersal classes (Mekong method)
# 7 quantile-based classes mapped to PD = 0.3 to 0.9
# ============================================================

quantile_breaks <- quantile(fish_traits$sigma_mob_final,
                            probs = seq(0, 1, length.out = N_DISP_CLASSES + 1),
                            na.rm = TRUE)

pd_values <- seq(PD_MIN, PD_MAX, length.out = N_DISP_CLASSES)

fish_traits <- fish_traits %>%
  mutate(
    dispersal_class = cut(sigma_mob_final,
                          breaks = quantile_breaks,
                          labels = FALSE,
                          include.lowest = TRUE),
    dispersal_prob = pd_values[dispersal_class]
  )


# ============================================================
# STEP 8: Save output
# ============================================================

fish_dis_class <- fish_traits %>%
  dplyr::select(
    species,
    distance = sigma_mob_final,
    dispersal_prob,
    dispersal_class,
    source,
    max_TL,
    max_SO,
    Migration_label,
    Caudal_fin_label
  ) %>%
  arrange(species)

fwrite(fish_dis_class, "traits/fish_dis_class.txt", sep = "\t")
message("\nfish_dis_class.txt saved")


# ============================================================
# STEP 9: Diagnostic plots
# ============================================================

# Plot 1: Calibration -- fishmove (with AR) vs predicted (without AR)
cal_check <- cal_data %>%
  mutate(predicted = exp(predict(best_model)))

p1 <- ggplot(cal_check, aes(x = sigma_mob_m, y = predicted)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = species), hjust = -0.1, vjust = -0.5, size = 2.5) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "sigma_mob (m) — with AR",
       y = "sigma_mob (m) — without AR",
       title = "Calibration: with vs without Aspect Ratio") +
  theme_bw()

# Plot 2: All species dispersal by migration type and data source
p2 <- ggplot(fish_dis_class, aes(x = max_TL, y = distance / 1000,
                                 color = Migration_label, shape = source)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10() + scale_x_log10() +
  labs(x = "Max total length (cm)",
       y = "Dispersal distance (km, log scale)",
       color = "Migration",
       title = "Dispersal estimates for all Greek freshwater fish") +
  theme_bw()

cowplot::plot_grid(p1, p2, ncol = 1, align = "v") %>%
  ggsave("traits/dispersal_diagnostics.png", ., width = 10, height = 12)

cat("\nPlots saved: traits/dispersal_diagnostics.png\n")


# ============================================================
# STEP 10: Summary
# ============================================================

cat("\n========== SUMMARY ==========\n")
cat("Calibration species (AR available):", sum(fish_dis_class$source == "AR available"), "\n")
cat("Predicted species (AR unavailable):", sum(fish_dis_class$source == "AR unavailable"), "\n\n")

cat("Dispersal distance (km) by migration type:\n")
fish_dis_class %>%
  group_by(Migration_label) %>%
  summarize(
    n = n(),
    median_km = round(median(distance / 1000), 1),
    min_km = round(min(distance / 1000), 1),
    max_km = round(max(distance / 1000), 1),
    .groups = "drop"
  ) %>% print()

cat("\nDispersal probability distribution:\n")
print(table(fish_dis_class$dispersal_prob))
library(imager)
library(magick)

# Load image
img <- load.image("traits/barbus_prespensis01-500.jpg")

# Display and click points
plot(img)
# Click to mark:
# 1-2: Total length line (for scale)
# 3-4: Fin height (tip to tip)
# Then trace fin outline for area

# Manual measurement script
measure_CAR <- function(img_path, total_length_cm = 30) {

  # Load and display image LARGE
  img <- image_read(img_path)

  # Get image info
  info <- image_info(img)
  cat("Image dimensions:", info$width, "x", info$height, "\n\n")

  # Open a large plotting window
  dev.new(width = 14, height = 10)  # Large window

  # Display image
  plot(img)

  # STEP 1: Set scale
  cat("STEP 1: Click TWO points along the 30cm body length\n")
  cat("(Click point 1, then point 2)\n")
  scale_points <- locator(2, type = "p", pch = 20, col = "red", cex = 2)

  # Draw scale line
  lines(scale_points$x, scale_points$y, col = "red", lwd = 3)

  scale_pixels <- sqrt((scale_points$x[2] - scale_points$x[1])^2 +
                         (scale_points$y[2] - scale_points$y[1])^2)
  pixels_per_cm <- scale_pixels / total_length_cm

  cat("Scale set:", round(pixels_per_cm, 2), "pixels per cm\n\n")

  # STEP 2: Measure fin height
  cat("STEP 2: Click fin height (top lobe tip, then bottom lobe tip)\n")
  fin_points <- locator(2, type = "p", pch = 20, col = "blue", cex = 2)

  # Draw height line
  lines(fin_points$x, fin_points$y, col = "blue", lwd = 3)

  fin_height_pixels <- sqrt((fin_points$x[2] - fin_points$x[1])^2 +
                              (fin_points$y[2] - fin_points$y[1])^2)
  fin_height_cm <- fin_height_pixels / pixels_per_cm

  cat("Fin height:", round(fin_height_cm, 2), "cm\n\n")

  # STEP 3: Trace fin outline - FIXED VERSION
  cat("STEP 3: Trace fin outline\n")
  cat("Click points around the fin edge\n")
  cat("Press ESC when done (not right-click)\n\n")

  # Collect points until user presses ESC
  area_points <- locator(n = 1000, type = "o", col = "green", lwd = 2)

  # Close polygon
  if (length(area_points$x) > 2) {
    lines(c(area_points$x[length(area_points$x)], area_points$x[1]),
          c(area_points$y[length(area_points$y)], area_points$y[1]),
          col = "green", lwd = 2)
  }

  # Calculate polygon area (shoelace formula)
  n <- length(area_points$x)
  if (n < 3) {
    stop("Need at least 3 points to calculate area")
  }

  # Add first point to end to close polygon
  x <- c(area_points$x, area_points$x[1])
  y <- c(area_points$y, area_points$y[1])

  area_pixels <- 0.5 * abs(sum(x[1:n] * y[2:(n+1)] - x[2:(n+1)] * y[1:n]))
  area_cm2 <- area_pixels / (pixels_per_cm^2)

  cat("Fin area:", round(area_cm2, 2), "cm²\n")
  cat("Number of outline points:", n, "\n\n")

  # Calculate CAR
  CAR <- (fin_height_cm^2) / area_cm2

  # Summary
  cat("=" , rep("=", 50), "\n", sep = "")
  cat("RESULTS:\n")
  cat("  Fin height (h):", round(fin_height_cm, 2), "cm\n")
  cat("  Fin area (A):  ", round(area_cm2, 2), "cm²\n")
  cat("  CAR = h²/A:    ", round(CAR, 2), "\n")
  cat("=" , rep("=", 50), "\n", sep = "")

  # Sanity check
  if (CAR < 1.5) {
    warning("CAR is very low (<1.5) - check if fin outline is correct")
  } else if (CAR > 5.0) {
    warning("CAR is very high (>5.0) - check if fin outline is correct")
  } else {
    cat("\n✓ CAR value looks reasonable for a forked fin (2.5-4.0)\n")
  }

  return(list(
    height_cm = fin_height_cm,
    area_cm2 = area_cm2,
    CAR = CAR,
    n_points = n
  ))
}

# Use it
results <- measure_CAR("traits/barbus_prespensis01-500.jpg", total_length_cm = 30)
# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA FROM ENVIRONMENT90M
# ============================================================================
# Purpose: Download environmental predictor variables as TXT files from Env90m
# Input: Previously downloaded stream network (partial_stream_network.gpkg)
# Output: TXT files with environmental data organized by dataset
# ============================================================================
# Date: 2026-03-23
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# LOAD EXISTING STREAM NETWORK
# ============================================================================

message("\n=== Loading Existing Stream Network ===")

stream_network <- read_geopackage(
  "spatial/sarantaporos/sarantaporos_stream_network.gpkg",
  import_as = "data.table"
)

message(sprintf("Loaded stream network: %d segments", nrow(stream_network)))

# Extract unique subcatchment IDs
subc_ids <- unique(stream_network$subc_id)
subc_ids <- subc_ids[!is.na(subc_ids)]

message(sprintf("Unique subcatchments: %d", length(subc_ids)))

# ============================================================================
# DETERMINE TILE IDs
# ============================================================================

message("\n=== Determining Tile IDs ===")

# Option 1: Manually specify tiles (if you know them)
tile_id <- c("h18v04", "h20v04")

# Option 2: Auto-detect from coordinates (uncomment if needed)
# coords <- st_coordinates(st_centroid(st_as_sf(stream_network)))
# tile_id <- get_tile_id(data = as.data.frame(coords), lon = "X", lat = "Y")
# tile_id <- unique(tile_id$tile_id)

message(sprintf("Tiles needed: %s", paste(tile_id, collapse = ", ")))

# ============================================================================
# CREATE DIRECTORY STRUCTURE
# ============================================================================

message("\n=== Creating Directory Structure ===")

dir.create("env90m", showWarnings = FALSE, recursive = TRUE)

message("✓ Created directory: env90m/")

# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA TABLES
# ============================================================================

message("\n=== Downloading Environmental Data Tables ===")
message("This may take several minutes depending on data size...")
# message("Total expected download: ~ 72.3 GB")

# --------------------------------------------------------------------------
# 1. OBSERVED CLIMATE VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Climate Variables ---")

download_observed_climate_tables(
  subset = c("bio01_1981-2010_observed", "bio04_1981-2010_observed",
             "bio05_1981-2010_observed", "bio06_1981-2010_observed",
             "bio15_1981-2010_observed", "bio17_1981-2010_observed",
             "bio18_1981-2010_observed"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Climate tables downloaded to: env90m/chelsa_bioclim_v2_1/")

# Verify download
climate_file <- list.files(
  path = "env90m/chelsa_bioclim_v2_1/1981-2010_observed/bio01",
  pattern = ".txt$",
  full.names = TRUE
)[1]

if (!is.na(climate_file) && file.exists(climate_file)) {
  file_size <- file.info(climate_file)$size / 1024 / 1024
  message(sprintf("  Sample file: %s (%.2f MB)", basename(climate_file), file_size))
  message("\n  First 10 rows:")
  system(paste("head", climate_file))
} else {
  warning("Climate file not found! Download may have failed.")
}

# --------------------------------------------------------------------------
# 2. LAND COVER VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Land Cover Variables ---")

download_landcover_tables(
  base_vars = c("c10", "c20", "c30", "c40", "c50", "c60", "c120", "c130",
                "c150", "c160", "c180", "c190", "c200", "c210"),
  years = c("1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000",
            "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
            "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Land cover tables downloaded to: env90m/esa_cci_landcover_v2_1_1/")

# Verify download
landcover_file <- list.files(
  path = "env90m/esa_cci_landcover_v2_1_1/c60",
  pattern = ".txt$",
  full.names = TRUE
)[1]

if (!is.na(landcover_file) && file.exists(landcover_file)) {
  file_size <- file.info(landcover_file)$size / 1024 / 1024
  message(sprintf("  Sample file: %s (%.2f MB)", basename(landcover_file), file_size))
  message("\n  First 10 rows:")
  system(paste("head", landcover_file))
} else {
  warning("Land cover file not found! Download may have failed.")
}

# --------------------------------------------------------------------------
# 3. HYDROGRAPHY90M VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Hydrography Variables ---")

download_hydrography90m_tables(
  subset = c("order_strahler", "length", "cum_length", "gradient", "elev_drop",
             "accumulation", "channel_grad_dw_seg", "channel_grad_up_seg",
             "channel_elv_dw_seg", "channel_elv_up_seg", "connections",
             "stream_dist_dw_near", "stream_dist_up_near", "slope_grad_dw_cel"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Hydrography tables downloaded to: env90m/hydrography90m_v1_0/")

# --------------------------------------------------------------------------
# 4. ADDITIONAL VARIABLES (OPTIONAL - UNCOMMENT IF NEEDED)
# --------------------------------------------------------------------------

# Soil variables
# download_soil_tables(
#   subset = c("clyppt", "sltppt", "sndppt"),
#   tile_ids = tile_id,
#   download = TRUE,
#   download_dir = "env90m",
#   file_format = "txt",
#   delete_zips = TRUE,
#   quiet = FALSE
# )

# Projected climate (future scenarios)
# download_projected_climate_tables(
#   subset = c("bio01_2041-2070_ssp370", "bio01_2041-2070_ssp585"),
#   tile_ids = tile_id,
#   download = TRUE,
#   download_dir = "env90m",
#   file_format = "txt",
#   delete_zips = TRUE,
#   quiet = FALSE
# )

# ============================================================================
# CREATE SUBCATCHMENT ID FILE
# ============================================================================

message("\n=== Creating Subcatchment ID Reference File ===")

subc_ids_dt <- data.table(subc_id = subc_ids)
fwrite(subc_ids_dt,
       file = "env90m/subc_ids.txt",
       col.names = FALSE)

message(sprintf("✓ Saved %d subcatchment IDs to: env90m/subc_ids.txt",
                length(subc_ids)))

# ============================================================================
# VERIFICATION & SUMMARY
# ============================================================================

message("\n=== Download Verification ===")

n_climate  <- length(list.files("env90m/chelsa_bioclim_v2_1",      pattern = ".txt$", recursive = TRUE))
n_landcover <- length(list.files("env90m/esa_cci_landcover_v2_1_1", pattern = ".txt$", recursive = TRUE))
n_hydro    <- length(list.files("env90m/hydrography90m_v1_0",       pattern = ".txt$", recursive = TRUE))

message(sprintf("\nFiles downloaded:"))
message(sprintf("  Climate:    %d files", n_climate))
message(sprintf("  Land cover: %d files", n_landcover))
message(sprintf("  Hydrography:%d files", n_hydro))
message(sprintf("  Total:      %d files", n_climate + n_landcover + n_hydro))

total_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024

message(sprintf("\nTotal disk space used: %.2f GB", total_size))

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== DOWNLOAD COMPLETE ===")
message("========================================")

message("\nDirectory structure created:")
message("  env90m/")
message("  ├── chelsa_bioclim_v2_1/")
message("  ├── esa_cci_landcover_v2_1_1/")
message("  ├── hydrography90m_v1_0/")
message("  └── subc_ids.txt")

message("\nVariables downloaded:")
message("  Climate:     bio01, bio04, bio05, bio06, bio15, bio17, bio18 (1981-2010_observed)")
message("  Land cover:  c10, c20, c30, c40, c50, c60, c120, c130, c150, c160, c180, c190, c200, c210 (1992-2020)")
message("  Hydrography: order_strahler, length, cum_length, gradient, elev_drop, accumulation,")
message("               channel_grad_dw/up_seg, channel_elv_dw/up_seg, connections,")
message("               stream_dist_dw/up_near, slope_grad_dw_cel")

message("\nTiles downloaded:")
message(sprintf("  %s", paste(tile_id, collapse = ", ")))

message("\nNext steps:")
message("  1. Run script: 02_create_prediction_table.R")
message("  2. This will create the pred_tab.csv for SDM modeling")

message(sprintf("\nNOTE: Raw downloaded tables can be deleted after creating"))
message(sprintf("      the prediction table to save disk space (~%.1f GB)", total_size))

message("\n========================================\n")
# ============================================================================
# CREATE PREDICTION TABLE FOR SPECIES DISTRIBUTION MODELING
# ============================================================================
# Purpose: Create prediction table from downloaded Environment90m data
# Input: Downloaded environmental tables from script 01
# Output: pred_tab.csv - ready for SDM modeling
# ============================================================================
# Date: 2026-04-23
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# VERIFY INPUT FILES EXIST
# ============================================================================

message("\n=== Verifying Input Files ===")

if (!file.exists("env90m/subc_ids.txt")) {
  stop("ERROR: env90m/subc_ids.txt not found!",
       "\n  Please run script 01_download_environmental_variables.R first.")
}

required_dirs <- c(
  "env90m/chelsa_bioclim_v2_1",
  "env90m/esa_cci_landcover_v2_1_1",
  "env90m/hydrography90m_v1_0"
)

for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    stop("ERROR: Required directory not found: ", dir,
         "\n  Please run script 01_download_environmental_variables.R first.")
  }
}

message("✓ All required input files found")

# ============================================================================
# CONFIGURATION
# ============================================================================

message("\n=== Configuration ===")

# Define variables to include in prediction table
variables <- c(
  "bio01_1981-2010_observed", "bio04_1981-2010_observed", "bio05_1981-2010_observed",
  "bio06_1981-2010_observed", "bio15_1981-2010_observed", "bio17_1981-2010_observed",
  "bio18_1981-2010_observed",

  # Land cover 2020 (most recent; species observations up to 2024)
  "c10_2020", "c20_2020", "c30_2020", "c40_2020", "c50_2020", "c60_2020",
  "c120_2020", "c130_2020", "c150_2020", "c160_2020", "c180_2020",
  "c190_2020", "c200_2020", "c210_2020",

  "order_strahler", "length", "cum_length", "gradient", "elev_drop", "accumulation",
  "channel_grad_dw_seg", "channel_grad_up_seg", "channel_elv_dw_seg", "channel_elv_up_seg",
   "stream_dist_dw_near", "stream_dist_up_near", "slope_grad_dw_cel"
)

# Define statistics to calculate
statistics <- c("mean")

# Define tile IDs (same as in script 01)
tile_id <- c("h18v04", "h20v04")

# Detect available cores for parallel processing
n_cores <- parallel::detectCores() - 2

message(sprintf("Variables: %d total", length(variables)))
message(sprintf("Statistics: %s", paste(statistics, collapse = ", ")))
message(sprintf("Tiles: %s", paste(tile_id, collapse = ", ")))
message(sprintf("CPU cores to use: %d", n_cores))

# ============================================================================
# CREATE PREDICTION TABLE
# ============================================================================

message("\n=== Creating Prediction Table ===")
message("This may take several minutes depending on data size...")

# Define output path
output_file <- "env90m/pred_tab2.csv"

# Run get_predict_table
pred_tab <- get_predict_table(
  variable = variables,
  statistics = statistics,
  tile_id = tile_id,
  input_var_path = "env90m",
  subcatch_id = "env90m/subc_ids.txt",
  out_file_path = output_file,
  read = TRUE,
  quiet = FALSE,
  n_cores = n_cores,
  overwrite = TRUE
)

message(sprintf("\n✓ Prediction table created: %d rows, %d columns",
                nrow(pred_tab),
                ncol(pred_tab)))

# ============================================================================
# HANDLE MISSING DATA (IF ANY)
# ============================================================================

message("\n=== Checking for Missing Data ===")

missing_counts <- pred_tab[, lapply(.SD, function(x) sum(is.na(x)))]
has_missing <- any(missing_counts > 0)

if (has_missing) {
  message("\n⚠ Missing values detected:")

  for (col in names(missing_counts)) {
    if (col != "subc_id") {
      n_missing <- missing_counts[[col]]
      if (n_missing > 0) {
        pct_missing <- 100 * n_missing / nrow(pred_tab)
        message(sprintf("  %s: %d (%.1f%%)", col, n_missing, pct_missing))
      }
    }
  }

  message("\nOptions for handling missing data:")
  message("  1. Keep all rows (NAs will need to be handled in modeling)")
  message("  2. Remove rows with any missing values")
  message("  3. Remove only specific problematic variables")

  message("\nSaving two versions:")

  fwrite(pred_tab, "env90m/pred_tab_full.csv")
  message("  ✓ Saved: env90m/pred_tab_full.csv (all rows, including NAs)")

  pred_tab_complete <- na.omit(pred_tab)
  fwrite(pred_tab_complete, "env90m/pred_tab_complete.csv")
  message(sprintf("  ✓ Saved: env90m/pred_tab_complete.csv (%d rows, NAs removed)",
                  nrow(pred_tab_complete)))

  message(sprintf("\nRemoved %d rows (%.1f%%) with missing values",
                  nrow(pred_tab) - nrow(pred_tab_complete),
                  100 * (nrow(pred_tab) - nrow(pred_tab_complete)) / nrow(pred_tab)))

  pred_tab <- pred_tab_complete

} else {
  message("✓ No missing values detected")
}

# ============================================================================
# DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Summary ===")

message(sprintf("\nDimensions: %d rows × %d columns", nrow(pred_tab), ncol(pred_tab)))

message("\nColumn names:")
print(names(pred_tab))

message("\nBasic statistics:")
print(summary(pred_tab))

message("\nChecking for extreme values...")
for (col in names(pred_tab)) {
  if (col != "subc_id" && is.numeric(pred_tab[[col]])) {
    q <- quantile(pred_tab[[col]], probs = c(0.01, 0.99), na.rm = TRUE)
    n_outliers <- sum(pred_tab[[col]] < q[1] | pred_tab[[col]] > q[2], na.rm = TRUE)
    if (n_outliers > 0) {
      pct_outliers <- 100 * n_outliers / nrow(pred_tab)
      message(sprintf("  %s: %d values (%.1f%%) outside 1st-99th percentile",
                      col, n_outliers, pct_outliers))
    }
  }
}

# ============================================================================
# OPTIONAL: CLEAN UP DOWNLOADED TABLES
# ============================================================================

message("\n=== Disk Space Management ===")

downloaded_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024

pred_tab_size <- file.info("env90m/pred_tab.csv")$size / 1024 / 1024

message(sprintf("Downloaded tables: %.2f GB", downloaded_size))
message(sprintf("Prediction table: %.2f MB", pred_tab_size))
message(sprintf("Space savings if deleted: %.2f GB", downloaded_size))

message("\nTo free up disk space, you can delete the downloaded tables:")
message("  env90m/chelsa_bioclim_v2_1/")
message("  env90m/esa_cci_landcover_v2_1_1/")
message("  env90m/hydrography90m_v1_0/")

# Uncomment to automatically delete
# unlink("env90m/chelsa_bioclim_v2_1", recursive = TRUE)
# unlink("env90m/esa_cci_landcover_v2_1_1", recursive = TRUE)
# unlink("env90m/hydrography90m_v1_0", recursive = TRUE)
# message("\n✓ Deleted downloaded tables")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== PREDICTION TABLE CREATION COMPLETE ===")
message("========================================")

message("\nFiles created:")
message("  ✓ env90m/pred_tab.csv (main file)")
if (has_missing) {
  message("  ✓ env90m/pred_tab_full.csv (with NAs)")
  message("  ✓ env90m/pred_tab_complete.csv (NAs removed)")
}

message("\nPrediction table summary:")
message(sprintf("  Rows:      %d", nrow(pred_tab)))
message(sprintf("  Columns:   %d", ncol(pred_tab)))
message(sprintf("  Variables: %d", length(variables)))
message(sprintf("  File size: %.2f MB", pred_tab_size))

message("\nNext steps:")
message("  1. Load pred_tab.csv in your SDM workflow")
message("  2. Join with species occurrence data")
message("  3. Run species distribution models")
message("  4. Predict habitat suitability")

message("\nExample usage:")
message("  pred_tab <- fread('env90m/pred_tab.csv')")

message("\n========================================\n")
# =============================================================================
# Quality check of the prediction table
# =============================================================================

# Inspect variable names
names(predict_table)

# Check number of sub-catchments matches expected
ifelse(
  nrow(predict_table) == nrow(subc_ids),
  "The number of rows in the prediction table matches the number of sub-catchments.",
  "Mismatch detected: the number of sub-catchments and prediction table rows differ."
)

# Check for missing values in each column
colSums(is.na(predict_table))

# Simplify climate variable names for readability
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))

# =============================================================================
# Collinearity assessment
# =============================================================================

# Drop near-zero variance land cover classes not expected in Greek freshwater
# subcatchments (mangroves, moss/lichen, sparse vegetation, water bodies as cover)
lc_drop <- c("c130_y2020", "c150_y2020", "c190_y2020", "c200_y2020")

predict_table <- predict_table %>%
  dplyr::select(-any_of(lc_drop))

# Select numeric predictor variables (mean values only)
# - ends_with("_mean"): climate and hydrography aggregated means
# - ends_with("2020"): land cover proportional cover per subcatchment
numeric_vars <- predict_table %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(ends_with("_mean") | ends_with("2020"))

# Compute and visualize pairwise correlation matrix
# High correlations (|r| > 0.7) indicate potential redundancy
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "lower",
         tl.cex = 0.6, diag = FALSE,
         col = COL2("RdBu", 200))  # diverging palette: blue = negative, red = positive

# -----------------------------------------------------------------------------
# VIF-based variable selection
# Sequentially removes the variable with the highest VIF until all
# remaining predictors fall below the threshold (th = 10)
# -----------------------------------------------------------------------------
vif_results <- vifstep(numeric_vars, th = 10)

# Inspect retained variables and their final VIF values
vif_results@results

selected_vars <- vif_results@results$Variables
selected_vars

# =============================================================================
# Subset prediction table to non-collinear predictors
# =============================================================================
predict_table <- predict_table %>%
  dplyr::select(any_of(c("subcID", selected_vars)))

# Final check
names(predict_table)
# =============================================================================
# Reconstruct true absences and check species overlap across basins
# =============================================================================

library(dplyr)
library(tidyr)

# --- Load files ---
occurr      <- read.csv("points_snapped/fish/occurr_for_sdm.csv")
sites       <- read.csv("points_snapped/all_snapped_with_basins.csv") %>%
  filter(source %in% c("HCMR", "GBIF")) %>%
  filter(subc_id %in% sites$subc_id) %>%
  select(subc_id, basin_id) %>% distinct()

# --- Join basin_id to occurrences via subc_id ---
occurr <- occurr %>%
  left_join(sites, by = "subc_id")

# Sanity check: any occurrences without a basin_id after join?
cat("Occurrences missing basin_id:", sum(is.na(occurr$basin_id)), "\n")

# =============================================================================
# Reconstruct true absences for HCMR sites
# Logic: for every HCMR site, any species NOT recorded there is truly absent
# This does NOT apply to GBIF records
# =============================================================================

# All HCMR sites with their basin
hcmr_sites <- occurr %>%
  filter(source == "HCMR") %>%
  select(site_id, subc_id, basin_id) %>%
  distinct()

# All species recorded at least once in HCMR data
hcmr_species <- occurr %>%
  filter(source == "HCMR") %>%
  distinct(species)

# Full crossed table: every HCMR site x every species
hcmr_full <- hcmr_sites %>%
  cross_join(hcmr_species) %>%
  # join back presences
  left_join(
    occurr %>% filter(source == "HCMR") %>% select(site_id, species) %>% mutate(presence = 1),
    by = c("site_id", "species")
  ) %>%
  mutate(presence = replace_na(presence, 0))

# Quick check
cat("Total HCMR site x species combinations:", nrow(hcmr_full), "\n")
cat("  Presences:", sum(hcmr_full$presence == 1), "\n")
cat("  True absences:", sum(hcmr_full$presence == 0), "\n")

# =============================================================================
# Species overlap check
# =============================================================================

sarantaporos_basin_id <- 1292502

# Species present in Sarantaporos (HCMR presences only)
species_sarant <- hcmr_full %>%
  filter(basin_id == sarantaporos_basin_id, presence == 1) %>%
  distinct(species) %>%
  pull(species)

# Species present in other Greek basins (HCMR presences only)
species_greece_other <- hcmr_full %>%
  filter(basin_id != sarantaporos_basin_id, presence == 1) %>%
  distinct(species) %>%
  pull(species)

# Species in GBIF
species_gbif <- occurr %>%
  filter(source == "GBIF") %>%
  distinct(species) %>%
  pull(species)

# --- Summary counts ---
cat("\n--- Species overlap summary ---\n")
cat("Species with HCMR presences in Sarantaporos:      ", length(species_sarant), "\n")
cat("Species with HCMR presences elsewhere in Greece:  ", length(species_greece_other), "\n")
cat("Species in GBIF:                                   ", length(species_gbif), "\n")
cat("Overlap Sarantaporos + Greece-wide HCMR:           ",
    length(intersect(species_sarant, species_greece_other)), "\n")
cat("Only in Sarantaporos (no HCMR elsewhere):          ",
    length(setdiff(species_sarant, species_greece_other)), "\n")

# --- Per-species breakdown table ---
all_species <- union(species_sarant, species_greece_other)

species_summary <- tibble(species = all_species) %>%
  mutate(
    in_sarantaporos    = species %in% species_sarant,
    in_greece_hcmr     = species %in% species_greece_other,
    in_gbif            = species %in% species_gbif,
    # true absences available in Sarantaporos for this species?
    true_abs_sarant    = species %in% (hcmr_full %>%
                                         filter(basin_id == sarantaporos_basin_id, presence == 0) %>%
                                         pull(species)),
    data_situation = case_when(
      in_sarantaporos & in_greece_hcmr  ~ "HCMR presences in both — true absences usable range-wide",
      in_sarantaporos & !in_greece_hcmr & in_gbif  ~ "Sarantaporos HCMR + GBIF only — true absences range-restricted",
      in_sarantaporos & !in_greece_hcmr & !in_gbif ~ "Sarantaporos HCMR only — consider basin-level model only",
      !in_sarantaporos & in_greece_hcmr ~ "Greece HCMR only — no Sarantaporos true absences",
      TRUE ~ "check"
    )
  )

print(species_summary, n = Inf)

# Count per situation — this is your decision table
species_summary %>% count(data_situation) %>% arrange(desc(n))
# ============================================================================
# CREATE PREDICTION TABLE FOR SPECIES DISTRIBUTION MODELING
# ============================================================================
# Purpose: Create prediction table from downloaded Environment90m data
# Input: Downloaded environmental tables from script 01
# Output: pred_tab.csv - ready for SDM modeling
# ============================================================================
# Date: 2026-02-05
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

# Set working directory
wdir <- "C:/Users/labbadi/OneDrive/Artigo_Peixes" # Created working directory
setwd(wdir)

# ============================================================================
# VERIFY INPUT FILES EXIST
# ============================================================================

message("\n=== Verifying Input Files ===")

# Check if subc_ids file exists
if (!file.exists("env90m/subc_ids.txt")) {
  stop("ERROR: env90m/subc_ids.txt not found!",
       "\n  Please run script 01_download_env90m_data.R first.")
}

# Check if downloaded data directories exist
required_dirs <- c(
  "env90m/chelsa_bioclim_v2_1",
  "env90m/esa_cci_landcover_v2_1_1",
  "env90m/hydrography90m_v1_0"
)

for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    stop("ERROR: Required directory not found: ", dir,
         "\n  Please run script 01_download_env90m_data.R first.")
  }
}

message("✓ All required input files found")

# ============================================================================
# CONFIGURATION
# ============================================================================

message("\n=== Configuration ===")

# Define variables to include in prediction table
variables <- c(
  "bio01_1981-2010_observed","bio04_1981-2010_observed","bio05_1981-2010_observed","bio06_1981-2010_observed",
  "bio15_1981-2010_observed","bio17_1981-2010_observed","bio18_1981-2010_observed",
  
  # c10
  "c10_1992","c10_1993","c10_1994","c10_1995","c10_1996","c10_1997","c10_1998","c10_1999","c10_2000","c10_2001",
  "c10_2002","c10_2003","c10_2004","c10_2005","c10_2006","c10_2007","c10_2008","c10_2009","c10_2010","c10_2011",
  "c10_2012","c10_2013","c10_2014","c10_2015","c10_2016","c10_2017","c10_2018","c10_2019","c10_2020",
  
  # c20
  "c20_1992","c20_1993","c20_1994","c20_1995","c20_1996","c20_1997","c20_1998","c20_1999","c20_2000","c20_2001",
  "c20_2002","c20_2003","c20_2004","c20_2005","c20_2006","c20_2007","c20_2008","c20_2009","c20_2010","c20_2011",
  "c20_2012","c20_2013","c20_2014","c20_2015","c20_2016","c20_2017","c20_2018","c20_2019","c20_2020",
  
  # c30
  "c30_1992","c30_1993","c30_1994","c30_1995","c30_1996","c30_1997","c30_1998","c30_1999","c30_2000","c30_2001",
  "c30_2002","c30_2003","c30_2004","c30_2005","c30_2006","c30_2007","c30_2008","c30_2009","c30_2010","c30_2011",
  "c30_2012","c30_2013","c30_2014","c30_2015","c30_2016","c30_2017","c30_2018","c30_2019","c30_2020",
  
  # c40
  "c40_1992","c40_1993","c40_1994","c40_1995","c40_1996","c40_1997","c40_1998","c40_1999","c40_2000","c40_2001",
  "c40_2002","c40_2003","c40_2004","c40_2005","c40_2006","c40_2007","c40_2008","c40_2009","c40_2010","c40_2011",
  "c40_2012","c40_2013","c40_2014","c40_2015","c40_2016","c40_2017","c40_2018","c40_2019","c40_2020",
  
  # c50
  "c50_1992","c50_1993","c50_1994","c50_1995","c50_1996","c50_1997","c50_1998","c50_1999","c50_2000","c50_2001",
  "c50_2002","c50_2003","c50_2004","c50_2005","c50_2006","c50_2007","c50_2008","c50_2009","c50_2010","c50_2011",
  "c50_2012","c50_2013","c50_2014","c50_2015","c50_2016","c50_2017","c50_2018","c50_2019","c50_2020",
  
  # c60
  "c60_1992","c60_1993","c60_1994","c60_1995","c60_1996","c60_1997","c60_1998","c60_1999","c60_2000","c60_2001",
  "c60_2002","c60_2003","c60_2004","c60_2005","c60_2006","c60_2007","c60_2008","c60_2009","c60_2010","c60_2011",
  "c60_2012","c60_2013","c60_2014","c60_2015","c60_2016","c60_2017","c60_2018","c60_2019","c60_2020",
  
  # c120
  "c120_1992","c120_1993","c120_1994","c120_1995","c120_1996","c120_1997","c120_1998","c120_1999","c120_2000","c120_2001",
  "c120_2002","c120_2003","c120_2004","c120_2005","c120_2006","c120_2007","c120_2008","c120_2009","c120_2010","c120_2011",
  "c120_2012","c120_2013","c120_2014","c120_2015","c120_2016","c120_2017","c120_2018","c120_2019","c120_2020",
  
  # c130
  "c130_1992","c130_1993","c130_1994","c130_1995","c130_1996","c130_1997","c130_1998","c130_1999","c130_2000","c130_2001",
  "c130_2002","c130_2003","c130_2004","c130_2005","c130_2006","c130_2007","c130_2008","c130_2009","c130_2010","c130_2011",
  "c130_2012","c130_2013","c130_2014","c130_2015","c130_2016","c130_2017","c130_2018","c130_2019","c130_2020",
  
  # c150
  "c150_1992","c150_1993","c150_1994","c150_1995","c150_1996","c150_1997","c150_1998","c150_1999","c150_2000","c150_2001",
  "c150_2002","c150_2003","c150_2004","c150_2005","c150_2006","c150_2007","c150_2008","c150_2009","c150_2010","c150_2011",
  "c150_2012","c150_2013","c150_2014","c150_2015","c150_2016","c150_2017","c150_2018","c150_2019","c150_2020",
  
  # c160
  "c160_1992","c160_1993","c160_1994","c160_1995","c160_1996","c160_1997","c160_1998","c160_1999","c160_2000","c160_2001",
  "c160_2002","c160_2003","c160_2004","c160_2005","c160_2006","c160_2007","c160_2008","c160_2009","c160_2010","c160_2011",
  "c160_2012","c160_2013","c160_2014","c160_2015","c160_2016","c160_2017","c160_2018","c160_2019","c160_2020",
  
  # c180
  "c180_1992","c180_1993","c180_1994","c180_1995","c180_1996","c180_1997","c180_1998","c180_1999","c180_2000","c180_2001",
  "c180_2002","c180_2003","c180_2004","c180_2005","c180_2006","c180_2007","c180_2008","c180_2009","c180_2010","c180_2011",
  "c180_2012","c180_2013","c180_2014","c180_2015","c180_2016","c180_2017","c180_2018","c180_2019","c180_2020",
  
  # c190
  "c190_1992","c190_1993","c190_1994","c190_1995","c190_1996","c190_1997","c190_1998","c190_1999","c190_2000","c190_2001",
  "c190_2002","c190_2003","c190_2004","c190_2005","c190_2006","c190_2007","c190_2008","c190_2009","c190_2010","c190_2011",
  "c190_2012","c190_2013","c190_2014","c190_2015","c190_2016","c190_2017","c190_2018","c190_2019","c190_2020",
  
  # c200
  "c200_1992","c200_1993","c200_1994","c200_1995","c200_1996","c200_1997","c200_1998","c200_1999","c200_2000","c200_2001",
  "c200_2002","c200_2003","c200_2004","c200_2005","c200_2006","c200_2007","c200_2008","c200_2009","c200_2010","c200_2011",
  "c200_2012","c200_2013","c200_2014","c200_2015","c200_2016","c200_2017","c200_2018","c200_2019","c200_2020",
  
  # c210
  "c210_1992","c210_1993","c210_1994","c210_1995","c210_1996","c210_1997","c210_1998","c210_1999","c210_2000","c210_2001",
  "c210_2002","c210_2003","c210_2004","c210_2005","c210_2006","c210_2007","c210_2008","c210_2009","c210_2010","c210_2011",
  "c210_2012","c210_2013","c210_2014","c210_2015","c210_2016","c210_2017","c210_2018","c210_2019","c210_2020",
  
  "order_strahler","length","cum_length","gradient","elev_drop","accumulation",
  "channel_grad_dw_seg","channel_grad_up_seg","channel_elv_dw_seg","channel_elv_up_seg",
  "connections","stream_dist_dw_near","stream_dist_up_near","slope_grad_dw_cel")

# Define statistics to calculate
# Options: "mean"
statistics <- c("mean")

# Define tile IDs (same as in script 01)
tile_id <- c("h18v04", "h20v04")

# Detect available cores for parallel processing
n_cores <- parallel::detectCores() - 6

message(sprintf("Variables: %s", paste(variables, collapse = ", ")))
message(sprintf("Statistics: %s", paste(statistics, collapse = ", ")))
message(sprintf("Tiles: %s", paste(tile_id, collapse = ", ")))
message(sprintf("CPU cores to use: %d", n_cores))

# ============================================================================
# CREATE PREDICTION TABLE
# ============================================================================

message("\n=== Creating Prediction Table ===")
message("This may take several minutes depending on data size...")

# Define path
output_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m/pred_tab.csv"
input_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m"
subcatch_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m/subc_ids.txt"


# Run get_predict_table
pred_tab <- get_predict_table(
  variable = variables,
  statistics = statistics,
  tile_id = tile_id,
  input_var_path = input_file,
  subcatch_id = subcatch_file,
  out_file_path = output_file,
  read = TRUE,
  quiet = FALSE,
  n_cores = n_cores,
  overwrite = TRUE
)

message(sprintf("\n✓ Prediction table created: %d rows, %d columns",
                nrow(pred_tab),
                ncol(pred_tab)))


# ============================================================================
# HANDLE MISSING DATA (IF ANY)
# ============================================================================

message("\n=== Checking for Missing Data ===")

# Count missing values per column
missing_counts <- pred_tab[, lapply(.SD, function(x) sum(is.na(x)))]

# Check if there are any missing values
has_missing <- any(missing_counts > 0)

if (has_missing) {
  message("\n⚠ Missing values detected:")
  
  for (col in names(missing_counts)) {
    if (col != "subc_id") {
      n_missing <- missing_counts[[col]]
      if (n_missing > 0) {
        pct_missing <- 100 * n_missing / nrow(pred_tab)
        message(sprintf("  %s: %d (%.1f%%)", col, n_missing, pct_missing))
      }
    }
  }
  
  # Ask user what to do
  message("\nOptions for handling missing data:")
  message("  1. Keep all rows (NAs will need to be handled in modeling)")
  message("  2. Remove rows with any missing values")
  message("  3. Remove only specific problematic variables")
  
  # For automation, we'll save both versions
  message("\nSaving two versions:")
  
  # Version 1: All data (with NAs)
  fwrite(pred_tab, "env90m/pred_tab_full.csv")
  message("  ✓ Saved: env90m/pred_tab_full.csv (all rows, including NAs)")
  
  # Version 2: Complete cases only
  pred_tab_complete <- na.omit(pred_tab)
  fwrite(pred_tab_complete, "env90m/pred_tab_complete.csv")
  message(sprintf("  ✓ Saved: env90m/pred_tab_complete.csv (%d rows, NAs removed)",
                  nrow(pred_tab_complete)))
  
  message(sprintf("\nRemoved %d rows (%.1f%%) with missing values",
                  nrow(pred_tab) - nrow(pred_tab_complete),
                  100 * (nrow(pred_tab) - nrow(pred_tab_complete)) / nrow(pred_tab)))
  
  # Use complete version for subsequent analyses
  pred_tab <- pred_tab_complete
  
} else {
  message("✓ No missing values detected")
}

# ============================================================================
# DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Summary ===")

# Dimensions
message(sprintf("\nDimensions: %d rows × %d columns", nrow(pred_tab), ncol(pred_tab)))

# Column names
message("\nColumn names:")
print(names(pred_tab))

# Basic statistics
message("\nBasic statistics:")
print(summary(pred_tab))

# Check for outliers
message("\nChecking for extreme values...")
for (col in names(pred_tab)) {
  if (col != "subc_id" && is.numeric(pred_tab[[col]])) {
    q <- quantile(pred_tab[[col]], probs = c(0.01, 0.99), na.rm = TRUE)
    n_outliers <- sum(pred_tab[[col]] < q[1] | pred_tab[[col]] > q[2], na.rm = TRUE)
    if (n_outliers > 0) {
      pct_outliers <- 100 * n_outliers / nrow(pred_tab)
      message(sprintf("  %s: %d values (%.1f%%) outside 1st-99th percentile",
                      col, n_outliers, pct_outliers))
    }
  }
}


# ============================================================================
# OPTIONAL: CLEAN UP DOWNLOADED TABLES
# ============================================================================

message("\n=== Disk Space Management ===")

# Calculate size of downloaded tables
downloaded_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024  # GB

# Calculate size of prediction table
pred_tab_size <- file.info("env90m/pred_tab.csv")$size / 1024 / 1024  # MB

message(sprintf("Downloaded tables: %.2f GB", downloaded_size))
message(sprintf("Prediction table: %.2f MB", pred_tab_size))
message(sprintf("Space savings if deleted: %.2f GB", downloaded_size))

message("\nTo free up disk space, you can delete the downloaded tables:")
message("  env90m/chelsa_bioclim_v2_1/")
message("  env90m/esa_cci_landcover_v2_1_1/")
message("  env90m/hydrography90m_v1_0/")

message("\nUncomment the following lines to delete:")
message("# unlink('env90m/chelsa_bioclim_v2_1', recursive = TRUE)")
message("# unlink('env90m/esa_cci_landcover_v2_1_1', recursive = TRUE)")
message("# unlink('env90m/hydrography90m_v1_0', recursive = TRUE)")

# Uncomment to automatically delete
# unlink("env90m/chelsa_bioclim_v2_1", recursive = TRUE)
# unlink("env90m/esa_cci_landcover_v2_1_1", recursive = TRUE)
# unlink("env90m/hydrography90m_v1_0", recursive = TRUE)
# message("\n✓ Deleted downloaded tables")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== PREDICTION TABLE CREATION COMPLETE ===")
message("========================================")

message("\nFiles created:")
message("  ✓ env90m/pred_tab.csv (main file)")
if (has_missing) {
  message("  ✓ env90m/pred_tab_full.csv (with NAs)")
  message("  ✓ env90m/pred_tab_complete.csv (NAs removed)")
}
message("  ✓ spatial/stream_networks/stream_network_with_env.csv")

message("\nPrediction table summary:")
message(sprintf("  Rows: %d", nrow(pred_tab)))
message(sprintf("  Columns: %d", ncol(pred_tab)))
message(sprintf("  Variables: %d", length(variables)))
message(sprintf("  File size: %.2f MB", pred_tab_size))

message("\nVariables included:")
for (var in variables) {
  var_cols <- grep(var, names(pred_tab), value = TRUE)
  message(sprintf("  %s: %s", var, paste(var_cols, collapse = ", ")))
}

message("\nNext steps:")
message("  1. Load pred_tab.csv in your SDM workflow")
message("  2. Join with species occurrence data")
message("  3. Run species distribution models")
message("  4. Predict habitat suitability")

message("\nExample usage:")
message("  pred_tab <- fread('env90m/pred_tab.csv')")
message("  # Use pred_tab for modeling...")

message("\n========================================\n")

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
# ============================================================================
# CREATE VISUALIZATION MAPS
# ============================================================================
# Purpose: Generate interactive maps of snapped points and networks
# Input: Results from previous steps
# Output: Interactive HTML maps
# ============================================================================

library(leaflet)
library(sf)
library(data.table)
library(htmlwidgets)

# Load data
all_snapped <- fread("../results/all_snapped_with_basins.csv")
all_streams <- st_read("../results/all_stream_networks.gpkg")

# Convert to sf
points_sf <- st_as_sf(
  all_snapped,
  coords = c("longitude_snapped", "latitude_snapped"),
  crs = 4326
)

# ============================================================================
# MAP 1: All Snapped Points by Source
# ============================================================================

message("Creating map 1: Snapped points...")

map1 <- leaflet(points_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~ifelse(source == "HCMR", "red", "blue"),
    radius = 3,
    fillOpacity = 0.7,
    popup = ~paste0(
      "Source: ", source, "<br>",
      "Site: ", site_id, "<br>",
      "Basin: ", basin_id, "<br>",
      "Snap distance: ", round(distance_metres, 1), "m"
    )
  ) %>%
  addLegend(
    colors = c("red", "blue"),
    labels = c("HCMR", "GBIF"),
    title = "Data Source"
  )

saveWidget(map1, "../results/maps/map_all_points.html", selfcontained = TRUE)

# ============================================================================
# MAP 2: Stream Networks with Points
# ============================================================================

message("Creating map 2: Streams and points...")

map2 <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = all_streams, color = "blue", weight = 1, opacity = 0.6) %>%
  addCircleMarkers(
    data = points_sf, 
    color = ~ifelse(source == "HCMR", "red", "orange"),
    radius = 4,
    fillOpacity = 0.8
  )

saveWidget(map2, "../results/maps/map_streams_and_points.html", selfcontained = TRUE)

message("\n=== Maps created in results/maps/ ===")

BASE_DIR   <- Sys.getenv("WORKFLOW_DATA", "/home/grigoropoulou/Documents/Postdoc/projects/workflow_paper/data")
NIMBUS_DIR <- Sys.getenv("NIMBUS_PATH", "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# pci_sparse.R
#
# Sparse implementation of the Population Connectivity Index (PCI)
# from riverconn::index_calculation (Baldan et al. 2022)
#
# Computes connectivity matrices only for occupied nodes (weight > 0),
# reducing memory from O(N²) to O(k²) where k = number of occupied
# subcatchments and N = total subcatchments in the basin.
# Produces identical results to riverconn::index_calculation.
#
# Validated against riverconn v0.3.31 on basins with up to ~800 nodes
# (see supplementary material).
#
# Limitations vs riverconn::index_calculation:
#   - Exponential dispersal kernel only (no threshold/leptokurtic)
#   - Symmetric directionality only (dir_fragmentation_type = "symmetric",
#     dir_distance_type = "symmetric")
#   - c_ij and B_ij always included (no flags to disable)
#   - index_type "sum" (CAFI) not implemented
#
# References:
#   Baldan, D., Cunillera-Montcusí, D., Funk, A., & Hein, T. (2022).
#     Introducing 'riverconn': an R package to assess river connectivity
#     indices. Environmental Modelling & Software, 156, 105470.
#
#   Jumani, S. et al. (2020). River fragmentation and flow alteration
#     metrics: a review. Environmental Research Letters, 15(12), 123009.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

pci_sparse <- function(graph,
                       weight = "weight",
                       field_B = "length_reach",
                       param = 0.5,
                       index_type = "full",
                       index_mode = "to",
                       pass_u = "pass_u",
                       pass_d = "pass_d") {

  # ----------------------------------------------------------
  # Input validation
  # ----------------------------------------------------------
  if (!igraph::is_igraph(graph))
    stop("'graph' must be an igraph object")
  if (!(index_type %in% c("full", "reach")))
    stop("'index_type' must be 'full' or 'reach'")
  if (index_type == "reach" && !(index_mode %in% c("from", "to")))
    stop("'index_mode' must be 'from' or 'to'")
  if (!(weight %in% igraph::vertex_attr_names(graph)))
    stop("'weight' must be a vertex attribute in 'graph'")
  if (!(field_B %in% igraph::vertex_attr_names(graph)))
    stop("'field_B' must be a vertex attribute in 'graph'")
  if (!(pass_u %in% igraph::edge_attr_names(graph)))
    stop("'pass_u' must be an edge attribute in 'graph'")
  if (!(pass_d %in% igraph::edge_attr_names(graph)))
    stop("'pass_d' must be an edge attribute in 'graph'")

  # ----------------------------------------------------------
  # Extract vertex and edge attributes
  # ----------------------------------------------------------
  v_weights <- igraph::vertex_attr(graph, weight)   # binary presence (0/1)
  v_lengths <- igraph::vertex_attr(graph, field_B)   # reach lengths (for distance)
  n_nodes   <- igraph::vcount(graph)

  presence_idx <- which(v_weights > 0)
  n_presence   <- length(presence_idx)

  # ----------------------------------------------------------
  # Early exit if fewer than 2 occupied nodes
  # ----------------------------------------------------------
  if (n_presence < 2) {
    if (index_type == "full") {
      return(data.frame(num = 0, den = 0, index = 0))
    } else {
      return(data.frame(
        name  = igraph::V(graph)$name,
        num   = 0,
        den   = 0,
        index = 0
      ))
    }
  }

  # ----------------------------------------------------------
  # Get edge passabilities
  # ----------------------------------------------------------
  edge_pass_u <- igraph::edge_attr(graph, pass_u)
  edge_pass_d <- igraph::edge_attr(graph, pass_d)

  # ----------------------------------------------------------
  # Build sparse c_ij and B_ij matrices (presence × presence)
  #
  # c_ij: barrier passability along path
  #   For each edge on the path: pass = pass_u * pass_d (symmetric)
  #   c_ij = product of pass along all edges on the path
  #
  # B_ij: dispersal probability based on distance
  #   distance_ij = sum of field_B for all vertices on the path
  #   B_ij = param ^ distance_ij (exponential kernel)
  # ----------------------------------------------------------

  c_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)
  B_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)

  for (i in 1:n_presence) {
    origin_node <- presence_idx[i]

    paths_result <- igraph::shortest_paths(
      graph,
      from   = origin_node,
      to     = presence_idx,
      mode   = "all",
      output = "both"
    )

    for (j in 1:n_presence) {
      if (i == j) next

      path_edges <- paths_result$epath[[j]]
      path_verts <- paths_result$vpath[[j]]

      if (length(path_edges) == 0) {
        c_ij_sparse[i, j] <- 0
        B_ij_sparse[i, j] <- 0
      } else {
        # c_ij: product of symmetric passability along path
        c_ij_sparse[i, j] <- prod(
          edge_pass_u[path_edges] * edge_pass_d[path_edges]
        )

        # B_ij: exponential decay with distance
        path_node_ids <- as.integer(path_verts)
        distance_ij   <- sum(v_lengths[path_node_ids])
        B_ij_sparse[i, j] <- param ^ distance_ij
      }
    }

    rm(paths_result)
  }

  gc(verbose = FALSE)

  # ----------------------------------------------------------
  # Combine: agg_mat = c_ij * B_ij (element-wise)
  # ----------------------------------------------------------
  agg_mat <- c_ij_sparse * B_ij_sparse

  # ----------------------------------------------------------
  # Calculate PCI using binary weights
  # ----------------------------------------------------------
  w_presence <- v_weights[presence_idx]

  if (index_type == "full") {
    # Catchment-level PCI: w' * agg_mat * w / (sum(w))^2
    index_num <- as.numeric(t(w_presence) %*% agg_mat %*% w_presence)
    index_den <- sum(w_presence)^2
    index_val <- index_num / index_den

    result <- data.frame(
      num   = index_num,
      den   = index_den,
      index = index_val
    )

  } else if (index_type == "reach") {
    # Reach-level PCI: per-node connectivity score
    if (index_mode == "to") {
      index_num_sparse <- agg_mat %*% w_presence
    } else {
      index_num_sparse <- t(t(w_presence) %*% agg_mat)
    }

    index_den <- sum(w_presence)

    # Expand to full node list (zeros for unoccupied nodes)
    index_num_full <- numeric(n_nodes)
    index_num_full[presence_idx] <- as.numeric(index_num_sparse)

    index_full <- index_num_full / index_den

    result <- data.frame(
      name  = igraph::V(graph)$name,
      num   = index_num_full,
      den   = index_den,
      index = index_full
    )
  }

  return(result)
}
save_to_nimbus <- function(data, filename, save_function = NULL, ...) {
  # Create local temp path
  local_path <- file.path(tempdir(), basename(filename))

  # Save locally first
  if (is.null(save_function)) {
    # Auto-detect save function based on file extension
    ext <- tools::file_ext(filename)
    if (ext == "html") {
      # HTML widget
      htmlwidgets::saveWidget(data, local_path, selfcontained = TRUE)
    } else if (ext %in% c("gpkg", "shp", "geojson")) {
      # Spatial data
      # For GPKG, use layer name from filename if not provided
      if (ext == "gpkg" && !"layer" %in% names(list(...))) {
        layer_name <- tools::file_path_sans_ext(basename(filename))
        sf::st_write(data, local_path, layer = layer_name, delete_dsn = TRUE, quiet = TRUE, ...)
      } else {
        sf::st_write(data, local_path, delete_dsn = TRUE, quiet = TRUE, ...)
      }
    } else if (ext == "csv") {
      # CSV
      data.table::fwrite(data, local_path, ...)
    } else if (ext == "rds") {
      # RDS
      saveRDS(data, local_path, ...)
    } else {
      stop("Unknown file type. Please provide save_function argument.")
    }
  } else {
    # Use custom save function
    save_function(data, local_path, ...)
  }

  # Verify local file was created and has size
  if (!file.exists(local_path)) {
    stop("Local file was not created: ", local_path)
  }
  local_size <- file.info(local_path)$size
  if (local_size == 0) {
    stop("Local file has 0 bytes: ", local_path)
  }

  # Copy to Nimbus
  nimbus_dest <- filename

  # Create directory if needed
  dir.create(dirname(nimbus_dest), recursive = TRUE, showWarnings = FALSE)

  # Use system cp command for WebDAV mounts
  copy_result <- system2("cp", args = c(shQuote(local_path), shQuote(nimbus_dest)))
  if (copy_result != 0) {
    stop("Failed to copy file to Nimbus: ", nimbus_dest)
  }

  # Clean up
  unlink(local_path)

  message(sprintf("✓ Saved to Nimbus: %s", filename))
  invisible(nimbus_dest)
}
