#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_download_gbif_fish.R   (Module 1 -- Biodiversity Data)
#
# Download freshwater fish occurrence records from GBIF for the Vjosa/Aoos
# basin. The target taxa are the species found in the cleaned HCMR dataset
# (Module 1, script 01); GBIF records supplement the survey data with
# additional occurrences across the basin.
#
# Downloads are submitted asynchronously (one occ_download() per taxon),
# then collected once GBIF has prepared each archive. A DOI is saved for
# citation.
#
# Extent: full Vjosa/Aoos basin via a bounding box (not Sarantaporos-only),
# matching the SDM training extent. The Sarantaporos trim happens in Module 3.
#
# Workflow:
#   1. Resolve target taxa (species list from cleaned HCMR data)
#   2. For each taxon: resolve GBIF key, check counts, submit download
#   3. Wait for, fetch, and combine all archives
#   4. Save combined occurrences + GBIF citation DOI
#
# INPUT:
#   - points_cleaned/fish/fish_basin_hcmr.csv   (species list, from script 01)
#
# OUTPUT:
#   - points_original/fish/fish_data_gbif.csv   (combined raw GBIF download)
#   - points_original/fish/gbif_citation.txt    (GBIF download DOI)
#   - points_original/fish/{taxon}/...          (per-taxon extracted archives)
#
# REQUIRES: GBIF credentials in environment (GBIF_USER, GBIF_PWD, GBIF_EMAIL)
#           and internet access.
#
# LOCATION: workflows/01_biodiversity_data/02_download_gbif_fish.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(rgbif)
library(data.table)
library(dplyr)
library(stringr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

out_dir <- file.path(BASE_DIR, "points_original/fish")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# GBIF API credentials (read from environment, not hard-coded).
GBIF_USER  <- Sys.getenv("GBIF_USER")
GBIF_PWD   <- Sys.getenv("GBIF_PWD")
GBIF_EMAIL <- Sys.getenv("GBIF_EMAIL")

# Basin bounding box (WKT polygon) obtained from the GeoFRESH/upstream tool
# at https://aqua.igb-berlin.de/upstream-dev/ . Used to spatially restrict
# the GBIF query to the Vjosa/Aoos basin.
BASIN_BBOX <- "POLYGON((19.311 39.783, 19.311 40.686, 21.23 40.686, 21.23 39.783, 19.311 39.783))"

# Query mode: "species" restricts to the HCMR species list; "order" pulls
# all Mediterranean freshwater fish orders (broader, used for exploration).
MODE <- "species"

# Mediterranean freshwater fish orders, used only when MODE == "order".
med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

# Target species = those present in the cleaned HCMR basin dataset.
species_list <- fread("points_cleaned/fish/fish_basin_hcmr.csv") %>%
  pull(species) %>%
  unique()

taxa <- if (MODE == "order") med_fish_orders else species_list
rank <- if (MODE == "order") "ORDER" else "SPECIES"

# ============================================================
# STEP 2: Submit downloads (one per taxon)
# ============================================================

submitted <- list()

for (taxon in taxa) {
  cat("\n[", taxon, "]\n")

  # Resolve the taxon to a GBIF usageKey via the backbone taxonomy.
  bb <- tryCatch(name_backbone(name = taxon, rank = rank), error = \(e) NULL)
  if (is.null(bb) || !"usageKey" %in% names(bb)) {
    cat("  no usageKey - skipping\n"); next
  }

  # Skip taxa with no occurrences in the basin to avoid empty downloads.
  n <- tryCatch(
    occ_search(taxonKey = bb$usageKey, geometry = BASIN_BBOX, limit = 1)$meta$count,
    error = \(e) 0L
  )
  if (!length(n) || n == 0) { cat("  0 occurrences - skipping\n"); next }
  cat("  ", n, "occurrences found\n")

  # Submit the download request, retrying up to 5 times. Filters exclude
  # geospatial issues, records without coordinates or eventDate, absences,
  # and fossil/living specimens (not wild occurrences).
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
    Sys.sleep(120)   # back off before retrying a failed submission
  }

  # Throttle GBIF API load: pause briefly every 5 successful submissions.
  if (length(submitted) %% 5 == 0) { cat("  [batch pause 60s]\n"); Sys.sleep(60) }
}

# ============================================================
# STEP 3: Wait, fetch, and combine
# ============================================================

rows <- lapply(names(submitted), function(taxon) {
  key      <- submitted[[taxon]]
  zip_path <- file.path(out_dir, paste0(key, ".zip"))

  # Only wait/download if the archive is not already on disk (lets the
  # script resume without re-fetching completed downloads).
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

  cat("  ", nrow(dt), "rows -", taxon, "\n")
  dt[, taxon_downloaded := taxon][]   # tag rows with the queried taxon
})

combined <- rbindlist(rows, fill = TRUE)
out_csv  <- file.path(out_dir, "fish_data_gbif.csv")
fwrite(combined, out_csv)

# ============================================================
# STEP 4: Save GBIF citation DOI
# ============================================================

# DOI of the last download; cite this in the manuscript for the GBIF data.
citation <- occ_download_meta(key)$doi
write.table(citation, "points_original/fish/gbif_citation.txt")

# ============================================================
# SUMMARY
# ============================================================

message(
  "\n", strrep("=", 60), "\nGBIF DOWNLOAD COMPLETE\n", strrep("=", 60),
  "\n  Mode:            ", MODE,
  "\n  Taxa downloaded: ", uniqueN(combined$taxon_downloaded),
  "\n  Total rows:      ", nrow(combined),
  "\n  Saved to:        ", out_csv,
  "\n\nNext: 03_clean_gbif_fish.R"
)
