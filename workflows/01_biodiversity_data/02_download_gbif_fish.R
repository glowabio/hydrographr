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

# Got bounding box from https://aqua.igb-berlin.de/upstream-dev/
BASIN_BBOX <- "POLYGON((19.311 39.783, 19.311 40.686, 21.23 40.686, 21.23 39.783, 19.311 39.783))"
MODE       <- "species"   # "order" | "species"

med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

species_list <- fread("points_cleaned/fish/fish_basin_hcmr.csv") %>%
  pull(species) %>%
  unique()

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
out_csv  <- file.path(out_dir, "fish_data_gbif.csv")
fwrite(combined, out_csv)

# GBIF citation
citation <- occ_download_meta(key)$doi

# write out
write.table(citation, "points_original/fish/gbif_citation.txt")

# ── Summary ──────────────────────────────────────────────────────────────────

message(
  "\n", strrep("=", 60), "\nGBIF DOWNLOAD COMPLETE\n", strrep("=", 60),
  "\n  Mode:            ", MODE,
  "\n  Taxa downloaded: ", uniqueN(combined$taxon_downloaded),
  "\n  Total rows:      ", nrow(combined),
  "\n  Saved to:        ", out_csv,
  "\n\nNext: 02_clean_gbif_data.R"
)
