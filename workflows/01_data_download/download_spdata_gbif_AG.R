# GBIF fish downloader for Greece — supports download by ORDER or SPECIES LIST
# Requirements: install.packages(c("rgbif", "sf", "data.table", "dplyr", "readxl", "stringr"))

library(rgbif)
library(sf)
library(data.table)
library(dplyr)
library(readxl)
library(stringr)

# ── Configuration ────────────────────────────────────────────────────────────

wdir          <- "/home/grigoropoulou/Documents/Postdoc/projects/workflow_paper/data"
setwd(wdir)

download_path <- file.path(wdir, "points_original/fish")
dir.create(download_path, recursive = TRUE, showWarnings = FALSE)

GBIF_USER  <- ""
GBIF_PWD   <- ""
GBIF_EMAIL <- ""

greece_poly <- "POLYGON((19.0 42.0, 19.0 34.5, 29.7 34.5, 29.7 42.0, 19.0 42.0))"

# ── Taxa to download ─────────────────────────────────────────────────────────

med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

species_list <- fread("points_cleaned/fish/fish_greece_hcmr.csv") %>%
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

cat(fread(file.path(download_path, "combined_greece_fish_occurrences_from_sp_list.csv")) %>% colnames(), sep = "\n")

message("\n", paste(rep("=", 60), collapse = ""))
message("GBIF DOWNLOAD COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("  Mode:              %s", MODE))
message(sprintf("  Total rows:        %d", nrow(combined_fish_df)))
message(sprintf("  Total columns:     %d", ncol(combined_fish_df)))
message(sprintf("  Taxa downloaded:   %d", length(unique(combined_fish_df$taxon_downloaded))))
message("\nNext step: ~/Documents/Postdoc/projects/workflow_paper/scripts/02_clean_gbif_data.R")
