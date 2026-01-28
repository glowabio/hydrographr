# Full, robust GBIF fish downloader for Greece (copy-paste all at once)
# Requirements: install.packages(c("rgbif","sf","data.table","dplyr"))
library(rgbif)
library(sf)
library(data.table)
library(dplyr)

# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
setwd(wdir)


## ----- configuration -----
med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

greece_poly <- "POLYGON((19.0 42.0, 19.0 34.5, 29.7 34.5, 29.7 42.0, 19.0 42.0))"

# local folder to save ZIPs and CSVs
download_path <- file.path(wdir, "points_original/fish")
dir.create(download_path)

# Credentials (replace)
GBIF_USER  <- ""
GBIF_PWD   <- ""
GBIF_EMAIL <- ""

## ----- helper: safe name_backbone that tolerates failures -----
safe_name_backbone <- function(name, rank = "ORDER") {
  res <- tryCatch({
    rgbif::name_backbone(name = name, rank = rank)
  }, error = function(e) {
    message("  -> name_backbone error for ", name, ": ", conditionMessage(e))
    return(NULL)
  })
  return(res)
}

## ----- main submitter: submit downloads in batches with retries -----
submit_gbif_downloads <- function(orders, polygon, user, pwd, email,
                                  batch_size = 3, wait_time = 300) {
  if(!dir.exists(download_path)) dir.create(download_path, recursive = TRUE)
  results <- list()
  submitted_in_batch <- 0

  for(order in orders) {
    cat("\nProcessing order:", order, "...\n")
    bb <- safe_name_backbone(order, rank = "ORDER")
    key <- if(!is.null(bb) && "usageKey" %in% names(bb)) bb$usageKey else NULL
    if(is.null(key)) {
      cat("  -> ERROR: No usageKey for", order, "- skipping.\n")
      next
    }

    # quick check for occurrences (safe)
    meta <- tryCatch({
      occ_search(taxonKey = key, geometry = polygon, limit = 1)$meta
    }, error = function(e){
      message("  -> occ_search error for ", order, ": ", conditionMessage(e))
      return(NULL)
    })
    n_occ <- if(!is.null(meta) && "count" %in% names(meta)) meta$count else 0
    if(n_occ == 0){
      cat("  -> No occurrences for", order, "- skipping.\n")
      next
    } else {
      cat("  -> Found", n_occ, "occurrences. Preparing download...\n")
    }

    # retry loop for submission to handle HTTP2/network errors
    success <- FALSE
    attempts <- 0
    while(!success && attempts < 5) {
      attempts <- attempts + 1
      d <- tryCatch({
        occ_download(
          pred("hasGeospatialIssue", FALSE),
          pred("hasCoordinate", TRUE),
          pred("occurrenceStatus", "PRESENT"),
          pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))),
          pred_in("taxonKey", key),
          pred_within(polygon),
          format = "SIMPLE_CSV",
          user = GBIF_USER, pwd = GBIF_PWD, email = GBIF_EMAIL
        )
      }, error = function(e) {
        message("  -> Submit failed (attempt ", attempts, ") for ", order, ": ", conditionMessage(e))
        return(NULL)
      })

      if(!is.null(d)) {
        success <- TRUE
        results[[order]] <- d
        submitted_in_batch <- submitted_in_batch + 1
        cat("  -> Download submitted for", order, "\n")
      } else {
        cat("  -> Waiting 120 seconds before retrying submission...\n")
        Sys.sleep(120)
      }
    }
    if(!success) {
      cat("  -> Failed to submit download for", order, "after retries; skipping.\n")
    }

    # batch wait
    if(submitted_in_batch >= batch_size) {
      cat("\nBatch of", batch_size, "submitted — waiting", wait_time, "seconds before next batch...\n")
      Sys.sleep(wait_time)
      submitted_in_batch <- 0
    }
  }
  return(results)
}

## ----- retrieval + import: wait, download ZIPs, extract occurrence.csv, read safely -----
retrieve_and_combine <- function(results, out_dir = download_path, import_data = TRUE) {
  if(length(results) == 0) {
    message("No downloads were submitted.")
    return(NULL)
  }
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  per_order_list <- list()

  for(order in names(results)) {
    key <- results[[order]]
    cat("\n=====================================\n")
    cat("Order:", order, " | key:", key, "\n")

    # if ZIP already exists, skip waiting/receiving again
    zip_name_expected <- paste0(key, ".zip")
    zip_path_expected <- file.path(out_dir, zip_name_expected)
    if(file.exists(zip_path_expected)) {
      cat("  -> ZIP already present locally:", zip_path_expected, "\n")
      zip_file <- zip_path_expected
    } else {
      # wait
      cat("  -> Waiting for GBIF to finish preparing the download...\n")
      occ_download_wait(key)
      # download
      cat("  -> Downloading ZIP...\n")
      dlres <- occ_download_get(key, path = out_dir)
      # occ_download_get sometimes returns a path or a list object; handle both
      zip_file <- if(is.character(dlres)) dlres else if(is.list(dlres) && "path" %in% names(dlres)) dlres$path else NA
      if(is.na(zip_file) || !file.exists(zip_file)) {
        message("  -> Failed to download ZIP for ", order, " (key=", key, "). Skipping.")
        next
      }
    }

    cat("  -> Unzipping and locating occurrence file...\n")
    # list files in zip
    zlist <- unzip(zip_file, list = TRUE)$Name
    occ_csv_name <- zlist[grepl("occurrence.*\\.csv$", zlist, ignore.case = TRUE)]
    if(length(occ_csv_name) == 0) {
      message("  -> No occurrence csv found in ZIP for ", order, ". Listing zip content:")
      print(zlist)
      next
    }
    occ_csv_name <- occ_csv_name[1]  # first match
    # create folder and extract only that CSV
    order_dir <- file.path(out_dir, gsub("[^A-Za-z0-9_\\-]", "_", order))
    if(!dir.exists(order_dir)) dir.create(order_dir, recursive = TRUE)
    extracted_csv <- unzip(zip_file, files = occ_csv_name, exdir = order_dir)
    if(length(extracted_csv) == 0 || !file.exists(extracted_csv)) {
      message("  -> Failed to extract CSV for ", order)
      next
    }

    # Read with fread fill=TRUE and then coerce all columns to character to avoid bind type issues
    cat("  -> Reading CSV with data.table::fread (fill=TRUE)...\n")
    df <- tryCatch({
      data.table::fread(extracted_csv, fill = TRUE, showProgress = FALSE)
    }, error = function(e) {
      message("  -> fread error for ", order, ": ", conditionMessage(e))
      return(NULL)
    })
    if(is.null(df)) next
    # convert all columns to character to prevent type mismatches
    df_char <- as.data.frame(lapply(df, function(col) if(is.factor(col)) as.character(col) else as.character(col)),
                             stringsAsFactors = FALSE)
    df_char$order_downloaded <- order
    per_order_list[[order]] <- df_char
    cat("  -> Completed import for", order, " (rows:", nrow(df_char), ")\n")
  }

  # combine safely using data.table::rbindlist fill=TRUE
  if(import_data && length(per_order_list) > 0) {
    combined <- data.table::rbindlist(per_order_list, fill = TRUE, use.names = TRUE)
    combined_df <- as.data.frame(combined, stringsAsFactors = FALSE)
    # save a combined CSV for convenience
    combined_csv <- file.path(out_dir, "combined_greece_fish_occurrences.csv")
    data.table::fwrite(combined_df, combined_csv)
    cat("\nCombined dataset saved to:", combined_csv, "\n")
    return(combined_df)
  } else {
    return(per_order_list)
  }
}

## ----- Run everything (submit + retrieve + combine) -----
# 1) Submit (this will only create requests on GBIF and return keys)
submitted_keys <- submit_gbif_downloads(med_fish_orders, greece_poly,
                                        user = GBIF_USER, pwd = GBIF_PWD, email = GBIF_EMAIL,
                                        batch_size = 3, wait_time = 400)

retrieve_and_combine <- function(submitted_keys, out_dir) {
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  combined_list <- list()

  for(order in names(submitted_keys)) {
    key <- submitted_keys[[order]]
    cat("\n=====================================\n")
    cat("Order:", order, "| key:", key, "\n")

    # Wait for GBIF download to finish
    occ_download_wait(key)

    # Get ZIP file
    zip_path <- occ_download_get(key, path = out_dir)$path
    cat("  -> ZIP downloaded:", zip_path, "\n")

    # Unzip to order-specific folder
    unzip_dir <- file.path(out_dir, order)
    dir.create(unzip_dir, showWarnings = FALSE)
    unzip(zip_path, exdir = unzip_dir)

    # Find CSV dynamically
    csv_files <- list.files(unzip_dir, full.names = TRUE, pattern = "\\.csv$")
    if(length(csv_files) == 0) {
      cat("  -> No CSV found for", order, "\n")
      next
    }

    # Read CSV
    df <- readr::read_csv(csv_files[1], guess_max = 200000)
    combined_list[[order]] <- df
  }

  # Combine all orders into single data frame
  combined_df <- bind_rows(combined_list, .id = "order")
  return(combined_df)
}



# Path where ZIPs are stored


# Orders (folder names)
med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

extract_and_combine_csvs_with_order <- function(submitted_keys, zip_dir) {
  if(!dir.exists(zip_dir)) stop("Folder does not exist: ", zip_dir)

  combined_list <- list()

  for(order in names(submitted_keys)) {
    key <- submitted_keys[[order]]
    zip_file <- file.path(zip_dir, paste0(key, ".zip"))

    if(!file.exists(zip_file)) {
      cat("\n=====================================\n")
      cat("ZIP not found for order:", order, "| expected file:", zip_file, "\n")
      next
    }

    cat("\n=====================================\n")
    cat("Processing order:", order, "| ZIP:", zip_file, "\n")

    # List contents
    zip_list <- unzip(zip_file, list = TRUE)$Name
    # <- ANY csv file in ZIP
    csv_files <- zip_list[grepl("\\.csv$", zip_list, ignore.case = TRUE)]
    if(length(csv_files) == 0) {
      cat("  -> No CSV found in ZIP\n")
      next
    }

    # Extract first CSV
    order_dir <- file.path(zip_dir, gsub("[^A-Za-z0-9_\\-]", "_", order))
    if(!dir.exists(order_dir)) dir.create(order_dir, recursive = TRUE)
    extracted_csv <- unzip(zip_file, files = csv_files[1], exdir = order_dir)

    # Read CSV safely
    df <- tryCatch({
      dt <- data.table::fread(extracted_csv, fill = TRUE, showProgress = FALSE)
      df_char <- as.data.frame(lapply(dt, as.character), stringsAsFactors = FALSE)
      df_char$order_downloaded <- order
      df_char
    }, error = function(e) {
      message("  -> Error reading CSV for order ", order, ": ", conditionMessage(e))
      return(NULL)
    })

    if(!is.null(df)) combined_list[[order]] <- df
  }

  if(length(combined_list) > 0) {
    combined_df <- data.table::rbindlist(combined_list, fill = TRUE)
    combined_df <- as.data.frame(combined_df, stringsAsFactors = FALSE)

    combined_csv_path <- file.path(zip_dir, "combined_greece_fish_occurrences.csv")
    data.table::fwrite(combined_df, combined_csv_path)
    cat("\nCombined CSV saved to:", combined_csv_path, "\n")
    return(combined_df)
  } else {
    cat("No data found in any ZIPs.\n")
    return(NULL)
  }
}

# Usage
combined_fish_df <- extract_and_combine_csvs_with_order(submitted_keys, download_path)
str(combined_fish_df)




# combined_fish_df is your combined data.frame (all columns coerced to character)
# Note: if you want to keep original types for some columns, you can post-process combined_fish_df afterwards.


downloads <- download_gbif_fish_greece(med_fish_orders, greece_poly, "afrogri", "2LYZcSFo51bu", "afroditi.grigoropoulou@igb-berlin.de")


# ============================================================================
# FINALIZE AND SAVE FOR CLEANING PIPELINE
# ============================================================================

message("\n=== Saving Combined Data for Cleaning ===")

# Ensure output directory exists
output_dir <- file.path(download_path, "points_original/fish")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save the combined dataset
output_file <- file.path(output_dir, "combined_greece_fish_occurrences.csv")
data.table::fwrite(combined_fish_df, output_file)

message(sprintf("✓ Saved %d rows to: %s", nrow(combined_fish_df), output_file))
message("\n" , paste(rep("=", 80), collapse = ""))
message("GBIF DOWNLOAD COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message(sprintf("\nDataset Summary:"))
message(sprintf("  Total rows: %d", nrow(combined_fish_df)))
message(sprintf("  Total columns: %d", ncol(combined_fish_df)))
message(sprintf("  Orders downloaded: %d", length(unique(combined_fish_df$order_downloaded))))
message("\nNext step: Run the cleaning script")
message(sprintf("  Script location: ~/Documents/Postdoc/projects/workflow_paper/scripts/02_clean_gbif_data.R"))
