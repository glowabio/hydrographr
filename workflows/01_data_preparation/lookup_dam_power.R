#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# lookup_dam_power.R
#
# Look up power attributes (power_mw, max_power) for the Sarantaporos dams
# from the Greece-wide dam Excel.
#
# JOIN KEY: dams_sarantaporos_table.csv$Name_GR == Excel$aa  (the RAAY plant
# code). 'aa' is NOT unique across rows because each plant has multiple
# geometry parts, so we keep only part == "Y/L" (the plant location) before
# joining. The Excel has one sheet per licence type (Installation /
# Operational / Production / Evaluation); we read and stack ALL sheets.
#
# A coordinate match (from the clean WKT columns) is used only as a fallback
# for any Sarantaporos dam whose Name_GR does not match an 'aa'.
#
# Output: dams_sarantaporos_power.csv
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)
# ------------------------------------------------------------
# PARAMETERS
# ------------------------------------------------------------

SARANTAPOROS_CSV <- "points_original/dams/dams_sarantaporos_table.csv"
GREECE_XLSX      <- "points_original/dams/All_hydro_Oct_2023.xlsx"
COL_POWER_MW     <- "power_mw"
COL_MAX_POWER    <- "max_power"
PART_KEEP        <- "Y/L"                              # plant-location rows
MATCH_DECIMALS   <- 4                                  # coord fallback (~10 m)

# ------------------------------------------------------------
# Helper: first lon/lat from a WKT POINT/MULTIPOINT string
# ------------------------------------------------------------
parse_lonlat <- function(wkt) {
  nums <- str_match(wkt, "(-?\\d+\\.\\d+)\\s+(-?\\d+\\.\\d+)")
  data.table(lon = as.numeric(nums[, 2]), lat = as.numeric(nums[, 3]))
}

# ------------------------------------------------------------
# STEP 1: Sarantaporos dams
# ------------------------------------------------------------
sar <- fread(SARANTAPOROS_CSV, sep = ";")
message("Sarantaporos rows: ", nrow(sar),
        "  | types: ", paste(sort(unique(sar$type)), collapse = ", "))

if (!"Name_GR" %in% names(sar))
  stop("Name_GR column not found in ", SARANTAPOROS_CSV)

sar_xy <- parse_lonlat(sar$geometry_wkt)
sar <- cbind(sar, sar_xy)
sar[, `:=`(lon_key = round(lon, MATCH_DECIMALS),
           lat_key = round(lat, MATCH_DECIMALS))]

# ------------------------------------------------------------
# STEP 2: Read ALL sheets of the Excel, stack, filter Y/L
# ------------------------------------------------------------
sheets <- excel_sheets(GREECE_XLSX)
message("\nExcel sheets (", length(sheets), "): ", paste(sheets, collapse = ", "))

gr <- map_dfr(sheets, function(sh) {
  d <- as.data.table(read_excel(GREECE_XLSX, sheet = sh))
  d[, sheet := sh]
  d
})
message("  Total rows across sheets: ", nrow(gr))
message("  Columns: ", paste(names(gr), collapse = ", "))

# filter to plant-location rows (Y/L) — resolves non-unique 'aa'
if ("part" %in% names(gr)) {
  n0 <- nrow(gr)
  gr <- gr[part == PART_KEEP]
  message("  Kept part == '", PART_KEEP, "': ", nrow(gr), " / ", n0, " rows")
} else {
  message("  WARNING: no 'part' column — cannot filter to Y/L; 'aa' may be non-unique")
}

# required columns present?
if (!"aa" %in% names(gr)) stop("'aa' column not found in Excel sheets")
power_cols <- intersect(c(COL_POWER_MW, COL_MAX_POWER), names(gr))
if (length(power_cols) == 0)
  stop("Power columns not found. Available: ", paste(names(gr), collapse = ", "))

# coordinates from the Excel geometry (for fallback + verification)
geom_col <- names(gr)[sapply(gr, function(x)
  is.character(x) && any(grepl("POINT", x, ignore.case = TRUE)))][1]
if (!is.na(geom_col)) {
  gr_xy <- parse_lonlat(gr[[geom_col]])
  gr <- cbind(gr, gr_xy)
  gr[, `:=`(lon_key = round(lon, MATCH_DECIMALS),
            lat_key = round(lat, MATCH_DECIMALS))]
}

# de-duplicate 'aa' if still repeated after Y/L filter (keep first; warn)
dup_aa <- gr[, .N, by = aa][N > 1]
if (nrow(dup_aa) > 0) {
  message("  NOTE: ", nrow(dup_aa), " 'aa' codes still appear >1x after Y/L filter; ",
          "keeping first occurrence of each.")
  gr <- gr[, .SD[1], by = aa]
}

id_cols  <- intersect(c("id1", "aa", "company", "sheet"), names(gr))
keep_cols <- unique(c(id_cols, power_cols,
                      intersect(c("lon", "lat", "lon_key", "lat_key"), names(gr))))
gr_small <- gr[, ..keep_cols]

# ------------------------------------------------------------
# STEP 3: Primary join — Name_GR == aa
# ------------------------------------------------------------
matched <- merge(sar, gr_small,
                 by.x = "Name_GR", by.y = "aa",
                 all.x = TRUE, suffixes = c("", "_gr"))

unmatched <- matched[is.na(get(power_cols[1]))]
message("\nID-matched (Name_GR=aa): ",
        nrow(matched) - nrow(unmatched), " / ", nrow(sar))

# ------------------------------------------------------------
# STEP 4: Coordinate fallback for unmatched rows
# ------------------------------------------------------------
if (nrow(unmatched) > 0 && !is.na(geom_col)) {
  message("  Trying coordinate fallback for ", nrow(unmatched), " unmatched dams...")
  fb <- merge(unmatched[, .(Name_GR, site_id, Name, type, lon_key, lat_key)],
              gr_small[, c("lon_key", "lat_key", power_cols), with = FALSE],
              by = c("lon_key", "lat_key"), all.x = TRUE)
  n_fb <- fb[!is.na(get(power_cols[1])), .N]
  message("  Coordinate fallback recovered: ", n_fb)
  if (n_fb > 0) {
    for (pc in power_cols) {
      idx <- match(matched$Name_GR, fb$Name_GR)
      need <- is.na(matched[[pc]]) & !is.na(idx)
      matched[[pc]][need] <- fb[[pc]][idx[need]]
    }
  }
}

still_missing <- matched[is.na(get(power_cols[1])), .(site_id, Name, Name_GR, type)]
if (nrow(still_missing) > 0) {
  message("\n  Still unmatched (no power found):")
  print(still_missing)
}

# ------------------------------------------------------------
# STEP 5: Report power spread for DAMs (cost-discrimination check)
# ------------------------------------------------------------
dams_only <- matched[type == "DAM"]
message("\n--- Power values for DAM-type barriers in Sarantaporos ---")
print(dams_only[, c("site_id", "Name_GR", power_cols), with = FALSE])

if (nrow(dams_only) > 0) {
  pv <- dams_only[[power_cols[2]]]; pv <- pv[!is.na(pv)]
  if (length(pv) > 0) {
    message("\n  ", power_cols[2], " across DAMs: range ",
            round(min(pv), 3), " - ", round(max(pv), 3),
            " | distinct values: ", length(unique(pv)))
    message("  (if ~1 distinct value, power does NOT discriminate dams -> ",
            "not useful as a cost layer)")
  }
}

fwrite(matched, "dams_sarantaporos_power.csv")
message("\nSaved: dams_sarantaporos_power.csv")

# ------------------------------------------------------------
# STEP 6: Write DAMs as GeoPackage (power columns only) for QGIS
# ------------------------------------------------------------
library(sf)

dams_gpkg <- dams_only[!is.na(lon_gr) & !is.na(lat_gr)]
keep_gpkg <- c("site_id", "Name", "Name_GR", "type", power_cols, "lon_gr", "lat_gr")
keep_gpkg <- intersect(keep_gpkg, names(dams_gpkg))
dams_gpkg <- dams_gpkg[, ..keep_gpkg]

dams_sf <- st_as_sf(dams_gpkg, coords = c("lon_gr", "lat_gr"), crs = 4326)

st_write(dams_sf, "points_cleaned/dams/dams_sarantaporos_power.gpkg", delete_dsn = TRUE, quiet = TRUE)
message("Saved: dams_sarantaporos_power.gpkg  (", nrow(dams_sf), " DAMs, columns: ",
        paste(setdiff(keep_gpkg, c("lon", "lat")), collapse = ", "), ")")


