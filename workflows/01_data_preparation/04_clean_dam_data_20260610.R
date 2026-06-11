#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_clean_dam_data.R   (Module 1 -- Barrier data preparation)
#
# Reproducible cleaning of small-hydropower (SHP) barrier data for the
# Sarantaporos sub-basin, starting from the RAE (RAAEY) licence registers.
#
# PROVENANCE (important for Methods):
#   The barrier data are the RAE small-hydropower registers, downloaded
#   January 2026 as five CSVs, one per licensing stage:
#     - Operational  (V_SDI_R_HYDRO13)  -> existing
#     - Installation (V_SDI_R_HYDRO12)  -> planned
#     - Production    (V_SDI_R_HYDRO11) -> planned
#     - Evaluation    (V_SDI_R_HYDRO7)  -> planned
#     - Rejected      (..._OTHER_VALUES)-> EXCLUDED (will not be built)
#   Each licence ('aa' = RAE plant code) has multiple geometry "parts":
#   Y/L = water-intake / dam locations, Y/S = powerhouse. A single licence
#   can therefore have several points along the river (intakes + station),
#   sometimes kilometres apart, sharing one 'aa' and one capacity (power_mw).
#
# CLASSIFICATION (dam / factory / dry / exclude):
#   Each point was classified by an expert (co-author) into DAM (a real
#   in-stream barrier), FACTORY (powerhouse), DRY (stream verified dry on
#   the ground / satellite) or EXCLUDE. This expert judgement is NOT
#   reproducible from the registers; it is supplied as a documented input
#   table (dams_sarantaporos_table.csv) and joined here BY COORDINATE.
#
# POWER OF MULTI-PART PLANTS:
#   A licence's capacity (power_mw) is the plant total, repeated on every
#   part row. Where one licence contributes several DAM points to the
#   network, its capacity is SPLIT equally across those DAM points so the
#   plant's energy is not counted multiple times in the per-MW dam ranking
#   (Module 9). Connectivity/fragmentation still treats each DAM as a cut.
#
# SCENARIOS:
#   existing = Operational licence dams within Sarantaporos
#   planned  = Installation + Production + Evaluation dams within Sarantaporos
#   (Rejected dams are excluded from both.)
#
# Input:
#   - points_original/dams/V_SDI_R_HYDRO13_Operational_Licence.csv
#   - points_original/dams/V_SDI_R_HYDRO12_Installation_Licence.csv
#   - points_original/dams/V_SDI_R_HYDRO11_Production_Licence.csv
#   - points_original/dams/V_SDI_R_HYDRO7_Evaluation.csv
#   - points_original/dams/dams_sarantaporos_table.csv   (expert classification + site_id)
#
# Note: no basin-polygon filter is applied here because the expert
# classification table is already restricted to the study set. To enforce
# a strict Sarantaporos-only cut, intersect with api_get_upstream_catchment().
#
# Output:
#   - points_cleaned/dams/dams_sarantaporos_clean.csv    (DAMs, existing+planned, with power)
#   - points_cleaned/dams/dams_sarantaporos_clean.gpkg
#   - points_cleaned/dams/dams_classification_full.csv   (all parts + type, audit trail)
#   - points_cleaned/maps/dams_sarantaporos_clean.html
#
# LOCATION: workflows/01_data_preparation/04_clean_dam_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
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
setwd(BASE_DIR)

dir.create("points_cleaned/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

DAM_DIR <- "points_original/dams"

# RAE register CSVs and the status each implies.
# Rejected is read only to be reported/excluded, not used in scenarios.
RAE_FILES <- tribble(
  ~file,                                          ~stage,         ~status,
  "V_SDI_R_HYDRO13_Operational_Licence.csv",      "operational",  "existing",
  "V_SDI_R_HYDRO12_Installation_Licence.csv",     "installation", "planned",
  "V_SDI_R_HYDRO11_Production_Licence.csv",        "production",  "planned",
  "V_SDI_R_HYDRO7_Evaluation.csv",                "evaluation",   "planned"
)
# Rejected handled separately (reported, excluded)
REJECTED_FILE <- "V_SDI_R_HYDRO_OTHER_VALUES_Rejected.csv"

# expert classification table (co-author): coordinates + type
CLASS_FILE <- file.path(DAM_DIR, "dams_sarantaporos_table.csv")

# only these types are kept as network barriers
DAM_TYPES <- "DAM"

# coordinate rounding for the classification join (~10 m at 4 dp)
JOIN_DP <- 4

# ============================================================
# Helper: first lon/lat from a WKT (MULTI)POINT string
# ============================================================
parse_lonlat <- function(wkt) {
  m <- str_match(wkt, "(-?\\d+\\.\\d+)\\s+(-?\\d+\\.\\d+)")
  data.table(longitude = as.numeric(m[, 2]),
             latitude  = as.numeric(m[, 3]))
}

# ============================================================
# STEP 1: Read & stack the RAE registers (OL + IL + PL + Evaluation)
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING (RAE registers -> Sarantaporos barriers)")
message(paste(rep("=", 80), collapse = ""))

message("\n=== Step 1: Reading RAE registers ===")

read_rae <- function(file, stage, status) {
  path <- file.path(DAM_DIR, file)
  if (!file.exists(path)) stop("RAE file not found: ", path)
  d <- fread(path, encoding = "UTF-8")
  xy <- parse_lonlat(d$geometry)
  d <- cbind(d, xy)
  d %>%
    transmute(
      aa, a_m, part, company,
      thesh,
      power_mw    = as.numeric(power_mw),
      max_power   = as.numeric(max_power),
      longitude, latitude,
      stage       = stage,
      status      = status
    )
}

rae <- pmap_dfr(RAE_FILES, function(file, stage, status)
  read_rae(file, stage, status))

message("  RAE rows (OL+IL+PL+Evaluation): ", nrow(rae))
message("  by stage: ",
        paste(names(table(rae$stage)), table(rae$stage), sep = "=", collapse = ", "))

# Some plants are registered in MORE THAN ONE sheet (e.g. a project listed
# under both Production and Evaluation). That produces duplicate points for
# the same physical location (same aa + part + coordinates) with different
# stage/status. Keep ONE row per (aa, part, location), with priority:
#   operational > production > installation > evaluation
# (existing wins; among planned, the more advanced licensing stage wins).
STAGE_PRIORITY <- c(operational = 1, production = 2, installation = 3, evaluation = 4)

n_before <- nrow(rae)
rae <- rae %>%
  mutate(.lonr = round(longitude, JOIN_DP),
         .latr = round(latitude,  JOIN_DP),
         .prio = STAGE_PRIORITY[stage]) %>%
  arrange(.prio) %>%
  distinct(aa, part, .lonr, .latr, .keep_all = TRUE) %>%
  select(-.lonr, -.latr, -.prio)
message("  De-duplicated across sheets: ", n_before, " -> ", nrow(rae),
        " (", n_before - nrow(rae), " duplicate cross-sheet rows removed)")

# Report rejected count (for Methods / transparency), then leave them out
rej <- read_rae(REJECTED_FILE, "rejected", "rejected")
message("  Rejected rows (EXCLUDED from scenarios): ", nrow(rej))

# ============================================================
# STEP 2: Join expert classification (BY COORDINATE)
# ============================================================
# The co-author table classifies each point as DAM / FACTORY / DRY /
# EXCLUDE. We join it to the RAE points on rounded coordinates (the two
# share the same source geometry, so coordinates match to ~10 m).

message("\n=== Step 2: Joining expert classification (by coordinate) ===")

classif <- fread(CLASS_FILE, sep = ";", encoding = "UTF-8") %>%
  transmute(
    site_id  = site_id,                       # keep the existing C- site_id
    type     = toupper(trimws(type)),
    lon_key  = round(as.numeric(longitude), JOIN_DP),
    lat_key  = round(as.numeric(latitude),  JOIN_DP),
    Name_GR
  )

rae <- rae %>%
  mutate(lon_key = round(longitude, JOIN_DP),
         lat_key = round(latitude,  JOIN_DP)) %>%
  left_join(classif, by = c("lon_key", "lat_key"))

n_unclassified <- sum(is.na(rae$type))
message("  RAE points classified: ", sum(!is.na(rae$type)),
        " | unclassified: ", n_unclassified,
        " (unclassified are outside the Sarantaporos study set)")

# audit trail: every part with its classification
fwrite(rae, "points_cleaned/dams/dams_classification_full.csv")

# ============================================================
# STEP 3: Keep DAM points only
# ============================================================

message("\n=== Step 3: Keeping DAM-type points ===")

dams <- rae %>%
  filter(type %in% DAM_TYPES) %>%
  filter(!is.na(longitude), !is.na(latitude))

message("  DAM points (pre-basin-filter): ", nrow(dams),
        " | existing: ", sum(dams$status == "existing"),
        " | planned: ",  sum(dams$status == "planned"))

# ============================================================
# STEP 4: Split plant capacity across co-licence DAM points
# ============================================================
# A licence ('aa') reports its TOTAL capacity on every part. Where one
# licence contributes several DAM points, dividing its power equally
# avoids counting the plant's energy multiple times in the per-MW ranking.
# Connectivity (Module 8/9 boundary) still treats each DAM as a cut.

message("\n=== Step 4: Splitting plant capacity across co-licence DAMs ===")

dams <- dams %>%
  group_by(aa) %>%
  mutate(n_dam_in_licence = n(),
         power_mw_split   = power_mw / n_dam_in_licence) %>%
  ungroup()

multi <- dams %>% filter(n_dam_in_licence > 1) %>% distinct(aa, n_dam_in_licence, power_mw)
if (nrow(multi) > 0) {
  message("  Licences with >1 DAM point (capacity split):")
  for (i in seq_len(nrow(multi)))
    message("    ", multi$aa[i], ": ", multi$power_mw[i], " MW / ",
            multi$n_dam_in_licence[i], " = ",
            round(multi$power_mw[i] / multi$n_dam_in_licence[i], 3), " MW each")
} else {
  message("  (no multi-DAM licences)")
}

# ============================================================
# STEP 5: Final table + outputs
# ============================================================
# (No basin polygon filter: the expert classification table is already
#  restricted to the study set. If a strict Sarantaporos-only cut is
#  wanted later, intersect dams_sf with api_get_upstream_catchment().)

message("\n=== Step 5: Writing outputs ===")

dams_out <- dams %>%
  transmute(
    site_id,
    aa, a_m, part, company, thesh,
    type, status, stage,
    power_mw_licence = power_mw,
    n_dam_in_licence,
    power_mw         = round(power_mw_split, 4),   # split capacity (for ranking)
    longitude, latitude
  )

fwrite(dams_out, "points_cleaned/dams/dams_sarantaporos_clean.csv")
message("  Saved: points_cleaned/dams/dams_sarantaporos_clean.csv  (",
        nrow(dams_out), " DAMs)")

dams_sf <- st_as_sf(dams_out, coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)
st_write(dams_sf %>%
           select(site_id, aa, a_m, part, type, status, power_mw),
         "points_cleaned/dams/dams_sarantaporos_clean.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
message("  Saved: points_cleaned/dams/dams_sarantaporos_clean.gpkg")

# ============================================================
# STEP 6: Map
# ============================================================

message("\n=== Step 6: Map ===")

status_colors <- colorFactor(c("existing" = "darkblue", "planned" = "orange"),
                             domain = c("existing", "planned"))

m <- leaflet(dams_out) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    color = ~status_colors(status), fillColor = ~status_colors(status),
    radius = 5, fillOpacity = 0.8, stroke = TRUE, weight = 1,
    popup = ~paste0("<b>site_id:</b> ", site_id,
                    "<br><b>aa:</b> ", aa, "<br><b>licence:</b> ", a_m,
                    "<br><b>status:</b> ", status,
                    "<br><b>power (split):</b> ", power_mw, " MW",
                    "<br><b>licence total:</b> ", power_mw_licence, " MW")
  ) %>%
  addLayersControl(baseGroups = c("CartoDB", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright", pal = status_colors, values = ~status,
            title = "Dam status", opacity = 0.8)

saveWidget(m, "points_cleaned/maps/dams_sarantaporos_clean.html", selfcontained = TRUE)
save_to_nimbus(m, "points_cleaned/maps/dams_sarantaporos_clean.html")
message("  Saved: points_cleaned/maps/dams_sarantaporos_clean.html")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("Source: RAE registers (Jan 2026), stages OL+IL+PL+Evaluation; Rejected excluded.")
message("Sarantaporos DAMs: ", nrow(dams_out),
        " (existing: ", sum(dams_out$status == "existing"),
        ", planned: ",  sum(dams_out$status == "planned"), ")")
message("Multi-DAM licences (capacity split): ", nrow(multi))
message("\nNext: snapping script (api_get_snapped_points_cascade)")
