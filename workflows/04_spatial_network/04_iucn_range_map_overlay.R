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
