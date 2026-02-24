#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02 -- dispersal_classes.R
# Estimate species dispersal distances using fishmove,
# bin into quantile classes, assign dispersal probability (PD)
#
# Reference for caudal fin aspect ratio as swimming performance proxy:
#   Radinger & Wolter (2013) Patterns and predictors of fish dispersal in rivers
#
# REPLACES: fish_dis_class.txt generation (done manually in Mekong study)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
# devtools::install_github("cran/fishmove")
library(fishmove)
library(readxl)
library(data.table)

# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
setwd(wdir)

# ============================================================
# PARAMETERS
# ============================================================
N_DISP_CLASSES <- 7
PD_MIN         <- 0.3
PD_MAX         <- 0.9

# ============================================================
# INPUT FILE
# ============================================================

# FILE 4: fish_traits.txt
# Required columns: species, max_length (cm), caudal_ar
# caudal_ar: caudal fin aspect ratio from FishBase
#            = (caudal fin height^2) / caudal fin surface area
#            proxy for sustained swimming performance (Radinger & Wolter 2013)
fish_traits <- read_xlsx("points_original/fish/Fish distributional & traits data (1).xlsx", sheet = "Traits")
fish_traits <- fish_traits %>% dplyr::rename("species" = "Species")

# Occurrences to compute mean stream order per species
occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")
occurrences <- occurrences %>% filter(source=="HCMR")

# Subcatchments to get strahler_order
subcatchments <- st_read("spatial/stream_networks/partial_stream_network.gpkg") %>%
  distinct(subc_id, .keep_all = TRUE)

# ============================================================
# STEP 1: Compute mean stream order per species from occurrences
# ============================================================
species_stream_order <- occurrences %>%
  left_join(subcatchments %>%
              st_drop_geometry() %>%
              dplyr::select(subc_id, strahler_order),
            by = "subc_id") %>%
  group_by(species) %>%
  dplyr::summarise(
    mean_SO = round(mean(strahler_order, na.rm = TRUE)),
    max_SO  = max(strahler_order, na.rm = TRUE),
    n_obs   = n(),
    .groups = "drop"
  )

# Join with traits
fish_traits <- fish_traits %>%
  left_join(species_stream_order, by = "species")

# Flag species with missing stream order data
missing_SO <- fish_traits %>% filter(is.na(mean_SO))
if (nrow(missing_SO) > 0) {
  message("WARNING: ", nrow(missing_SO), " species have no stream order data")
  message("These species will use default SO=5")
  fish_traits <- fish_traits %>%
    mutate(mean_SO = ifelse(is.na(mean_SO), 5, mean_SO))
}

# ============================================================
# STEP 2: Run fishmove per species
# fishmove expects L in millimeters, not centimeters!
# ============================================================
DEFAULT_AR <- 2.5

dispersal_results <- fish_traits %>%
  rowwise() %>%
  dplyr::mutate(
    # Use sigma_mob (mobile component) as dispersal distance proxy
    disp_dist = {
      result <- fishmove(
        L  = max_TL * 10,
        AR = DEFAULT_AR,
        SO = mean_SO,
        T  = 30,
        interval = "confidence",
        rep = 50
      )
      result$pred.fishmove["fit", "sigma_mob", 1, 1, 1, 1]
    }
  ) %>%
  ungroup()



# ============================================================
# STEP 3: Bin into dispersal classes
# ============================================================

quantile_breaks <- quantile(dispersal_results$disp_dist,
                            probs = seq(0, 1, length.out = 8),
                            na.rm = TRUE)

pd_values <- seq(0.3, 0.9, length.out = 7)

dispersal_results <- dispersal_results %>%
  mutate(
    disp_class = cut(disp_dist, breaks = quantile_breaks,
                     labels = FALSE, include.lowest = TRUE),
    dispersal_prob = pd_values[disp_class]
  )
# ============================================================
# STEP 4: Report and save
# ============================================================

fish_dis_class <- dispersal_results %>%
  dplyr::select(species, max_TL, mean_SO, disp_dist, disp_class, dispersal_prob)

fwrite(fish_dis_class, "traits/fish_dis_class.txt")
message("\nfish_dis_class.txt saved")
