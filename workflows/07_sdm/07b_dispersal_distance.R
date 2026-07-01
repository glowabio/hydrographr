#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 07b_dispersal_distance.R   (Module 7 -- SDM)
#
# Estimate a per-species dispersal distance (sigma_mob, metres) used only as
# the gap-filling threshold in 08_habitat_classification.R. This is the
# physical-distance input to fill_habitat_gaps(); it is separate from the
# ordinal dispersal ranks used for the Population Connectivity Index in
# Module 11, which are derived directly from migratory class.
#
# Method (following the fishmove framework; Radinger & Wolter 2014):
#   sigma_mob for species with a directly measured caudal-fin aspect ratio
#   is taken from fishmove(L, AR, SO, T); for species without AR it is taken
#   from a predictive model log(sigma_mob) ~ log(TL) + sqrt(SO) fitted on the
#   AR-calibrated species. In this seven-species case study none of the focal
#   species has a directly measured AR, so all seven values come from the
#   predictive model. Two species absent from the trait table use a congeneric
#   surrogate, consistent with Modules 10 and 11.
#
# For this case study the exact sigma_mob value does not change the gap-filling
# outcome for six of seven species: the thresholds are far larger than any gap
# length in the network, and the single-patch species are already maximally
# connected. sigma_mob therefore acts as a coarse "fill short gaps" threshold
# rather than a precise dispersal estimate.
#
# The predictive model extrapolates poorly for very large-bodied species
# (Anguilla anguilla), so implausible values are capped (see SIGMA_MOB_CAP_M).
#
# Inputs:
#   points_original/fish/Fish distributional & traits data (1).xlsx  (Traits sheet)
#   points_snapped/fish/fish_all_species_snapped.csv                 (stream order)
#
# Output:
#   traits/fish_dispersal_distance.txt   (columns: species, distance)
#   read by 08_habitat_classification.R via the `distance` column.
#
# LOCATION: workflows/07_sdm/07b_dispersal_distance.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(readxl)
library(data.table)
library(fishmove)

select <- dplyr::select
rename <- dplyr::rename
summarise <- dplyr::summarise
group_by  <- dplyr::group_by
mutate    <- dplyr::mutate
filter <- dplyr::filter
pull      <- dplyr::pull

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("traits", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

T_REF      <- 3650   # 10-year dispersal window, population-level
SO_DEFAULT <- 5      # fallback stream order

# Cap for implausible predictive-model extrapolations. sigma_mob only sets a
# gap-fill threshold; any value far above the network span (~70 km longest
# path) has the same effect, so capping does not change gap-filling but
# prevents nonsensical distances (e.g. Anguilla ~317 km) appearing in outputs.
SIGMA_MOB_CAP_M <- 100000

# Seven focal species (Sarantaporos)
TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Anguilla_anguilla",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Oxynoemacheilus_pindus",
  "Salmo_farioides",
  "Squalius_platyceps"
)

# Congeneric / genus surrogates for species absent from the trait table,
# matching Modules 10 and 11.
#   Chondrostoma ohridanum -> Chondrostoma vardarense (congener)
#   Squalius platyceps     -> median across Squalius spp. in the table (genus)
SURROGATE_CONGENER <- c(Chondrostoma_ohridanum = "Chondrostoma_vardarense")
GENUS_MEDIAN_FOR   <- c("Squalius_platyceps")   # take median over its genus

# ============================================================
# STEP 1: Trait data (max_TL, migratory class)
# ============================================================

fish_traits <- read_xlsx(
  "points_original/fish/Fish distributional & traits data (1).xlsx",
  sheet = "Traits"
) %>%
  rename(species = Species) %>%
  select(species, max_TL, Migration) %>%
  mutate(
    Migration_label = case_when(
      Migration == 0 ~ "Non-migratory",
      Migration == 1 ~ "Potamodromous",
      Migration == 2 ~ "Long_distance",
      TRUE           ~ "Unknown"
    )
  )

message("Trait rows: ", nrow(fish_traits))

# ============================================================
# STEP 2: Stream order per species from snapped occurrences
# ============================================================

occ <- fread("points_snapped/fish/fish_all_species_snapped.csv")

so_by_species <- occ %>%
  group_by(species) %>%
  summarise(max_SO = max(strahler, na.rm = TRUE), .groups = "drop")

fish_traits <- fish_traits %>%
  left_join(so_by_species, by = "species") %>%
  mutate(max_SO = ifelse(is.na(max_SO) | !is.finite(max_SO), SO_DEFAULT, max_SO))

# ============================================================
# STEP 3: Aspect ratios and fishmove calibration set
# ============================================================

ar_data <- read.csv("traits/AspectRatioData.csv") %>%
  group_by(species) %>%
  summarise(AR = mean(AR, na.rm = TRUE), .groups = "drop") %>%
  mutate(species = gsub(" ", "_", species),
         species = ifelse(species == "Gasterosteus_aculeatus",
                          "Gasterosteus_gymnurus", species))

calibration <- fish_traits %>% inner_join(ar_data, by = "species")
message("Calibration species (with AR): ", nrow(calibration))

fm_results <- data.frame()
for (i in seq_len(nrow(calibration))) {
  sp    <- calibration$species[i]
  tl_mm <- calibration$max_TL[i] * 10
  ar    <- calibration$AR[i]
  so    <- calibration$max_SO[i]
  tryCatch({
    fm <- fishmove(L = tl_mm, AR = ar, SO = so, T = T_REF, rep = 200, seed = 42)
    fm_results <- rbind(fm_results, data.frame(
      species = sp, sigma_mob_m = fm$pred.fishmove["fit", "sigma_mob", , , , ]
    ))
  }, error = function(e) cat("  fishmove error:", sp, conditionMessage(e), "\n"))
}

# ============================================================
# STEP 4: Predictive model log(sigma_mob) ~ log(TL) + sqrt(SO)
# ============================================================

cal_data <- fm_results %>%
  left_join(fish_traits, by = "species") %>%
  mutate(log_sigma_mob = log(sigma_mob_m),
         log_TL  = log(max_TL),
         sqrt_SO = sqrt(max_SO))

disp_model <- lm(log_sigma_mob ~ log_TL + sqrt_SO, data = cal_data)
cat("\nPredictive model R^2: ", round(summary(disp_model)$r.squared, 3), "\n")

predict_sigma <- function(tl, so) {
  as.numeric(exp(predict(disp_model,
                         newdata = data.frame(log_TL = log(tl), sqrt_SO = sqrt(so)))))
}

# ============================================================
# STEP 5: sigma_mob for the seven focal species
# ============================================================

# Helper: get max_TL / max_SO for a species, applying surrogate rules.
trait_row <- function(sp) {

  # Direct match
  r <- fish_traits %>% filter(species == sp)

  # Congener surrogate (e.g. Chondrostoma ohridanum -> vardarense)
  if (nrow(r) == 0 && sp %in% names(SURROGATE_CONGENER)) {
    surrogate <- SURROGATE_CONGENER[[sp]]
    r <- fish_traits %>% filter(species == surrogate)
    if (nrow(r) > 0) r$source_note <- paste0("congener:", surrogate)
  }

  # Genus median (e.g. Squalius platyceps -> median over Squalius spp.)
  if (nrow(r) == 0 && sp %in% GENUS_MEDIAN_FOR) {
    genus <- sub("_.*$", "", sp)
    g <- fish_traits %>% filter(sub("_.*$", "", species) == genus, species != sp)
    if (nrow(g) > 0) {
      r <- tibble(
        species         = sp,
        max_TL          = median(g$max_TL, na.rm = TRUE),
        Migration       = as.numeric(names(sort(table(g$Migration), decreasing = TRUE))[1]),
        Migration_label = names(sort(table(g$Migration_label), decreasing = TRUE))[1],
        max_SO          = NA_real_,
        source_note     = paste0("genus_median:", genus)
      )
    }
  }

  if (!"source_note" %in% names(r)) r$source_note <- "predictive_model"

  # Stream order always from this species' own occurrences if available
  so_own <- so_by_species %>% filter(species == sp) %>% pull(max_SO)
  if (length(so_own) == 1 && is.finite(so_own)) r$max_SO <- so_own
  if (is.na(r$max_SO) | !is.finite(r$max_SO))   r$max_SO <- SO_DEFAULT

  r
}

rows <- lapply(TARGET_SPECIES, function(sp) {
  tr <- trait_row(sp)
  sigma <- predict_sigma(tr$max_TL, tr$max_SO)
  sigma_capped <- min(sigma, SIGMA_MOB_CAP_M)

  data.table(
    species         = sp,
    distance        = round(sigma_capped, 1),
    Migration_label = tr$Migration_label,
    max_TL          = tr$max_TL,
    max_SO          = tr$max_SO,
    source          = tr$source_note,
    capped          = sigma > SIGMA_MOB_CAP_M
  )
})

fish_dis_class <- rbindlist(rows)

# 3-tier dispersal_prob from migratory class (for reference / Module 11 parity)
fish_dis_class <- fish_dis_class %>%
  mutate(dispersal_prob = case_when(
    Migration_label == "Non-migratory" ~ 0.3,
    Migration_label == "Potamodromous" ~ 0.6,
    Migration_label == "Long_distance" ~ 0.9,
    TRUE                               ~ NA_real_
  )) %>%
  select(species, distance, dispersal_prob, Migration_label,
         max_TL, max_SO, source, capped) %>%
  arrange(species) %>%
  as.data.table()

print(fish_dis_class)

fwrite(fish_dis_class[, .(species, distance)],
       "traits/fish_dispersal_distance.txt", sep = "\t")
message("\nSaved: traits/fish_dispersal_distance.txt")
message("Read by 08_habitat_classification.R via the `distance` column.")
