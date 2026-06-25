#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_dispersal_estimation.R
#
# Assign a species-specific dispersal probability to each of the seven
# Sarantaporos fish species, for use as the dispersal parameter in the
# Population Connectivity Index (PCI; Module: Connectivity).
#
# APPROACH
#   The PCI uses dispersal probability only through its RELATIVE ranking
#   across species (B_ij = dispersal_prob ^ distance). We therefore do not
#   estimate physical dispersal distances. Instead we assign three ordinal
#   dispersal tiers from the migratory class of each species, the most
#   defensible dispersal signal available for this small, partly endemic
#   species set:
#       migration 0 (non-migratory)   -> dispersal_prob 0.3
#       migration 1 (potamodromous)   -> dispersal_prob 0.6
#       migration 2 (long-distance)   -> dispersal_prob 0.9
#   Body size (max_TL) is retained in the output for description only; it
#   does not enter the dispersal probability, because a within-tier body
#   size difference is not a documented dispersal-probability difference
#   at this scale.
#
#   This is a simplification of the seven-quantile fishmove scheme of
#   Sun et al. (2025), which was designed for a continental species pool;
#   for seven species the quantile binning is degenerate (~one species per
#   class) and adds arbitrary breakpoints without ecological content.
#
# TRAIT SUBSTITUTIONS (also stated in the manuscript)
#   Two of the seven species are absent from the trait table and take a
#   surrogate. Both surrogates are potamodromous (migration 1), so the
#   assigned tier is robust to the exact surrogate chosen:
#     - Chondrostoma ohridanum -> traits of Chondrostoma vardarense
#                                 (in-table congener)
#     - Squalius platyceps     -> genus-level median across all Squalius
#                                 species in the trait table
#
# REQUIRES
#   tidyverse, data.table, readxl
#
# INPUT
#   points_original/fish/species_list_sarantaporos.txt   (species column)
#   points_original/fish/Fish distributional & traits data (1).xlsx
#                                                        (sheet "Traits")
#   points_snapped/fish/fish_all_species_snapped.csv     (optional; for max_SO)
#
# OUTPUT
#   traits/fish_dis_class.txt
#     columns: species, max_TL, max_SO, Migration, Migration_label,
#              dispersal_prob, source
#
# LOCATION
#   workflows/<connectivity_module>/01_dispersal_estimation.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(data.table)
library(readxl)

# ============================================================
# FIX: prevent MASS::select etc. from masking dplyr verbs
# ============================================================
select   <- dplyr::select
rename   <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- "/home/grigoropoulou/Documents/Postdoc/projects/workflow_paper/data"
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
TRAITS_XLSX  <- "points_original/fish/Fish distributional & traits data (1).xlsx"
SPECIES_LIST <- "points_original/fish/species_list_sarantaporos.txt"
OCCURRENCES  <- "points_snapped/fish/fish_all_species_snapped.csv"

# Migration tier -> dispersal probability (the only ecological claim here:
# three ordinal tiers; values follow the 0.3-0.9 range of Sun et al. 2025)
MIGRATION_TO_PD <- c("0" = 0.3, "1" = 0.6, "2" = 0.9)

# Migration code -> label (legend of the trait table)
MIGRATION_LABELS <- c("0" = "Non-migratory",
                      "1" = "Potamodromous",
                      "2" = "Long-distance")

# Documented trait surrogates (target species -> donor species in the table)
SUBSTITUTE_CONGENER <- c("Chondrostoma_ohridanum" = "Chondrostoma_vardarense")
SUBSTITUTE_GENUS_MEDIAN <- "Squalius_platyceps"   # uses median over genus

# ============================================================
# STEP 1: Read and clean the Sarantaporos species list
#   The list contains a known duplicate/typo for Chondrostoma
#   (ohridana / ohridanum); we keep the "_ohridanum" form only.
# ============================================================
species_list <- fread(SPECIES_LIST, header = TRUE) %>%
  dplyr::mutate(species = gsub(" ", "_", species)) %>%
  dplyr::filter(species != "Chondrostoma_ohridana") %>%   # drop the typo variant
  dplyr::distinct(species) %>%
  dplyr::arrange(species)

message("Species in Sarantaporos list: ", nrow(species_list))
print(species_list$species)

# ============================================================
# STEP 2: Read the trait table (sheet "Traits")
# ============================================================
traits_raw <- read_xlsx(TRAITS_XLSX, sheet = "Traits") %>%
  dplyr::rename(species = Species) %>%
  dplyr::mutate(species = gsub(" ", "_", species)) %>%
  dplyr::select(species, max_TL, Migration)

# ============================================================
# STEP 3: Resolve traits for each target species
#   (a) direct match in the table
#   (b) congener surrogate (Chondrostoma ohridanum -> vardarense)
#   (c) genus-median surrogate (Squalius platyceps)
# ============================================================

# (c) genus-median surrogate values (computed before per-species resolution)
squalius_median <- traits_raw %>%
  dplyr::filter(grepl("^Squalius", species)) %>%
  dplyr::summarise(
    max_TL    = median(max_TL,   na.rm = TRUE),
    Migration = median(Migration, na.rm = TRUE)
  )

resolve_traits <- function(sp) {

  # (b) congener surrogate
  if (sp %in% names(SUBSTITUTE_CONGENER)) {
    donor <- SUBSTITUTE_CONGENER[[sp]]
    row <- traits_raw %>% dplyr::filter(species == donor)
    if (nrow(row) == 1) {
      return(tibble(species = sp,
                    max_TL = row$max_TL, Migration = row$Migration,
                    source = paste0("substitute: ", donor)))
    }
  }

  # (c) genus-median surrogate
  if (sp == SUBSTITUTE_GENUS_MEDIAN) {
    return(tibble(species = sp,
                  max_TL = squalius_median$max_TL,
                  Migration = squalius_median$Migration,
                  source = "substitute: Squalius genus median"))
  }

  # (a) direct match
  row <- traits_raw %>% dplyr::filter(species == sp)
  if (nrow(row) == 1) {
    return(tibble(species = sp,
                  max_TL = row$max_TL, Migration = row$Migration,
                  source = "trait table"))
  }

  # not found
  tibble(species = sp, max_TL = NA_real_, Migration = NA_real_,
         source = "MISSING")
}

fish_traits <- purrr::map_dfr(species_list$species, resolve_traits)

# Report substitutions and any unresolved species
subs <- fish_traits %>% dplyr::filter(source != "trait table")
if (nrow(subs) > 0) {
  message("\nTrait surrogates applied (state in manuscript):")
  subs %>% dplyr::select(species, source) %>% as.data.frame() %>% print()
}
missing <- fish_traits %>% dplyr::filter(source == "MISSING")
if (nrow(missing) > 0) {
  warning("Species with no resolvable traits: ",
          paste(missing$species, collapse = ", "))
}

# ============================================================
# STEP 4: Assign dispersal probability from migration tier
# ============================================================
fish_traits <- fish_traits %>%
  dplyr::mutate(
    Migration       = as.integer(round(Migration)),
    Migration_label = MIGRATION_LABELS[as.character(Migration)],
    dispersal_prob  = MIGRATION_TO_PD[as.character(Migration)]
  )

# ============================================================
# STEP 5: Attach max stream order from occurrences (descriptive only)
#   Not used for dispersal_prob; included for figures / labelling.
#   Skipped gracefully if the occurrence file is unavailable.
# ============================================================
if (file.exists(OCCURRENCES)) {
  species_stream_order <- fread(OCCURRENCES) %>%
    dplyr::mutate(species = gsub(" ", "_", species)) %>%
    group_by(species) %>%
    dplyr::summarise(max_SO = max(strahler, na.rm = TRUE),
                     .groups = "drop")

  fish_traits <- fish_traits %>%
    dplyr::left_join(species_stream_order, by = "species")
} else {
  message("\nOccurrence file not found; max_SO set to NA (descriptive only).")
  fish_traits <- fish_traits %>% dplyr::mutate(max_SO = NA_real_)
}

# ============================================================
# STEP 6: Write output
# ============================================================
fish_dis_class <- fish_traits %>%
  dplyr::select(species, max_TL, max_SO, Migration, Migration_label,
                dispersal_prob, source) %>%
  dplyr::arrange(dispersal_prob, species)

dir.create("traits", showWarnings = FALSE, recursive = TRUE)
fwrite(fish_dis_class, "traits/fish_dis_class.txt", sep = "\t")

message("\ntraits/fish_dis_class.txt written")
cat("\n========== DISPERSAL CLASSES ==========\n")
as.data.frame(fish_dis_class) %>% print()

cat("\nDispersal probability distribution:\n")
print(table(fish_dis_class$dispersal_prob))
