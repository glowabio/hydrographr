# =============================================================================
# Reconstruct true absences and check species overlap across basins
# =============================================================================

library(dplyr)
library(tidyr)

# --- Load files ---
occurr      <- read.csv("points_snapped/fish/occurr_for_sdm.csv")
sites       <- read.csv("points_snapped/all_snapped_with_basins.csv") %>%
  filter(source %in% c("HCMR", "GBIF")) %>%
  filter(subc_id %in% sites$subc_id) %>%
  select(subc_id, basin_id) %>% distinct()

# --- Join basin_id to occurrences via subc_id ---
occurr <- occurr %>%
  left_join(sites, by = "subc_id")

# Sanity check: any occurrences without a basin_id after join?
cat("Occurrences missing basin_id:", sum(is.na(occurr$basin_id)), "\n")

# =============================================================================
# Reconstruct true absences for HCMR sites
# Logic: for every HCMR site, any species NOT recorded there is truly absent
# This does NOT apply to GBIF records
# =============================================================================

# All HCMR sites with their basin
hcmr_sites <- occurr %>%
  filter(source == "HCMR") %>%
  select(site_id, subc_id, basin_id) %>%
  distinct()

# All species recorded at least once in HCMR data
hcmr_species <- occurr %>%
  filter(source == "HCMR") %>%
  distinct(species)

# Full crossed table: every HCMR site x every species
hcmr_full <- hcmr_sites %>%
  cross_join(hcmr_species) %>%
  # join back presences
  left_join(
    occurr %>% filter(source == "HCMR") %>% select(site_id, species) %>% mutate(presence = 1),
    by = c("site_id", "species")
  ) %>%
  mutate(presence = replace_na(presence, 0))

# Quick check
cat("Total HCMR site x species combinations:", nrow(hcmr_full), "\n")
cat("  Presences:", sum(hcmr_full$presence == 1), "\n")
cat("  True absences:", sum(hcmr_full$presence == 0), "\n")

# =============================================================================
# Species overlap check
# =============================================================================

sarantaporos_basin_id <- 1292502

# Species present in Sarantaporos (HCMR presences only)
species_sarant <- hcmr_full %>%
  filter(basin_id == sarantaporos_basin_id, presence == 1) %>%
  distinct(species) %>%
  pull(species)

# Species present in other Greek basins (HCMR presences only)
species_greece_other <- hcmr_full %>%
  filter(basin_id != sarantaporos_basin_id, presence == 1) %>%
  distinct(species) %>%
  pull(species)

# Species in GBIF
species_gbif <- occurr %>%
  filter(source == "GBIF") %>%
  distinct(species) %>%
  pull(species)

# --- Summary counts ---
cat("\n--- Species overlap summary ---\n")
cat("Species with HCMR presences in Sarantaporos:      ", length(species_sarant), "\n")
cat("Species with HCMR presences elsewhere in Greece:  ", length(species_greece_other), "\n")
cat("Species in GBIF:                                   ", length(species_gbif), "\n")
cat("Overlap Sarantaporos + Greece-wide HCMR:           ",
    length(intersect(species_sarant, species_greece_other)), "\n")
cat("Only in Sarantaporos (no HCMR elsewhere):          ",
    length(setdiff(species_sarant, species_greece_other)), "\n")

# --- Per-species breakdown table ---
all_species <- union(species_sarant, species_greece_other)

species_summary <- tibble(species = all_species) %>%
  mutate(
    in_sarantaporos    = species %in% species_sarant,
    in_greece_hcmr     = species %in% species_greece_other,
    in_gbif            = species %in% species_gbif,
    # true absences available in Sarantaporos for this species?
    true_abs_sarant    = species %in% (hcmr_full %>%
                                         filter(basin_id == sarantaporos_basin_id, presence == 0) %>%
                                         pull(species)),
    data_situation = case_when(
      in_sarantaporos & in_greece_hcmr  ~ "HCMR presences in both — true absences usable range-wide",
      in_sarantaporos & !in_greece_hcmr & in_gbif  ~ "Sarantaporos HCMR + GBIF only — true absences range-restricted",
      in_sarantaporos & !in_greece_hcmr & !in_gbif ~ "Sarantaporos HCMR only — consider basin-level model only",
      !in_sarantaporos & in_greece_hcmr ~ "Greece HCMR only — no Sarantaporos true absences",
      TRUE ~ "check"
    )
  )

print(species_summary, n = Inf)

# Count per situation — this is your decision table
species_summary %>% count(data_situation) %>% arrange(desc(n))
