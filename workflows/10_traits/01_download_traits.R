


# install.packages("fwtraits")
library(fwtraits)
library(tidyverse)
library(data.table)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- BASE_DIR
setwd(BASE_DIR)

# ============================================================
# STEP 1: Authenticate with freshwaterecology.info
# ============================================================

# First time: register at https://www.freshwaterecology.info/
# and request an API key from the database managers.
# Store your API key in .Renviron:
#   fw_setapikey()
#   Add line: API_KEY=your_api_key_here
#   Then restart R

fw_be4ustart()
fw_setapikey()  # will prompt for API key or read from .Renviron

# ============================================================
# STEP 2: Check what parameters are available for fish
# ============================================================

# Get the database guide — lists all organism groups and parameters
db_guide <- fw_dbguide()

# Filter to fish parameters only
fish_params <- db_guide %>%
  filter(grepl("fi", organismgroup, ignore.case = TRUE))

cat("Available fish parameters:", nrow(fish_params), "\n")
print(fish_params %>% select(parameters_cleaned, DataType) %>% distinct())

# ============================================================
# STEP 3: Prepare species list
# ============================================================

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# fwtraits expects "Genus species" format (with space)
species_list <- data.frame(
  scientificName = gsub("_", " ", unique(occurrences$species))
)

cat("Species to query:", nrow(species_list), "\n")

# ============================================================
# STEP 4: Fetch traits — key ecological parameters
# ============================================================

# Parameters relevant for connectivity/fragmentation analysis:
# - rheophily habitat: flow preference (rheophilic vs limnophilic)
# - migration: migratory behaviour
# - spawning habitat: reproductive strategy
# - feeding diet adult: trophic guild
# - temperature tolerance: thermal sensitivity
# - body size: max body length

# Fetch traits
fish_traits_fwe <- fw_fetchdata(
  data = species_list,
  ecoparams = c(
    "rheophily habitat",
    "migration",
    "spawning habitat",
    "feeding diet adult",
    "temperature tolerance",
    "body size"
  ),
  taxonomic_column = "scientificName",
  organismgroup = "fi"
)

cat("\nTraits retrieved for", length(unique(fish_traits_fwe$taxasearched$clean)), "species\n")

# ============================================================
# STEP 5: Check coverage — which species have data?
# ============================================================

# How many species have each trait?
trait_coverage <- fish_traits_fwe$ecodata %>%
  group_by(Parameter) %>%
  summarize(
    n_species_with_data = sum(!is.na(value)),
    n_species_total = n(),
    pct_coverage = round(100 * sum(!is.na(value)) / n(), 1),
    .groups = "drop"
  )

cat("\nTrait coverage:\n")
print(trait_coverage)

# Which species have NO data at all?
species_with_data <- fish_traits_fwe %>%
  filter(!is.na(value)) %>%
  pull(scientificName) %>%
  unique()

species_missing <- setdiff(species_list$scientificName, species_with_data)
cat("\nSpecies with no data in freshwaterecology.info:", length(species_missing), "\n")
if (length(species_missing) > 0) print(species_missing)

# ============================================================
# STEP 6: Also try fetching ALL available fish parameters
# ============================================================

# Get all parameter names for fish
all_fish_params <- fish_params$parameter_cleaned

# Fetch everything (may take a while)
fish_traits_all <- fw_fetchdata(
  data = species_list,
  ecoparams = all_fish_params,
  taxonomic_column = "scientificName",
  organismgroup = "fi"
)

# Summarize what we got
all_coverage <- fish_traits_all %>%
  filter(!is.na(value)) %>%
  group_by(parameter) %>%
  summarize(n_species = n_distinct(scientificName), .groups = "drop") %>%
  arrange(desc(n_species))

cat("\nAll parameters with data (sorted by coverage):\n")
print(all_coverage, n = 30)

# ============================================================
# STEP 7: Save
# ============================================================

fwrite(fish_traits_fwe, "traits/fwtraits_fish_greece.csv")
fwrite(fish_traits_all, "traits/fwtraits_fish_greece_all.csv")
fwrite(as.data.frame(trait_coverage), "traits/fwtraits_coverage_summary.csv")

cat("\nSaved:\n")
cat("  traits/fwtraits_fish_greece.csv (selected parameters)\n")
cat("  traits/fwtraits_fish_greece_all.csv (all parameters)\n")
cat("  traits/fwtraits_coverage_summary.csv\n")





migration <- fw_fetchdata(data = 'Abramis brama',
                          organismgroup = 'fi',
                          ecoparams = 'migration',
                          cachefolder = 'cache',
                          warn = TRUE,
                          inform = TRUE,
                          details = TRUE)#the species spelling is checked
