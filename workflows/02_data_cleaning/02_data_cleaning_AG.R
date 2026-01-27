---
  title: "GBIF Occurrence Data Cleaning Workflow"
---
# Introduction: this vignette presents a reproducible workflow to clean, spatially validate, and taxonomically check freshwater fish occurrence records from the Global Biodiversity Information Facility (GBIF). The goal is to produce a cleaned dataset suitable for downstream analyses such as species distribution modeling.
#The final output will be saved as `gbif_data_cleaned.csv`.

###Setting the scene
#We begin by loading all necessary packages for data manipulation, spatial filtering, and taxonomy checks.

devtools::install_github("ytorres-cambas/danubeoccurR")
library(data.table)
library(dplyr)
library(tidyr)
library(CoordinateCleaner)
library(specleanr)
library(rfishbase)
library(devtools)
library(danubeoccurR)
library(readr)
library(leaflet)

#2.Reading the raw GBIF data
#We use fread() to import the occurrence CSV file efficiently
#This avoids loading unnecessary columns into memory and handles large datasets smoothly

file_raw <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data/points_original/combined_greece_fish_occurrences.csv"

gbif <- fread(file_raw,
              quote = "\"",
              fill = Inf)

# Keep only rows with correct number of columns (no V columns)
v_cols <- grep("^V[0-9]+$", names(gbif), value = TRUE)

if (length(v_cols) > 0) {
  gbif_tmp <- gbif[rowSums(gbif[, ..v_cols] != "", na.rm = TRUE) == 0]
  gbif_tmp <- gbif_tmp[, !..v_cols]  # Drop V columns
} else {
  gbif_tmp <- gbif
}

# check problematic lon values
gbif_tmp %>%
  filter(!is.na(decimalLongitude)) %>%
  select(decimalLongitude) %>%
  arrange(desc(decimalLongitude)) %>% head()

# exclude these rows
gbif_tmp <- gbif_tmp %>% filter(decimalLongitude<20000)

# Save tmp file to read again
fwrite(gbif_tmp, "~/combined_greece_fish_occurrences_tmp.csv")

message(sprintf("Saved %d clean rows (excluded %d malformed)",
                nrow(gbif_tmp),
                nrow(gbif) - nrow(gbif_tmp)))

# read with read_csv that trackes problematic rows
gbif_tmp <- read_csv("~/combined_greece_fish_occurrences_tmp.csv")

# get problematic row numbers
prob_rows <- problems(gbif_tmp)$row
# remove these rows
gbif_clean <- gbif_tmp[-prob_rows,]

# write out
fwrite(gbif_clean, file.path(nimbus_path, "points_original/combined_greece_fish_occurrences_clean.csv"))

# read in again to detect problems
fin <- read_csv(file.path(nimbus_path, "points_original/combined_greece_fish_occurrences_clean.csv"))
#no more problems hopefully
#remove tmp file
file.remove("~/combined_greece_fish_occurrences_tmp.csv")

# unique locations to snap
# fin <- fin %>%
#   distinct(decimalLatitude,decimalLongitude, .keep_all = TRUE) %>%
#   select(gbifID, decimalLongitude, decimalLatitude)


# gbif_raw <- read_csv(
#   "~/combined_greece_fish_occurrences.csv",
#   col_types = cols(.default = col_character()),
#   locale = locale(encoding = "UTF-8"),
#   show_col_types = FALSE,
#   progress = FALSE
# )


gbif_raw <- fin

head(gbif_raw)
glimpse(gbif_raw)

#3. Initial exploration and duplicates
#Here we examine if there are duplicate records based on coordinates and key taxonomic fields.

gbif_raw %>%
  count(decimalLongitude, decimalLatitude, speciesKey, datasetKey, year) %>%
  filter(n > 1)

#4. Coordinate type conversion
#Coordinates often come as character vectors. We convert them to numeric for filtering and cleaning operations

gbif_raw <- gbif_raw %>%
  mutate(
    decimalLatitude  = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude)
  )

#5. Metadata-based filtering
#We apply basic metadata filters to remove records with missing year information, low precision, or high coordinate uncertainty

gbif_metadata_filtered <- gbif_raw %>%
  filter(!is.na(year)) %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999))

#6. CoordinateCleaner spatial filters
#We use the CoordinateCleaner package to remove problematic coordinates such as country centroids, capitals, and institutional locations
#We also remove spatial duplicates

gbif_spatial_cleaned <- gbif_metadata_filtered %>%
  cc_cen(buffer = 1000) %>%
  cc_cap(buffer = 1000) %>%
  cc_inst(buffer = 1000) %>%
  distinct(
    decimalLongitude,
    decimalLatitude,
    speciesKey,
    datasetKey,
    year,
    .keep_all = TRUE
  ) %>%
  filter(species != "")

#7. Taxonomic Name Checking
#We validate species names against a curated reference (e.g., FishBase) using the check_species_name function

gbif_taxa_checked <- check_species_name(
  data = gbif_spatial_cleaned,
  col_species_name = "species",
  target_accuracy = 90,
  accuracy_decrement = NULL,
  verbose = TRUE,
  manual = TRUE
)

glimpse(gbif_taxa_checked)

#8. Manual curation
#Certain taxa that remain unresolved or non-specific (e.g., genus-only records) are manually excluded

gbif_taxa_curated <- gbif_taxa_checked[-c(5836:5840), ]
glimpse(gbif_taxa_curated)

#9. Final cleaning and selection
#We standardize missing values, select only relevant columns, and remove records missing coordinates

gbif_cleaned <- gbif_taxa_curated %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  select(
    gbifID,
    datasetKey,
    class,
    order,
    family,
    genus,
    species,
    year,
    month,
    day,
    decimalLongitude,
    decimalLatitude
  ) %>%
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  )

glimpse(gbif_cleaned)

# visualise
leaflet(gbif_cleaned) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~decimalLongitude,
    lat = ~decimalLatitude,
    radius = 3,
    color = "red",
    fillOpacity = 0.5)


#10. Exporting the cleaned dataset
#We write out the final cleaned dataset once

fwrite(gbif_cleaned,
  file.path(nimbus_path, "points_original/GBIF_data_cleaned_afroditi.csv"),
  row.names = FALSE
)


