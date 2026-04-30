#!/bin/bash
# =============================================================================
# reorganise_workflow.sh
# Reorganise workflow scripts into new folder structure and update
# internal path/name references where possible.
# Run from the workflows/ directory.
# =============================================================================

set -e  # exit on error

echo "=== Creating new folder structure ==="

# mkdir -p 01_data_preparation
# mkdir -p 02_snapping
# mkdir -p 03_spatial_network
# mkdir -p 04_connectivity
# mkdir -p 05_traits
# mkdir -p 06_sdm
# mkdir -p 07_visualization
# mkdir -p helpers
# mkdir -p archive

# echo "=== Moving data preparation scripts ==="
# # Already in correct folder — just rename to match new numbering
# git mv 01_data_preparation/01_clean_hcmr_fish_data.R  01_data_preparation/01_clean_hcmr_fish.R
# git mv 01_data_preparation/02_download_data_gbif.R     01_data_preparation/02_download_gbif_fish.R
# git mv 01_data_preparation/03_clean_gbif_data.R        01_data_preparation/03_clean_gbif_fish.R
# git mv 01_data_preparation/04_clean_all_dam_data.R     01_data_preparation/04_clean_dam_data.R

# echo "=== Moving snapping scripts ==="
# git mv 03_snapping/join_spnames_with_locations.R  02_snapping/02_join_spnames_with_locations.R
# git mv 03_snapping/snap_all_data.R                02_snapping/01_snap_all_data.R

# echo "=== Moving spatial network scripts ==="
# git mv 04_spatial_network/01_extract_subbasin.R          03_spatial_network/01_extract_subbasin.R
# git mv 04_spatial_network/02_generate_network_graph.R     03_spatial_network/02_generate_network_graph.R
# git mv 04_spatial_network/03_overlay_with_basin_names.R   03_spatial_network/03_overlay_with_basin_names.R
# git mv 04_spatial_network/04_iucn_range_map_overlay.R     03_spatial_network/04_iucn_range_map_overlay.R

# echo "=== Moving connectivity scripts ==="
# git mv 05_connectivity/01_pci_calculation.R              04_connectivity/01_pci_calculation.R
# git mv 05_connectivity/01b_pci_plots.R                   04_connectivity/01b_pci_plots.R
# git mv 05_connectivity/01_pci_calculation_subhabitats.R  04_connectivity/01c_pci_calculation_subhabitats.R
# git mv 05_connectivity/02_fragmentation_scenarios.R      04_connectivity/02_fragmentation_scenarios.R
# git mv 05_connectivity/02_fragmentation_ALL_species.R    04_connectivity/02b_fragmentation_all_species.R
# git mv 05_connectivity/03_dam_impact_zones.R             04_connectivity/03_dam_impact_zones.R
# git mv 05_connectivity/04_combined_visualizations.R      04_connectivity/04_combined_visualizations.R
# git mv 05_connectivity/get_subgraph_between_points.R     04_connectivity/05_get_subgraph_between_points.R
# git mv 05_connectivity/visualizations_ALL.R              04_connectivity/06_visualizations_all.R
# git mv 05_connectivity/old/stream_fragmentation.R        archive/stream_fragmentation.R

# echo "=== Moving traits scripts ==="
# git mv 06_traits/01_download_traits.R           05_traits/01_download_traits.R
# git mv 06_traits/02_dispersal_classification.R  05_traits/02_dispersal_classification.R
# git mv 06_traits/measure_car.R                  05_traits/03_measure_car.R
# # Non-R files
# git mv 06_traits/methods_PCI                          05_traits/methods_PCI
# git mv 06_traits/recommendation_single_subc_species.txt  05_traits/recommendation_single_subc_species.txt

# echo "=== Moving SDM scripts ==="
# git mv 07_sdm/01_download_env90m_data.R    06_sdm/01_download_env90m_data.R
# git mv 07_sdm/02_create_prediction_table.R 06_sdm/02_create_prediction_table.R
# git mv 07_sdm/03_check_multicollinearity.R 06_sdm/03_check_multicollinearity.R
# git mv 07_sdm/ssn.R                        06_sdm/04_ssn_models.R
# # Likely redundant — archive for review
# git mv 07_sdm/check_splist.R               archive/check_splist.R
# git mv 07_sdm/create_predict_table.R       archive/create_predict_table_old.R

# echo "=== Moving visualization scripts ==="
# git mv 08_visualization/create_maps.R  07_visualization/01_create_maps.R

# echo "=== Helpers stay in place ==="
# # helpers/ folder keeps the same name and location — no moves needed

echo "=== Removing now-empty old folders ==="
rmdir 03_snapping       2>/dev/null || true
rmdir 04_spatial_network 2>/dev/null || true
rmdir 05_connectivity/old 2>/dev/null || true
rmdir 05_connectivity    2>/dev/null || true
rmdir 06_traits          2>/dev/null || true
rmdir 07_sdm             2>/dev/null || true
rmdir 08_visualization   2>/dev/null || true

# echo ""
echo "=== Updating internal LOCATION comments in scripts ==="
# Update the # LOCATION: header comment in each script

declare -A location_map=(
  ["01_data_preparation/01_clean_hcmr_fish.R"]="workflows/01_data_preparation/01_clean_hcmr_fish.R"
  ["01_data_preparation/02_download_gbif_fish.R"]="workflows/01_data_preparation/02_download_gbif_fish.R"
  ["01_data_preparation/03_clean_gbif_fish.R"]="workflows/01_data_preparation/03_clean_gbif_fish.R"
  ["01_data_preparation/04_clean_dam_data.R"]="workflows/01_data_preparation/04_clean_dam_data.R"
  ["02_snapping/01_snap_all_data.R"]="workflows/02_snapping/01_snap_all_data.R"
  ["02_snapping/02_join_spnames_with_locations.R"]="workflows/02_snapping/02_join_spnames_with_locations.R"
  ["03_spatial_network/01_extract_subbasin.R"]="workflows/03_spatial_network/01_extract_subbasin.R"
  ["03_spatial_network/02_generate_network_graph.R"]="workflows/03_spatial_network/02_generate_network_graph.R"
  ["03_spatial_network/03_overlay_with_basin_names.R"]="workflows/03_spatial_network/03_overlay_with_basin_names.R"
  ["03_spatial_network/04_iucn_range_map_overlay.R"]="workflows/03_spatial_network/04_iucn_range_map_overlay.R"
  ["04_connectivity/01_pci_calculation.R"]="workflows/04_connectivity/01_pci_calculation.R"
  ["04_connectivity/01b_pci_plots.R"]="workflows/04_connectivity/01b_pci_plots.R"
  ["04_connectivity/01c_pci_calculation_subhabitats.R"]="workflows/04_connectivity/01c_pci_calculation_subhabitats.R"
  ["04_connectivity/02_fragmentation_scenarios.R"]="workflows/04_connectivity/02_fragmentation_scenarios.R"
  ["04_connectivity/02b_fragmentation_all_species.R"]="workflows/04_connectivity/02b_fragmentation_all_species.R"
  ["04_connectivity/03_dam_impact_zones.R"]="workflows/04_connectivity/03_dam_impact_zones.R"
  ["04_connectivity/04_combined_visualizations.R"]="workflows/04_connectivity/04_combined_visualizations.R"
  ["04_connectivity/05_get_subgraph_between_points.R"]="workflows/04_connectivity/05_get_subgraph_between_points.R"
  ["04_connectivity/06_visualizations_all.R"]="workflows/04_connectivity/06_visualizations_all.R"
  ["05_traits/01_download_traits.R"]="workflows/05_traits/01_download_traits.R"
  ["05_traits/02_dispersal_classification.R"]="workflows/05_traits/02_dispersal_classification.R"
  ["05_traits/03_measure_car.R"]="workflows/05_traits/03_measure_car.R"
  ["06_sdm/01_download_env90m_data.R"]="workflows/06_sdm/01_download_env90m_data.R"
  ["06_sdm/02_create_prediction_table.R"]="workflows/06_sdm/02_create_prediction_table.R"
  ["06_sdm/03_check_multicollinearity.R"]="workflows/06_sdm/03_check_multicollinearity.R"
  ["06_sdm/04_ssn_models.R"]="workflows/06_sdm/04_ssn_models.R"
  ["07_visualization/01_create_maps.R"]="workflows/07_visualization/01_create_maps.R"
)

for file in "${!location_map[@]}"; do
  new_location="${location_map[$file]}"
  if [ -f "$file" ]; then
    sed -i "s|# LOCATION:.*|# LOCATION: $new_location|" "$file"
    echo "  Updated LOCATION in: $file"
  fi
done

echo ""
echo "=== Updating source() paths in scripts ==="
# Update source() calls that reference helpers by old relative paths
# These patterns cover the most common cases — review manually after

for file in $(find . -name "*.R" -not -path "./archive/*"); do
  # source() calls to helpers — adjust relative depth
  sed -i 's|source("helpers/config.R")|source("helpers/config.R")|g' "$file"
  sed -i 's|source("helpers/save_to_nimbus.R")|source("helpers/save_to_nimbus.R")|g' "$file"
  sed -i 's|source("helpers/pci_sparse.R")|source("helpers/pci_sparse.R")|g' "$file"
  # Old absolute paths with script names — update script name references
  sed -i 's|01_clean_hcmr_fish_data\.R|01_clean_hcmr_fish.R|g' "$file"
  sed -i 's|02_download_data_gbif\.R|02_download_gbif_fish.R|g' "$file"
  sed -i 's|03_clean_gbif_data\.R|03_clean_gbif_fish.R|g' "$file"
  sed -i 's|04_clean_all_dam_data\.R|04_clean_dam_data.R|g' "$file"
done

echo ""
echo "=== Committing reorganisation ==="
git add -A
git commit -m "refactor: reorganise workflow scripts into numbered folder structure

- Renumber folders: 03_snapping→02, 04_spatial→03, 05_connectivity→04,
  06_traits→05, 07_sdm→06, 08_visualization→07
- Rename scripts for consistency (remove redundant words, lowercase)
- Archive redundant/old scripts to archive/
- Update LOCATION comments in all script headers
- Update script name references in sed-patchable locations

Manual review still needed for:
- source() calls with absolute paths
- Any cross-script references not caught by sed"

echo ""
echo "=== Done ==="
echo "Please manually review:"
echo "  - Absolute source() paths (grep -r 'source(' . to find them)"
echo "  - Any script that calls another script by name"
echo "  - archive/ folder contents — delete if confirmed redundant"