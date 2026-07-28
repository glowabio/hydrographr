# Script dependency map

What each script reads, what it writes, and which later scripts consume its
outputs. Paths are relative to the data directory (`WORKFLOW_DATA`; see
`helpers/config.R`).

Use this to work out what must be re-run after changing a script: follow the
"Consumed by" column forward. Modules and scripts otherwise run in numeric
order — see `README.md` for the two exceptions in Module 9.

## Module 01 — `01_biodiversity_data`

Cleans and combines fish occurrence records from two sources (HCMR survey data,
GBIF) into the two datasets the snapping step (Module 3) needs.

| Script | Reads | Writes | Consumed by |
| `01_clean_hcmr_fish.R` | `points_original/fish/fish_distributional_and_traits_data.xlsx` (raw); `points_original/fish/Sarantaporos.xlsx` (raw, optional) | `points_cleaned/fish/fish_basin_hcmr.csv`; `points_cleaned/fish/fish_points_to_snap_hcmr.csv`; `config/study_area_params.csv` (BASIN_ID); `points_cleaned/maps/hcmr_fish_basin_overview.html` (figure) | `fish_basin_hcmr.csv` → `02_download_gbif_fish.R` (this module); `03_snapping/02_join_spnames_with_locations.R`; `03_snapping/04_study_area_map.R`. `fish_points_to_snap_hcmr.csv` → `03_snapping/01_snap_all_data.R`. `study_area_params.csv` → `03_snapping/03_extract_subbasin.R` (BASIN_ID propagates from there, not re-read directly by later modules) |
| `02_download_gbif_fish.R` | `points_cleaned/fish/fish_basin_hcmr.csv` (from `01_clean_hcmr_fish.R`, this module); GBIF API (needs `GBIF_USER`/`GBIF_PWD`/`GBIF_EMAIL` + internet) | `points_original/fish/fish_data_gbif.csv`; `points_original/fish/gbif_citation.txt`; `points_original/fish/{taxon}/*.csv`\*\* (per-taxon extracted archives); `points_original/fish/*.zip`\*\* (raw GBIF archives) | `fish_data_gbif.csv` → `03_clean_gbif_fish.R` (this module) |
| `03_clean_gbif_fish.R` | `points_original/fish/fish_data_gbif.csv` (from `02_download_gbif_fish.R`, this module); `config/study_area_params.csv` (BASIN_ID, from `01_clean_hcmr_fish.R`) | `points_cleaned/fish/fish_gbif_clean.csv`; `points_cleaned/fish/fish_gbif_clean_to_snap.csv`; `points_cleaned/maps/gbif_fish_cleaned_overview.html` (figure) | `fish_gbif_clean.csv` → `03_snapping/02_join_spnames_with_locations.R`; `03_snapping/04_study_area_map.R`. `fish_gbif_clean_to_snap.csv` → `03_snapping/01_snap_all_data.R` |

## Module 02 — `02_barrier_data`

Cleans the RAE small-hydropower licence registers into DAM and FACTORY point
sets for snapping (Module 3). The study area map lives in Module 3
(`03_snapping/04_study_area_map.R`), since it draws on outputs from Modules 1,
2 and 3.

| Script | Reads | Writes | Consumed by |
| `01_clean_dam_data.R` | `points_original/dams/V_SDI_R_HYDRO13_Operational_Licence.csv`; `V_SDI_R_HYDRO12_Installation_Licence.csv`; `V_SDI_R_HYDRO11_Production_Licence.csv`; `V_SDI_R_HYDRO7_Evaluation.csv`; `V_SDI_R_HYDRO_OTHER_VALUES_Rejected.csv` (report-only); `dams_sarantaporos_table.csv` (expert classification, raw) | `points_cleaned/dams/dams_sarantaporos_clean.csv`; `dams_sarantaporos_clean.gpkg`; `dams_classification_full.csv` (audit trail); `factories_sarantaporos_clean.csv`; `points_cleaned/maps/dams_sarantaporos_clean.html` (figure) | `dams_sarantaporos_clean.csv` → `03_snapping/01_snap_all_data.R`; `03_snapping/04_study_area_map.R`. `factories_sarantaporos_clean.csv` → `03_snapping/01_snap_all_data.R`. `dams_classification_full.csv`, `.gpkg`, map HTML → not read by any script (audit trail / GIS / figure only; `.gpkg` referenced only in a commented-out block in `09_spatial_prioritization/02_planned_dam_ranking.R`) |

## Module 03 — `03_snapping`

Snaps fish/dam/factory points to the network, re-attaches species names, and
extracts the basin (training) and Sarantaporos sub-basin (prediction)
extents. Includes `04_study_area_map.R`, moved in from `02_barrier_data`.

| Script | Reads | Writes | Consumed by |
| `01_snap_all_data.R` | `points_cleaned/fish/fish_points_to_snap_hcmr.csv` (Module 1); `fish_gbif_clean_to_snap.csv` (Module 1); `points_cleaned/dams/dams_sarantaporos_clean.csv`, `factories_sarantaporos_clean.csv` (Module 2); GeoFRESH snapping API | `points_snapped/fish/all_snapped_fish_points.csv`; `points_snapped/dams/dams_snapped_points.csv`, `factories_snapped_points.csv`; `points_snapped/maps/{hcmr,gbif,dams,factories}_snapping_check.html`; `points_cleaned/{fish,dams}/*_failed_to_snap.csv` (if any fail) | `all_snapped_fish_points.csv` → `02_join_spnames_with_locations.R` (this module). `dams_snapped_points.csv` → `03_extract_subbasin.R` (this module); `09_spatial_prioritization/02_planned_dam_ranking.R`. `factories_snapped_points.csv` → `08_habitat_fragmentation/00_compute_diversion_length.R`. Maps/failed-to-snap files → not read by any script (QA only) |
| `02_join_spnames_with_locations.R` | `points_cleaned/fish/fish_basin_hcmr.csv` (Module 1); `fish_gbif_clean.csv` (Module 1); `points_snapped/fish/all_snapped_fish_points.csv` (this module) | `points_snapped/fish/fish_hcmr_with_species_snapped.csv`; `fish_gbif_with_species_snapped.csv`; `fish_all_species_snapped.csv` (hub file) | `fish_hcmr_with_species_snapped.csv`, `fish_gbif_with_species_snapped.csv` → `10_traits/02_functional_trait_analysis.R`. `fish_all_species_snapped.csv` → `03_extract_subbasin.R` (this module); `06_env_space/01_env_space.R`; `07_sdm/03b_prepare_sdm_data.R`; `07_sdm/07b_dispersal_distance.R`; `10_traits/01_download_traits.R`; `11_pci/01_dispersal_estimation.R`; `11_pci/02_pci_calculation.R` |
| `03_extract_subbasin.R` | `points_snapped/fish/fish_all_species_snapped.csv`, `points_snapped/dams/dams_snapped_points.csv` (this module); `config/study_area_params.csv` (BASIN_ID, Module 1); `points_original/fish/species_list_sarantaporos.txt` (raw, target species) | `spatial/basin/{basin_polygon,stream_network,stream_network_pruned}.gpkg`, `basin_subc_ids_pruned.csv`; `spatial/subbasin_sarantaporos/{subbasin_polygon,stream_network,stream_network_pruned}.gpkg`, `subbasin_subc_ids_pruned.csv`; `points_snapped/subbasin_sarantaporos/{fish_subbasin,dams_subbasin,fish_sdm_subbasin,species_coverage_summary}.csv`; `points_snapped/basin/fish_sdm_basin.csv`; `figures/sarantaporos_map_prunning.png` | Basin/subbasin polygon+network files → read throughout Modules 4-9 and 12 (network graph, fragmentation, SDM, spatial prioritization, lake catchments — see those modules' passes for the full list). `fish_subbasin.csv`/`dams_subbasin.csv` → `07_sdm/05_maxent.R`; `08_habitat_fragmentation/{01_habitat_fragmentation_metrics,02_habitat_fragmentation_figures}.R`. `species_coverage_summary.csv`/pruning figure → not read by any script (QA only) |
| `04_study_area_map.R` | `points_cleaned/fish/fish_basin_hcmr.csv`, `fish_gbif_clean.csv` (Module 1); `points_cleaned/dams/dams_sarantaporos_clean.csv` (Module 2); `spatial/subbasin_sarantaporos/subbasin_polygon.gpkg`, `spatial/basin/basin_polygon.gpkg` (this module) | `figures/study_area_map.pdf` | Not read by any script (final publication figure) |

## Module 04 — `04_network_analyses`

Builds the igraph river-network graph (current/future barrier scenarios) and
computes topology-only centrality/structure metrics.

| Script | Reads | Writes | Consumed by |
| `01_generate_network_graph.R` | `spatial/subbasin_sarantaporos/stream_network_pruned.gpkg` (Module 3); `spatial/basin/stream_network_pruned.gpkg` (Module 3, reach length only); `points_snapped/dams/dams_snapped_points.csv` (Module 3) | `spatial/stream_network_graphs/river_graph_current.RDS`, `river_graph_future.RDS` | `river_graph_current.RDS`/`river_graph_future.RDS` → `02_centrality.R` (this module); `05_network_fragmentation/{01_network_fragmentation,02_network_fragmentation_map}.R`; `08_habitat_fragmentation/{00_compute_diversion_length,01_habitat_fragmentation_metrics}.R`; `09_spatial_prioritization/{01_spatial_prioritization,02_planned_dam_ranking}.R`; `11_pci/02_pci_calculation.R`; `helpers/get_subgraph_between_points.R` |
| `02_centrality.R` | `spatial/stream_network_graphs/river_graph_current.RDS` (this module); `spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv`, `stream_network_pruned.gpkg`, `subbasin_polygon.gpkg` (Module 3) | `connectivity/centrality_table.csv`; `spatial/stream_network_graphs/stream_betweeness.gpkg`; `connectivity/centrality_map.png`; `connectivity/network_summary.csv` | Not read by any script — manuscript-methods outputs (betweenness map + descriptive network stats), not pipeline checkpoints |

## Module 05 — `05_network_fragmentation`

Species-free structural fragmentation of the network graph (current vs.
future dam scenario) — the network-level counterpart to Module 8's
species-level habitat fragmentation.

| Script | Reads | Writes | Consumed by |
| `01_network_fragmentation.R` | `spatial/stream_network_graphs/river_graph_current.RDS`, `river_graph_future.RDS` (Module 4) | `connectivity/network_fragmentation_summary.csv`; `fragment_strahler_distribution.csv`; `fragment_lengths.csv` | Not read by any script — manuscript results tables |
| `02_network_fragmentation_map.R` | `spatial/stream_network_graphs/river_graph_current.RDS`, `river_graph_future.RDS` (Module 4); `spatial/subbasin_sarantaporos/stream_network_pruned.gpkg` (Module 3); `points_snapped/dams/dams_snapped_points.csv` (Module 3) | `figures/fragmentation_network_map.png` | Not read by any script — manuscript figure |

## Module 06 — `06_env_space`

Descriptive-only module, purely for manuscript figures.

| Script | Reads | Writes | Consumed by |
| `01_env_space.R` | `points_snapped/fish/fish_all_species_snapped.csv` (Module 3); `spatial/subbasin_sarantaporos/stream_network.gpkg` (Module 3); `env90m/predict_table.csv` (Module 7) | `env90m/env_space_table.csv` (subset of the Module 7 predictor table); `figures/env_space/violin_{variable}.png`; `env_space_violins_panel.png`; `env_space_pca.png` | Not read by any script — manuscript figures only |

## Module 07 — `07_sdm`

11 scripts: download → prediction table → VIF filter → occurrence prep →
3 model types (SSN/MaxEnt/RF) → ensemble → habitat classification → figures.

| Script | Reads | Writes | Consumed by |
| `01_download_env90m_data.R` | `spatial/basin/stream_network_pruned.gpkg` (Module 3) | `env90m/{chelsa_bioclim_v2_1,esa_cci_landcover_v2_1_1,hydrography90m_v1_0}/...`; `env90m/subc_ids_basin.txt` | `02_create_prediction_table.R` (this module) |
| `02_create_prediction_table.R` | `env90m/subc_ids_basin.txt`, `env90m/{chelsa,esa_cci,hydrography}/...` (this module) | `env90m/predict_table.csv` (+ `_full`/`_complete` if NAs) | `03_check_multicollinearity.R`, `03b_prepare_sdm_data.R`, `04_ssn_models.R` (this module) |
| `03_check_multicollinearity.R` | `env90m/predict_table.csv` (this module); `spatial/basin/basin_subc_ids_pruned.csv` (Module 3) | `env90m/predict_table_vif.csv`; `env90m/selected_vars.csv` | `predict_table_vif.csv` → `05_maxent.R`, `06_random_forest.R` (this module). `selected_vars.csv` → not read by any script |
| `03b_prepare_sdm_data.R` | `points_snapped/fish/fish_all_species_snapped.csv` (Module 3); `spatial/basin/basin_subc_ids_pruned.csv`, `stream_network_pruned.gpkg` (Module 3); `env90m/predict_table.csv` (this module); `points_original/fish/species_list_sarantaporos.txt` | `sdm/input/occurr/{species}/occurr_{species}.csv`; `species_data_summary.csv` | `occurr_{species}.csv` → `04_ssn_models.R`, `06_random_forest.R` (this module) |
| `04_ssn_models.R` | `spatial/basin/stream_network_pruned.gpkg`, `basin_subc_ids_pruned.csv` (Module 3); `spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv` (Module 3); `sdm/input/occurr/{species}/occurr_{species}.csv`, `env90m/predict_table.csv` (this module) | `spatial/basin/ssn/{species}.ssn`; `sdm/ssn_models/{ssn_{species}.rds,torgegram_{species}.png,model_summary.csv,ssn_coefficients.csv,ssn_model_fit.csv,ssn_covariance_params.csv,ssn_loocv.csv}`; `sdm/predictions/pred_{species}.csv`; `spatial/subbasin_sarantaporos/stream_network_predictions.gpkg` | `pred_{species}.csv` → `07_ensemble.R` (this module). `model_summary.csv` → `07_ensemble.R`, `08_habitat_classification.R` (this module). Everything else terminal (manuscript methods) |
| `05_maxent.R` | `points_snapped/basin/fish_sdm_basin.csv` (Module 3); `spatial/basin/basin_subc_ids_pruned.csv` (Module 3); `env90m/predict_table_vif.csv` (this module); `spatial/subbasin_sarantaporos/{stream_network_pruned.gpkg,subbasin_subc_ids_pruned.csv}` (Module 3) | `sdm/maxent_models/{maxent_{species}.rds,maxent_evaluation.csv,maxent_variable_importance.csv}`; `sdm/predictions/pred_maxent_{species}.csv`; `spatial/subbasin_sarantaporos/stream_network_predictions_maxent.gpkg` | `pred_maxent_{species}.csv` → `07_ensemble.R` (this module). `maxent_evaluation.csv` → `07_ensemble.R`, `08_habitat_classification.R` (this module) |
| `06_random_forest.R` | `sdm/input/occurr/{species}/occurr_{species}.csv` (this module); `spatial/basin/basin_subc_ids_pruned.csv` (Module 3); `env90m/predict_table_vif.csv` (this module); `spatial/subbasin_sarantaporos/{stream_network_pruned.gpkg,subbasin_subc_ids_pruned.csv}` (Module 3) | `sdm/rf_models/{rf_{species}.rds,rf_evaluation.csv,rf_variable_importance.csv}`; `sdm/predictions/pred_rf_{species}.csv`; `spatial/subbasin_sarantaporos/stream_network_predictions_rf.gpkg` | `pred_rf_{species}.csv` → `07_ensemble.R` (this module). `rf_evaluation.csv` → `07_ensemble.R`, `08_habitat_classification.R` (this module) |
| `07_ensemble.R` | `sdm/predictions/{pred_{species},pred_maxent_{species},pred_rf_{species}}.csv`, `sdm/ssn_models/model_summary.csv`, `sdm/maxent_models/maxent_evaluation.csv`, `sdm/rf_models/rf_evaluation.csv` (this module); `spatial/subbasin_sarantaporos/{subbasin_subc_ids_pruned.csv,stream_network_pruned.gpkg}` (Module 3) | `sdm/ensemble/{ensemble_{species}.csv,ensemble_summary.csv}`; `spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg` | `ensemble_{species}.csv` → `08_habitat_classification.R` (this module), `figures_ensemble.R` (this module), `09_spatial_prioritization/01_spatial_prioritization.R` |
| `07b_dispersal_distance.R` | `points_original/fish/fish_distributional_and_traits_data.xlsx` (raw); `traits/AspectRatioData.csv` (raw); `points_snapped/fish/fish_all_species_snapped.csv` (Module 3) | `traits/fish_dispersal_distance.txt` | `08_habitat_classification.R` (this module) |
| `08_habitat_classification.R` | `spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg`, `sdm/{maxent_models/maxent_evaluation.csv,rf_models/rf_evaluation.csv,ssn_models/model_summary.csv}`, `traits/fish_dispersal_distance.txt` (this module) | `sdm/habitat/{habitat_{species}.csv,habitat_summary.csv}`; `spatial/subbasin_sarantaporos/{stream_network_habitat_lpt.gpkg,stream_network_habitat_mcc.gpkg}` | `habitat_summary.csv`, `stream_network_habitat_{tss,mcc}.gpkg` → `08_habitat_fragmentation/{01_habitat_fragmentation_metrics,habitat_fragmentation_metrics_sensitivity_analysis}.R`, `09_spatial_prioritization/01_spatial_prioritization.R` |
| `figures_ensemble.R` | `sdm/ensemble/ensemble_{species}.csv`, `sdm/habitat/habitat_summary.csv` (this module); `spatial/subbasin_sarantaporos/stream_network_pruned.gpkg` (Module 3) | `figures/sdm/{fig_ensemble_continuous.png,fig_ensemble_binary.png,fig_barbus_prespensis_habitat.png}` | Not read by any script — manuscript figures |

## Module 08 — `08_habitat_fragmentation`

Species-level habitat patch/fragmentation analysis, figures, and a buffer-
radius sensitivity check. All outputs are terminal (manuscript-only) — no
later module reads anything this module writes.

| Script | Reads | Writes | Consumed by |
| `00_compute_diversion_length.R` | `points_snapped/dams/{dams_snapped_points,factories_snapped_points}.csv` (Module 3); `spatial/stream_network_graphs/river_graph_current.RDS` (Module 4) | `points_cleaned/dams/diversion_lengths.csv` | Not read by any script — manuscript number |
| `01_habitat_fragmentation_metrics.R` | `spatial/subbasin_sarantaporos/{subbasin_subc_ids_pruned.csv,stream_network_pruned.gpkg,stream_network_habitat_lpt.gpkg}` (Module 3/7); `spatial/basin/stream_network_pruned.gpkg` (Module 3); `spatial/stream_network_graphs/{river_graph_current,river_graph_future}.RDS` (Module 4); `points_snapped/dams/dams_snapped_points.csv`, `points_snapped/basin/fish_sdm_basin.csv` (Module 3) | `sdm/patch_metrics/{species_passability,patch_membership_<sp>,dist_patch_<sp>,dist_point_<sp>,dam_buffer_reaches,dam_patch_proximity,dam_buffer_overlap,fragment_membership_<scenario>_<sp>,fragments_<scenario>_<sp>,fragment_summary_all,patch_summary_all,longest_path_all,species_impact_ranking}.csv` | `02_habitat_fragmentation_figures.R` and `habitat_fragmentation_metrics_sensitivity_analysis.R` (this module). Nothing outside this module |
| `02_habitat_fragmentation_figures.R` | `sdm/patch_metrics/*` (this module, script 01); `spatial/subbasin_sarantaporos/stream_network_pruned.gpkg` (Module 3); `points_snapped/{dams/dams_snapped_points,basin/fish_sdm_basin}.csv` (Module 3) | `figures/patch_metrics/*.png` (histograms + 3 summary charts); `figures/maps/map1_patches_*.png`, `map2_fragments_buffer_*.png` | Not read by any script — manuscript figures |
| `habitat_fragmentation_metrics_sensitivity_analysis.R` | `spatial/subbasin_sarantaporos/{subbasin_subc_ids_pruned.csv,stream_network_habitat_lpt.gpkg}` (Module 3/7); `points_snapped/dams/dams_snapped_points.csv` (Module 3); `sdm/patch_metrics/{patch_membership_<sp>,patch_summary_all}.csv` (this module, script 01) | `sdm/patch_metrics/sensitivity/{dam_buffer_reaches_up<U>_down<D>,dam_buffer_overlap_sensitivity,dam_buffer_overlap_sensitivity_species,dam_buffer_overlap_sensitivity_range}.csv` | Not read by any script — manuscript sensitivity check |

## Module 09 — `09_spatial_prioritization`

`prioritizr` spatial conservation prioritization under current/future
barrier scenarios, a planned-dam damage ranking, and figures. All outputs
are terminal (manuscript-only) — nothing outside this module reads them.

| Script | Reads | Writes | Consumed by |
| `01_spatial_prioritization.R` | `spatial/subbasin_sarantaporos/stream_network_pruned.gpkg`, `spatial/basin/stream_network.gpkg` (Module 3); `spatial/hfp_zonal_stats.csv` (this module, `00_human_footprint_cost_layer.R`); `sdm/ensemble/ensemble_<sp>.csv`, `sdm/habitat/habitat_summary.csv`, `sdm/habitat/habitat_<sp>.csv` (Module 7); `points_snapped/dams/dams_snapped_points.csv` (Module 3); `spatial/stream_network_graphs/{river_graph_current,river_graph_future}.RDS` (Module 4) | `prioritization/{pu_dat,puvspr_dat,summary_table,comparison_30pct,cost_scenario_AB_30pct,cost_scenario_AB_connectivity_30pct,sensitivity_k1_k3,boundary_penalty_calibration,forgone_hydropower_30pct,forgone_hydropower_dams_30pct}.csv`; `prioritization/solutions/solution_<scenario>_<target>pct.csv`; `prioritization/maps/{priority_comparison_30pct,summary_selected_reaches,boundary_penalty_calibration}.png` | `pu_dat.csv`, `puvspr_dat.csv` → `02_planned_dam_ranking.R` (this module). `comparison_30pct.csv` → `03_figures_prioritization.R` (this module). Nothing outside this module |
| `02_planned_dam_ranking.R` | `prioritization/{puvspr_dat,pu_dat}.csv` (this module, script 01); `spatial/stream_network_graphs/river_graph_current.RDS` (Module 4); `points_snapped/dams/dams_snapped_points.csv` (Module 3) | `prioritization/planned_dam_ranking.csv` | `03_figures_prioritization.R` (this module) |
| `03_figures_prioritization.R` | `prioritization/planned_dam_ranking.csv` (this module, script 02); `prioritization/comparison_30pct.csv` (this module, script 01); `spatial/subbasin_sarantaporos/stream_network_pruned.gpkg` (Module 3); `points_snapped/dams/dams_snapped_points.csv` (Module 3) | `prioritization/maps/{fig1_dam_ranking_bars,fig2_dam_damage_map,fig3_priority_comparison_30pct}.png` | Not read by any script — manuscript figures |
| `00_human_footprint_cost_layer.R` | `spatial/hfp_2021_100m_v1-2_cog.tif` (raw, pre-downloaded — **corrupted, needs re-download**, see README.md Module 09 section); `spatial/subbasin_sarantaporos/subbasin_polygon.gpkg` (Module 3) | `spatial/{hfp_crop,hfp_wgs,subcatchment_sarantaporos}.tif`; `spatial/hfp_zonal_stats.csv` | `hfp_zonal_stats.csv` → `01_spatial_prioritization.R` (this module) |
| `sensitivity_analyses.R` | Nothing from disk — reads objects left in the R environment by `01_spatial_prioritization.R` (`pu_dat`, `spec_dat`, `puvspr_dat`, `bmat_current`, `bmat_future`, `COMPARISON_TARGET`, `SOLVER_GAP`, `N_THREADS`, `BOUNDARY_PENALTY`) | `prioritization/boundary_penalty_sweep.csv` + `maps/boundary_penalty_sweep.png`; `prioritization/sensitivity_gap.csv` | Not read by any script. Not auto-run by anything either — manual companion, source/paste after script 01 |

## Module 10 — `10_traits`

Two independent scripts — do not share data despite living in the same
module directory.

| Script | Reads | Writes | Consumed by |
| `01_download_traits.R` | `points_snapped/fish/fish_all_species_snapped.csv` (Module 3); freshwaterecology.info API | `traits/{fwtraits_fish_greece,fwtraits_fish_greece_all,fwtraits_coverage_summary}.csv` | Not read by any script, including `02_functional_trait_analysis.R` in this same module |
| `02_functional_trait_analysis.R` | `points_original/fish/species_list_sarantaporos.txt` (raw); `points_original/fish/fish_distributional_and_traits_data.xlsx` "Traits" sheet (raw); `points_snapped/fish/fish_hcmr_with_species_snapped.csv` (Module 3); `spatial/subbasin_sarantaporos/{subbasin_polygon,stream_network_pruned}.gpkg` (Module 3) | `figures/traits/{fish_trait_dendrogram,fish_group_pie_map,fish_fd_map}.png`; `figures/traits/fish_fd_map_table.csv` | Not read by any script — manuscript figures |

## Module 11 — `11_pci`

Population Connectivity Index (current vs. future scenario) and
Fragmentation Index for the 7 focal species. All outputs terminal.

| Script | Reads | Writes | Consumed by |
| `01_dispersal_estimation.R` | `points_original/fish/species_list_sarantaporos.txt`, `fish_distributional_and_traits_data.xlsx` "Traits" sheet (raw); `points_snapped/fish/fish_all_species_snapped.csv` (Module 3, optional, guarded) | `traits/fish_dispersal_rank.txt` | `02_pci_calculation.R`, `03_pci_figures.R` (this module) |
| `02_pci_calculation.R` | `spatial/stream_network_graphs/{river_graph_current,river_graph_future}.RDS` (Module 4); `points_snapped/fish/fish_all_species_snapped.csv` (Module 3); `traits/fish_dispersal_rank.txt` (this module); `sdm/patch_metrics/species_passability.csv` (Module 8) | `connectivity/pci/{pci_full.RDS,fi_summary.txt}` | `03_pci_figures.R` (this module) |
| `03_pci_figures.R` | `connectivity/pci/fi_summary.txt` (this module); `traits/fish_dispersal_rank.txt` (this module) | `connectivity/pci/{fig_fi_barplot,fig_pci_dotplot,fig_pci_panel}.png`, `fig_pci_panel.pdf` | Not read by any script — manuscript figures |

## Module 12 — `12_lakes`

Standalone lake side-analysis (Aoos Springs reservoir). No output is
consumed by anything outside this module — not part of the core
connectivity pipeline.

| Script | Reads | Writes | Consumed by |
| `01_download_lake_data.R` | Hydrography90m / Env90m download endpoints | `spatial/{basin,sub_catchment,segment,accumulation,direction}*.tif`; `env90m/esa_cci_landcover_v2_1_1/*.txt` | `02_extract_lake_intersection.R`, `03_delineate_lake_catchment.R`, `04_lake_landcover_analysis.R` (this module) |
| `02_extract_lake_intersection.R` | `spatial/basin/stream_network_pruned.gpkg` (Module 3); `points_snapped/fish/all_snapped_fish_points.csv` (Module 3); `lakes/swot_lakes/swot_lakes.gpkg` (raw, manual download); `spatial/{segment,accumulation,basin}_h20v04.tif` (this module, script 01); external GWB tool | `lakes/lake_id.txt`; `lakes/lake_intersections/{coord_lake_<id>.txt,lake_<id>.tif,outlets_<id>.gpkg}` | `03_delineate_lake_catchment.R` (this module) |
| `03_delineate_lake_catchment.R` | `lakes/lake_intersections/{coord_lake_<id>.txt,lake_<id>.tif,outlets_<id>.gpkg}` (this module, script 02); `spatial/basin/stream_network_pruned.gpkg` (Module 3); `lakes/swot_lakes/swot_lakes.gpkg` (raw); `spatial/{direction,sub_catchment}_<tile>.tif` (this module, script 01); `spatial/basin/basin_polygon.gpkg` (Module 3, if present, else fetched) | `lakes/lake_intersections/basin_lake_<id>_merged.tif`; `spatial/subc_id_lake_catchment.tif`; `lakes/subc_IDs_lake_catchment.txt`; `figures/lakes/lake_catchment_map.{png,pdf}`; `figures/lakes/lake_catchment_inset_basin_map.{png,pdf}` | `04_lake_landcover_analysis.R` (this module) |
| `04_lake_landcover_analysis.R` | `lakes/subc_IDs_lake_catchment.txt`, `spatial/subc_id_lake_catchment.tif` (this module, script 03); `env90m/esa_cci_landcover_v2_1_1/*.txt` (this module, script 01) | `env90m/predictTB_lake_landcover.csv`; `figures/lakes/{lake_landcover_timeseries,lake_landcover_classes_grouped_colors}.png` | Not read by any script — manuscript figures |

---

## Design notes

**Two dispersal measures, deliberately separate.** Module 7's
`07b_dispersal_distance.R` estimates a continuous per-species dispersal distance
(`sigma_mob`) from the `fishmove` framework, used solely as the gap-filling
threshold in `08_habitat_classification.R`. Module 11's `01_dispersal_estimation.R`
derives ordinal dispersal ranks from traits, used solely for the connectivity
index. They are not interchangeable, and the `fishmove` time window is set for
individual movement rather than the multi-generation window appropriate to a
population-level connectivity ranking.

**Species passability has one source.** `02_pci_calculation.R` reads
`sdm/patch_metrics/species_passability.csv`, exported by Module 8's
`01_habitat_fragmentation_metrics.R` for exactly this purpose. Do not
reintroduce a second hand-authored copy: the values must change in one place.

**Module 6 reuses the Module 7 predictor table.** `01_env_space.R` builds its
table by subsetting `env90m/predict_table.csv` rather than downloading and
rescaling its own copy, so the values it describes are exactly those the models
were fitted on. The download and rescale steps remain in the script, commented
out, as a from-scratch path.

**Module 6 is terminal.** Nothing consumes its outputs; they are manuscript
figures only. Changing it requires no downstream re-runs.
