# Sarantaporos freshwater connectivity workflow

A twelve-module R pipeline for assessing freshwater fish distributions, stream
network connectivity and structural fragmentation, applied to the Sarantaporos
sub-basin of the Aoos/Vjosa river system in Greece.

The workflow runs from raw occurrence and barrier records through to species
distribution models, habitat fragmentation metrics and spatial conservation
prioritisation. It is built on [`hydrographr`](https://github.com/glowabio/hydrographr)
and the Hydrography90m / Environment90m datasets, and is intended to be adapted
to other basins and other freshwater taxa.

Modules run in numeric order and each consumes outputs from earlier ones.
`DEPENDENCIES.md` gives the full script-level input/output map.

---

## Setup

### 1. Paths and credentials

Copy `.Renviron.example` to `.Renviron` — in this folder, the repository root,
or your home directory — and fill in the values for your machine. `.Renviron` is
git-ignored, so machine-specific paths and credentials never enter the
repository.

| Variable | Required for | What it is |
|---|---|---|
| `WORKFLOWS_CODE` | everything | the folder containing `helpers/` and the numbered module folders |
| `WORKFLOW_DATA` | everything | the data directory, kept separate from the code |
| `GBIF_USER`, `GBIF_PWD`, `GBIF_EMAIL` | Module 1 | GBIF account, for the occurrence download |
| `API_KEY` | Module 10 (optional step) | freshwaterecology.info API key |
| `GWB_DIR` | Module 12 | local GuidosToolbox install directory |

Every script begins by sourcing `helpers/config.R`, which resolves these two
paths, then does `setwd(BASE_DIR)`; all other paths in the scripts are relative
to the data directory. If a required variable is unset, `config.R` stops with a
message naming it.

`WORKFLOW_DATA` can also be set per run, which is the intended way to work
against a test or alternative data directory without editing anything:

```bash
WORKFLOW_DATA=/path/to/other/data Rscript 07_sdm/08_habitat_classification.R
```

Note that R gives `.Renviron` precedence over variables exported in the shell.
If you want per-run overrides of `WORKFLOW_DATA` to take effect, leave it out of
`.Renviron` and set it on the command line instead.

### 2. R packages

Package versions and licences are listed in `LICENSES.txt`. The main
dependencies are `hydrographr`, `sf`, `terra`, `data.table`, `dplyr`,
`igraph`, `prioritizr`, `SSN2`, `SSNbler`, `maxnet`, `ranger`, `dismo`,
`rgbif`, `fwtraits` and `ggplot2`.

### 3. Input data

Most inputs are downloaded by the scripts themselves. The following are
**not** fetched automatically and must be placed in the data directory before
the relevant module runs:

| File | Needed by | Source |
|---|---|---|
| `points_original/fish/fish_distributional_and_traits_data.xlsx` | Modules 1, 7, 10, 11 | field survey and expert trait data (see Data availability) |
| `points_original/fish/Sarantaporos.xlsx` | Module 1 | field survey records |
| `points_original/dams/V_SDI_R_HYDRO*.csv` | Module 2 | Greek hydropower licence register |
| `traits/AspectRatioData.csv` | Module 7 | caudal-fin aspect ratios for the dispersal model |
| `lakes/swot_lakes/swot_lakes.gpkg` | Module 12 | SWOT Prior Lake Database, manual download from <https://hydroweb.next.theia-land.fr/> |
| `spatial/hfp_2021_100m_v1-2_cog.tif` | Module 9 | Human Footprint Index (~13 GB); the `download.file()` call in `00_human_footprint_cost_layer.R` is commented out by default |

Everything else — GBIF occurrences, Hydrography90m and Environment90m tiles,
CHELSA climate and ESA CCI land cover — is downloaded by the scripts on a fresh
run, given internet access and the credentials above.

---

## Running the workflow

Run modules in numeric order, and scripts within a module in numeric order.
Two scripts are exceptions:

- `09_spatial_prioritization/00_human_footprint_cost_layer.R` must run **before**
  `01_spatial_prioritization.R`, which reads its output as the cost layer.
- `09_spatial_prioritization/sensitivity_analyses.R` is a companion script, not
  part of the automated sequence. Source it in the same R session immediately
  after `01_spatial_prioritization.R`, since it uses objects that script leaves
  in the environment rather than reading them from disk.

```bash
export WORKFLOW_DATA=/path/to/data
Rscript 01_biodiversity_data/01_clean_hcmr_fish.R
Rscript 01_biodiversity_data/02_download_gbif_fish.R
# ... and so on
```

---

## Modules

### 01 — `01_biodiversity_data`

Cleans and combines fish occurrence records from field surveys and GBIF.

| Script | Purpose |
|---|---|
| `01_clean_hcmr_fish.R` | Cleans the field survey records; validates species names against FishBase; assigns the basin ID used downstream |
| `02_download_gbif_fish.R` | Queries the GBIF download API per taxonomic group and merges the results |
| `03_clean_gbif_fish.R` | Filters GBIF records to the basin, removes duplicates and problematic coordinates |

Requires internet access and GBIF credentials. GBIF downloads are issued a DOI
each; the script records them in `points_original/fish/gbif_citation.txt`.

### 02 — `02_barrier_data`

`01_clean_dam_data.R` cleans the hydropower licence register and maps licence
stage onto two scenarios: operational licences become the *existing* barrier,
all other stages become *planned* barriers. Rejected applications are excluded.

### 03 — `03_snapping`

Places occurrence and barrier points onto the stream network, and delineates the
study sub-basin.

| Script | Purpose |
|---|---|
| `01_snap_all_data.R` | Snaps points to stream segments via the GeoFRESH API, using a hierarchical cascade from higher to lower Strahler orders |
| `02_join_spnames_with_locations.R` | Re-attaches species names to snapped locations |
| `03_extract_subbasin.R` | Delineates the Sarantaporos sub-basin and prunes the network |
| `04_study_area_map.R` | Study area overview map |

Requires internet access (GeoFRESH API).

### 04 — `04_network_analyses`

Builds the river network graph and computes centrality.

| Script | Purpose |
|---|---|
| `01_generate_network_graph.R` | Builds directed graphs for the current and future barrier scenarios |
| `02_centrality.R` | Betweenness centrality across the network |

### 05 — `05_network_fragmentation`

`01_network_fragmentation.R` computes structural fragmentation of the whole
network under both scenarios; `02_network_fragmentation_map.R` maps it. This is
network-level fragmentation, independent of species.

### 06 — `06_env_space`

`01_env_space.R` describes how occurrences distribute across the environmental
predictors used for modelling, before any model is fitted. It subsets the SDM
prediction table (Module 7) to occurrence sub-catchments and produces
per-species violin plots and a PCA biplot. Descriptive only — nothing downstream
consumes its outputs.

### 07 — `07_sdm`

Species distribution modelling: an ensemble of three algorithms, thresholded
into predicted suitable reaches.

| Script | Purpose |
|---|---|
| `01_download_env90m_data.R` | Downloads Hydrography90m, CHELSA and ESA CCI land cover tiles |
| `02_create_prediction_table.R` | Builds the per-sub-catchment predictor table and rescales to analysis units |
| `03_check_multicollinearity.R` | VIF screening; writes the retained predictor list |
| `03b_prepare_sdm_data.R` | Assembles presence and pseudo-absence tables at a balanced prevalence |
| `04_ssn_models.R` | Spatial stream network models (SSN2) |
| `05_maxent.R` | MaxEnt (presence-background) |
| `06_random_forest.R` | Random forest (case-weighted) |
| `07_ensemble.R` | Averages the continuous predictions across algorithms |
| `07b_dispersal_distance.R` | Per-species dispersal distance from the `fishmove` framework, used for gap-filling |
| `08_habitat_classification.R` | Binarises the ensemble, fills short gaps, removes isolated reaches |
| `figures_ensemble.R` | Ensemble prediction figures |

The binarisation threshold is the Lowest Presence Threshold (LPT) — the lowest
ensemble suitability at any occurrence of a species — applied uniformly across
species. A Matthews Correlation Coefficient threshold is retained as a
sensitivity analysis. Columns and files carrying LPT values are named `*_lpt`;
the `*_tss` names that remain refer to max-TSS values reported as model
evaluation statistics, which no longer set the threshold.


### 08 — `08_habitat_fragmentation`

Fragmentation of predicted suitable habitat by barriers.

| Script | Purpose |
|---|---|
| `00_compute_diversion_length.R` | Diversion lengths for dam impact zones |
| `01_habitat_fragmentation_metrics.R` | Patch structure, fragment counts, dam buffer overlap, longest corridors |
| `02_habitat_fragmentation_figures.R` | Maps and summary plots |
| `habitat_fragmentation_metrics_sensitivity_analysis.R` | Sensitivity of buffer overlap to impact-zone radius |

### 09 — `09_spatial_prioritization`

Spatial conservation prioritisation with `prioritizr`, under both barrier
scenarios.

| Script | Purpose |
|---|---|
| `00_human_footprint_cost_layer.R` | Crops the Human Footprint raster and computes per-sub-catchment zonal statistics — **run first** |
| `01_spatial_prioritization.R` | Minimum-set prioritisation across four targets and two scenarios, with barrier-aware connectivity |
| `02_planned_dam_ranking.R` | Ranks planned dams by isolated habitat per unit of energy |
| `03_figures_prioritization.R` | Prioritisation figures |
| `sensitivity_analyses.R` | Boundary-penalty sweep and solver-gap check (companion script, see above) |

The solver runs single-threaded: CBC with multiple threads and a non-zero
optimality gap is not reproducible run to run.

### 10 — `10_traits`

| Script | Purpose |
|---|---|
| `01_download_traits.R` | Optional: retrieves traits from freshwaterecology.info via `fwtraits` |
| `02_functional_trait_analysis.R` | Gower dissimilarity, hierarchical clustering, functional diversity mapping |

`02_functional_trait_analysis.R` reads the expert-compiled trait table from the
`Traits` sheet of the occurrence spreadsheet. `01_download_traits.R` is provided
for users who do not have their own trait data; its output is not consumed by
the analysis.

### 11 — `11_pci`

Population Connectivity Index.

| Script | Purpose |
|---|---|
| `01_dispersal_estimation.R` | Ordinal dispersal ranks per species from traits |
| `02_pci_calculation.R` | PCI and fragmentation index under both scenarios |
| `03_pci_figures.R` | PCI figures |

The ordinal dispersal ranks used here are distinct from the continuous dispersal
distances estimated in Module 7 for habitat gap-filling.

### 12 — `12_lakes`

Integrates lakes into the stream network.

| Script | Purpose |
|---|---|
| `01_download_lake_data.R` | Downloads the Hydrography90m tiles and land cover tables this module needs |
| `02_extract_lake_intersection.R` | Finds intersections between lakes and the stream network (requires GuidosToolbox) |
| `03_delineate_lake_catchment.R` | Delineates lake catchments |
| `04_lake_landcover_analysis.R` | Land cover composition of lake catchments |

---

## Repository layout

```
workflows/
  helpers/          config.R and shared helper functions
  01_.. 12_..       the twelve modules
  archive/          superseded scripts, kept for reference only
  DEPENDENCIES.md   script-level input/output map
  LICENSES.txt      package licences
  .Renviron.example template for paths and credentials
```

Scripts under `archive/` are not part of the workflow, are not maintained, and
still contain absolute paths from the original development machine. They are
retained only as a record of earlier approaches.

---

## Data availability

Input data that the workflow cannot generate — field survey fish records,
expert-compiled traits, the hydropower register extracts, and the GBIF
occurrence records used in the published analysis — are deposited separately;
see the manuscript's data availability statement for the DOI.

The GBIF occurrence records are archived alongside their download DOIs because
GBIF retains downloads only for a limited period and its records are
continuously updated: re-running `02_download_gbif_fish.R` reproduces the
procedure, not the dataset.
