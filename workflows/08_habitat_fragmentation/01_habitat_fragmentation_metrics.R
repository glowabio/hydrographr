#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_habitat_fragmentation_metrics.R   (Module 8 -- Habitat Fragmentation)
#
# Compute habitat patch metrics and dam-related fragmentation for freshwater
# fish species in the Sarantaporos subbasin.
#
# SDM predictions were made on Sarantaporos + Voidomatis (subbasin);
# here we trim to Sarantaporos only for connectivity analysis by filtering
# network reaches to those in subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
# (produced by 01b_extract_sarantaporos_subbasin.R).
#
# Metrics organised in three groups:
#
#   GROUP A — Habitat structure (no dams)
#     A1. Total suitable habitat length per species (km)
#     A2. Number of habitat patches per species
#     A3. Inter-patch distances (along network, outer tip to outer tip)
#         + Inter-point distances (between occurrence points)
#     A4. Longest along-network path:
#           (a) between occurrence points
#           (b) between the two most distant suitable reaches
#
#   GROUP B — Dam geometry
#     Dam buffers computed once on the full Sarantaporos network.
#     B1. Along-network distance of each patch from its nearest dam
#         (current and future scenarios)
#     B2. Overlap of patches with dam impact buffers
#           upstream:   50 m  (inundation / backwater)
#           downstream: 2000 m (dewatered reach, run-of-river)
#
#   GROUP C — Fragmentation after dams
#     Uses pre-built scenario graphs (river_graph_current.RDS /
#     river_graph_future.RDS from 04_network_analyses/01_generate_network_graph.R).
#     Edges flagged as barriers (E()$barrier == TRUE, i.e. a dam on the
#     reach) are deleted for binary structural fragmentation.
#     Suitable subgraph induced on cut graph = habitat split into fragments.
#     C1. Number of fragments per species per scenario (current / future)
#     C2. Total suitable habitat (km) per fragment
#     C3. Number of occurrence points per fragment
#     C4. Species ranking by fragment increase (delta_fragments)
#
# NOTE on passability:
#   Group C fragment counts are STRUCTURAL and uniform across species:
#   every dam is treated as a cut, regardless of the species' ability to
#   pass it. The species-specific passability (0.8 / 0.5 / 0; see below)
#   enters only the population-level connectivity (PCI) step in Module 10.
#   For Salmo farioides and Anguilla anguilla (passability 0.8) the
#   structural fragment counts therefore represent a worst case relative
#   to their actual movement ability, but they still lose habitat in the
#   dewatered reach downstream of run-of-river plants.
#
# Species passability for the PCI step (exported to species_passability.csv):
#   Anguilla anguilla, Salmo farioides               -> 0.8
#   Chondrostoma ohridanum, Alburnoides prespensis,
#     Barbus prespensis                               -> 0.5
#   Oxynoemacheilus pindus, Squalius platyceps        -> 0.0
#
# Prerequisite scripts:
#   01b_extract_sarantaporos_subbasin.R               (Sarantaporos subc_ids)
#   04_network_analyses/01_generate_network_graph.R   (scenario graphs)
#   06_sdm/08_habitat_classification.R                (bin_ columns in habitat gpkg)
#
# Inputs:
#   spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
#   spatial/subbasin_sarantaporos/stream_network_pruned.gpkg  (reach lengths)
#   spatial/subbasin/stream_network_habitat_tss.gpkg          (SDM predictions)
#   spatial/stream_networks/river_graph_current.RDS
#   spatial/stream_networks/river_graph_future.RDS
#   points_snapped/dams/dams_snapped_points.csv
#   points_snapped/basin/fish_sdm_basin.csv
#
# Outputs (all in sdm/patch_metrics/): see Step 7
#
# Figures: see 02_habitat_fragmentation_figures.R
#
# LOCATION: workflows/08_habitat_fragmentation/01_habitat_fragmentation_metrics.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(igraph)
library(sf)
library(data.table)
library(dplyr)

select <- dplyr::select

source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Anguilla_anguilla",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Oxynoemacheilus_pindus",
  "Salmo_farioides",
  "Squalius_platyceps"
)

# Species passability used in the PCI step — stored here for reference/export.
# Expert-based values rescaled per supervisor: species that can largely pass
# small hydropower structures take 0.8 (not 1.0, to reflect uncertainty).
SPECIES_PASSABILITY <- c(
  Anguilla_anguilla      = 0.8,
  Salmo_farioides        = 0.8,
  Chondrostoma_ohridanum = 0.5,
  Alburnoides_prespensis = 0.5,
  Barbus_prespensis      = 0.5,
  Oxynoemacheilus_pindus = 0.0,
  Squalius_platyceps     = 0.0
)

# Dam buffer radii (metres) — see MS methods
DAM_BUFFER_UP_M   <-   50   # upstream:   inundation / backwater
DAM_BUFFER_DOWN_M <- 2000   # downstream: dewatered reach (run-of-river)

# Minimum patch size — guard against single-reach artefacts
MIN_PATCH_REACHES <- 2L

# Threshold method for SDM habitat classification
THRESHOLD_METHOD <- "tss"

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HABITAT FRAGMENTATION METRICS — SARANTAPOROS")
message(paste(rep("=", 80), collapse = ""))

dir.create("sdm/patch_metrics", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Build Sarantaporos network graph with reach lengths
# ============================================================

message("\n=== Step 1: Building Sarantaporos network graph ===")

outlet_df <- data.frame(
  site_id   = "sarantaporos_outlet",
  longitude = 20.5897,
  latitude  = 40.070519
)

message("  Getting outlet subc_id via api_get_ids() ...")
outlet_ids <- api_get_ids(
  points          = outlet_df,
  mode            = "local",
  colname_lon     = "longitude",
  colname_lat     = "latitude",
  colname_site_id = "site_id"
)

outlet_subc_id <- outlet_ids$subc_id[1]
message("  Outlet subc_id: ", outlet_subc_id)

message("  Loading full Vjosa pruned network ...")
vjosa_g <- read_geopackage(
  gpkg      = "spatial/basin/stream_network_pruned.gpkg",
  import_as = "graph"
)
message("  Vjosa graph: ", vcount(vjosa_g), " nodes | ",
        ecount(vjosa_g), " edges")

message("  Extracting Sarantaporos subgraph via get_catchment_graph() ...")
network_g <- get_catchment_graph(
  g        = vjosa_g,
  subc_id  = outlet_subc_id,
  mode     = "in",
  as_graph = TRUE,
  n_cores  = 1
)
message("  Sarantaporos graph: ", vcount(network_g), " nodes | ",
        ecount(network_g), " edges")

sarantaporos_ids_pruned <- fread(
  "spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv"
)$subc_id

network_g <- induced_subgraph(
  network_g,
  vids = V(network_g)[name %in% as.character(sarantaporos_ids_pruned)]
)
message("  After filtering to Sarantaporos: ", vcount(network_g), " nodes")

edge_df <- igraph::as_data_frame(network_g, what = "edges") %>%
  as.data.table()

network_dt <- edge_df %>%
  rename(subc_id        = from,
         target         = to,
         reach_length_m = length) %>%
  mutate(subc_id = as.integer(subc_id),
         target  = as.integer(target))

sarantaporos_ids <- network_dt$subc_id
message("  Sarantaporos reaches: ", length(sarantaporos_ids))

# Each edge represents one stream segment; use its length directly as weight.
E(network_g)$weight <- E(network_g)$length

# Load SDM habitat predictions and join bin_ columns to network_dt.
hab_gpkg <- paste0("spatial/subbasin/stream_network_habitat_",
                   THRESHOLD_METHOD, ".gpkg")
message("  Loading SDM predictions from: ", hab_gpkg)

hab_dt <- st_read(hab_gpkg, quiet = TRUE) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  filter(subc_id %in% sarantaporos_ids)

message("  SDM reaches (Sarantaporos): ", nrow(hab_dt))

bin_cols   <- names(hab_dt)[grepl("^(bin|semi|gap|isol)_", names(hab_dt))]
network_dt <- network_dt %>%
  left_join(
    hab_dt %>% select(subc_id, all_of(bin_cols)),
    by = "subc_id"
  )

network_sf <- st_read(hab_gpkg, quiet = TRUE) %>%
  filter(subc_id %in% sarantaporos_ids)

message("  Graph: ", vcount(network_g), " nodes | ",
        ecount(network_g), " edges")

# ============================================================
# STEP 2: Load scenario graphs (for fragment counting, Group C)
# ============================================================

message("\n=== Step 2: Loading scenario graphs ===")

# Scenario graphs flag dam edges via E()$barrier (TRUE where a dam sits on
# the reach), built by 04_network_analyses/01_generate_network_graph.R.
# A dam attributes to a single downstream edge, so one dam = one cut.
# We delete barrier edges for binary structural fragmentation.
river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

message("  Current: ", vcount(river_graph_current), " nodes | ",
        ecount(river_graph_current), " edges")
message("  Future:  ", vcount(river_graph_future),  " nodes | ",
        ecount(river_graph_future),  " edges")

if (!"barrier" %in% edge_attr_names(river_graph_current))
  stop("E()$barrier not found in river_graph_current — rerun ",
       "04_network_analyses/01_generate_network_graph.R")

message("  Barrier edges: current = ",
        sum(E(river_graph_current)$barrier, na.rm = TRUE),
        " | future = ",
        sum(E(river_graph_future)$barrier, na.rm = TRUE))

# ============================================================
# STEP 3: Load dam data and occurrence points
# ============================================================

message("\n=== Step 3: Loading dam data and occurrence points ===")

dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% network_dt$subc_id)

dams_current <- dams %>% filter(status == "existing")
dams_future  <- dams %>% filter(status %in% c("existing", "planned"))

message("  Dams current: ", nrow(dams_current),
        " | future: ", nrow(dams_future))

fish_pts_sub <- fread("points_snapped/basin/fish_sdm_basin.csv") %>%
  filter(subc_id %in% network_dt$subc_id)

message("  Occurrence records in Sarantaporos: ", nrow(fish_pts_sub))

# Export passability reference for the PCI step
data.table(
  species     = names(SPECIES_PASSABILITY),
  passability = SPECIES_PASSABILITY
) %>%
  fwrite("sdm/patch_metrics/species_passability.csv")
message("  Saved: sdm/patch_metrics/species_passability.csv")

# ============================================================
# STEP 4: Helper — fragment counting from scenario graph
# ============================================================

# Deletes barrier edges (a dam on the reach) from the scenario graph, then
# induces the suitable subgraph (reaches where bin_species == 1) on the cut
# graph. Weakly connected components of the resulting undirected subgraph are
# the habitat fragments after dam placement. Weak components ignore flow
# direction — correct because fish move both up- and downstream within a
# fragment.

get_fragments <- function(scenario_graph, suitable_ids_chr) {

  dam_edge_idx <- which(E(scenario_graph)$barrier)
  message("    Barrier edges deleted: ", length(dam_edge_idx))

  g_cut <- delete_edges(scenario_graph, dam_edge_idx)

  suitable_in_g <- suitable_ids_chr[
    suitable_ids_chr %in% as.character(V(g_cut)$name)
  ]

  if (length(suitable_in_g) < 1) return(NULL)

  g_cut %>%
    induced_subgraph(vids = V(.)[name %in% suitable_in_g]) %>%
    as_undirected(mode = "collapse", edge.attr.comb = "mean") %>%
    components(mode = "weak")
}

# ============================================================
# STEP 5: GROUP B — Dam buffer computation (once, all future dams)
# ============================================================

message("\n=== Step 5: Group B — Dam impact buffers ===")

network_sf_buf <- network_sf %>%
  left_join(
    network_dt %>% select(subc_id, length = reach_length_m),
    by = "subc_id"
  )

message("  Computing buffers for ", nrow(dams_future),
        " dams (future scenario — worst case) ...")
message("  Upstream: ", DAM_BUFFER_UP_M,
        " m | Downstream: ", DAM_BUFFER_DOWN_M, " m")

buffer_dt <- tryCatch({
  get_buffer_along_the_network(
    lines_sf    = network_sf_buf,
    target_ids  = unique(as.character(dams_future$subc_id)),
    up_radius   = DAM_BUFFER_UP_M,
    down_radius = DAM_BUFFER_DOWN_M
  ) %>%
    as.data.table()
}, error = function(e) {
  message("  ERROR in get_buffer_along_the_network: ", e$message)
  NULL
})

BUF_SUBC_COL <- "subc_id"

if (!is.null(buffer_dt)) {
  message("  Buffer reaches: ", nrow(buffer_dt))
  fwrite(buffer_dt, "sdm/patch_metrics/dam_buffer_reaches.csv")
  message("  Saved: sdm/patch_metrics/dam_buffer_reaches.csv")
}

# ============================================================
# STEP 6: Species loop — Groups A, B (per species), C
# ============================================================

message("\n=== Step 6: Species loop ===")

all_patch_summary   <- list()
longest_path_rows   <- list()
dam_proximity_rows  <- list()
buffer_overlap_rows <- list()
fragment_rows       <- list()

for (sp in TARGET_SPECIES) {

  message("\n", paste(rep("-", 60), collapse = ""))
  message("  SPECIES: ", sp)

  bin_col <- paste0("bin_", sp)
  if (!bin_col %in% names(network_dt)) {
    message("  Column '", bin_col, "' not found — skipping"); next
  }

  suitable_ids <- network_dt %>%
    filter(.data[[bin_col]] == 1L) %>%
    pull(subc_id) %>%
    as.character()

  n_suitable <- length(suitable_ids)
  message("  Suitable reaches: ", n_suitable)
  if (n_suitable == 0) { message("  No suitable reaches — skipping"); next }

  # ----------------------------------------------------------
  # A1-A2: Patch identification (no dams, network_g topology)
  # ----------------------------------------------------------
  suitable_subg_undir <- network_g %>%
    induced_subgraph(vids = V(.)[name %in% suitable_ids]) %>%
    as_undirected(mode = "collapse", edge.attr.comb = "mean")

  comps <- components(suitable_subg_undir, mode = "weak")

  patch_membership <- data.table(
    species  = sp,
    subc_id  = as.integer(V(suitable_subg_undir)$name),
    patch_id = comps$membership
  ) %>%
    left_join(
      network_dt %>% select(subc_id, reach_length_m),
      by = "subc_id"
    )

  patch_summary <- patch_membership %>%
    group_by(species, patch_id) %>%
    summarise(
      n_reaches = n(),
      length_km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3),
      .groups   = "drop"
    ) %>%
    filter(n_reaches >= MIN_PATCH_REACHES) %>%
    as.data.table()

  valid_patches <- patch_summary$patch_id
  n_patches     <- length(valid_patches)
  total_len_km  <- round(sum(patch_summary$length_km), 3)

  message("  Patches (>= ", MIN_PATCH_REACHES, " reaches): ", n_patches,
          " | Total: ", total_len_km, " km")

  patch_membership <- patch_membership %>%
    filter(patch_id %in% valid_patches) %>%
    as.data.table()

  fwrite(patch_membership,
         paste0("sdm/patch_metrics/patch_membership_", sp, ".csv"))

  patch_nodes     <- patch_membership %>%
    select(patch_id, subc_id) %>%
    mutate(subc_id = as.character(subc_id)) %>%
    as.data.table()
  all_patch_nodes <- unique(patch_nodes$subc_id)

  # ----------------------------------------------------------
  # A3: Inter-patch distances
  # ----------------------------------------------------------
  dist_patch_long <- NULL
  all_pairs_dist  <- NULL

  if (n_patches >= 2 || (n_patches == 1 && length(all_patch_nodes) >= 2)) {

    message("  Computing inter-patch distances ...")

    all_pairs_dist <- igraph::distances(
      network_g,
      v    = V(network_g)[name %in% all_patch_nodes],
      to   = V(network_g)[name %in% all_patch_nodes],
      mode = "all"
    )

    if (n_patches >= 2) {

      dist_patch_long <- combn(valid_patches, 2, simplify = FALSE) %>%
        lapply(function(pair) {
          nodes_a <- patch_nodes %>% filter(patch_id == pair[1]) %>% pull(subc_id)
          nodes_b <- patch_nodes %>% filter(patch_id == pair[2]) %>% pull(subc_id)
          sub_mat <- all_pairs_dist[rownames(all_pairs_dist) %in% nodes_a,
                                    colnames(all_pairs_dist) %in% nodes_b,
                                    drop = FALSE]
          min_d   <- min(sub_mat[is.finite(sub_mat)], na.rm = TRUE)
          data.table(
            species     = sp,
            patch_a     = pair[1],
            patch_b     = pair[2],
            dist_km     = round(if (is.finite(min_d)) min_d / 1000 else NA_real_, 4),
            length_km_a = patch_summary[patch_id == pair[1], length_km],
            length_km_b = patch_summary[patch_id == pair[2], length_km]
          )
        }) %>%
        rbindlist()

      fwrite(dist_patch_long,
             paste0("sdm/patch_metrics/dist_patch_", sp, ".csv"))
      message("  Inter-patch pairs: ", nrow(dist_patch_long),
              " | Range: [",
              round(min(dist_patch_long$dist_km, na.rm = TRUE), 2), ", ",
              round(max(dist_patch_long$dist_km, na.rm = TRUE), 2), "] km")
    }
  }

  # ----------------------------------------------------------
  # A3: Inter-point distances
  # ----------------------------------------------------------
  dist_point_long <- NULL

  sp_pt_ids <- fish_pts_sub %>%
    filter(species == sp) %>%
    pull(subc_id) %>%
    unique() %>%
    as.character() %>%
    .[. %in% as.character(V(network_g)$name)]

  n_miss_pts <- fish_pts_sub %>%
    filter(species == sp) %>%
    pull(subc_id) %>%
    unique() %>%
    as.character() %>%
    setdiff(as.character(V(network_g)$name)) %>%
    length()
  if (n_miss_pts > 0)
    message("  WARNING: ", n_miss_pts,
            " occurrence subc_id(s) not in graph — excluded")

  if (length(sp_pt_ids) >= 2) {

    message("  Computing inter-point distances (",
            length(sp_pt_ids), " points) ...")

    dist_pt_mat <- igraph::distances(
      network_g,
      v    = V(network_g)[name %in% sp_pt_ids],
      to   = V(network_g)[name %in% sp_pt_ids],
      mode = "all"
    )

    dist_point_long <- combn(sp_pt_ids, 2, simplify = FALSE) %>%
      lapply(function(pair) {
        d <- dist_pt_mat[pair[1], pair[2]]
        data.table(
          species   = sp,
          subc_id_a = as.integer(pair[1]),
          subc_id_b = as.integer(pair[2]),
          dist_km   = round(if (is.finite(d)) d / 1000 else NA_real_, 4)
        )
      }) %>%
      rbindlist()

    fwrite(dist_point_long,
           paste0("sdm/patch_metrics/dist_point_", sp, ".csv"))
    message("  Inter-point pairs: ", nrow(dist_point_long),
            " | Range: [",
            round(min(dist_point_long$dist_km, na.rm = TRUE), 2), ", ",
            round(max(dist_point_long$dist_km, na.rm = TRUE), 2), "] km")
  }

  # ----------------------------------------------------------
  # A4: Longest path
  # ----------------------------------------------------------
  longest_point_km   <- NA_real_
  longest_habitat_km <- NA_real_

  if (!is.null(dist_point_long) && nrow(dist_point_long) > 0)
    longest_point_km <- max(dist_point_long$dist_km, na.rm = TRUE)

  if (!is.null(all_pairs_dist))
    longest_habitat_km <- round(
      max(all_pairs_dist[is.finite(all_pairs_dist)], na.rm = TRUE) / 1000, 3
    )

  longest_path_rows[[sp]] <- data.table(
    species            = sp,
    longest_point_km   = round(longest_point_km,   3),
    longest_habitat_km = round(longest_habitat_km, 3)
  )
  message("  Longest path — points: ", longest_point_km,
          " km | habitat: ", longest_habitat_km, " km")

  # ----------------------------------------------------------
  # B1: Along-network distance of each patch from nearest dam
  # ----------------------------------------------------------
  for (scenario_name in c("current", "future")) {

    dam_ids_chr <- if (scenario_name == "current")
      as.character(dams_current$subc_id)
    else
      as.character(dams_future$subc_id)

    if (length(dam_ids_chr) == 0 || length(all_patch_nodes) == 0) next

    dam_ids_in_g <- dam_ids_chr[
      dam_ids_chr %in% as.character(V(network_g)$name)
    ]
    if (length(dam_ids_in_g) == 0) next

    dist_to_dams <- igraph::distances(
      network_g,
      v    = V(network_g)[name %in% all_patch_nodes],
      to   = V(network_g)[name %in% dam_ids_in_g],
      mode = "all"
    )

    for (pid in valid_patches) {
      nodes_p <- patch_nodes %>% filter(patch_id == pid) %>% pull(subc_id)
      sub_d   <- dist_to_dams[rownames(dist_to_dams) %in% nodes_p, , drop = FALSE]
      min_d   <- min(sub_d[is.finite(sub_d)], na.rm = TRUE)
      dam_proximity_rows[[paste(sp, scenario_name, pid)]] <- data.table(
        species         = sp,
        scenario        = scenario_name,
        patch_id        = pid,
        patch_length_km = patch_summary[patch_id == pid, length_km],
        min_dist_dam_km = round(if (is.finite(min_d)) min_d / 1000 else NA_real_, 4)
      )
    }
  }

  # ----------------------------------------------------------
  # B2: Overlap of each patch with dam impact buffers
  # ----------------------------------------------------------
  if (!is.null(buffer_dt)) {

    up_ids   <- buffer_dt %>%
      filter(direction == "upstream") %>%
      pull(!!BUF_SUBC_COL) %>%
      as.character()
    dn_ids   <- buffer_dt %>%
      filter(direction == "downstream") %>%
      pull(!!BUF_SUBC_COL) %>%
      as.character()

    for (pid in valid_patches) {

      patch_ids_chr <- patch_membership %>%
        filter(patch_id == pid) %>%
        pull(subc_id) %>%
        as.character()

      overlap_ids    <- intersect(patch_ids_chr, union(up_ids, dn_ids))
      n_overlap      <- length(overlap_ids)

      overlap_len_km <- if (n_overlap > 0)
        network_dt %>%
        filter(subc_id %in% as.integer(overlap_ids)) %>%
        summarise(km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3)) %>%
        pull(km)
      else 0

      patch_len <- patch_summary[patch_id == pid, length_km]

      buffer_overlap_rows[[paste(sp, pid)]] <- data.table(
        species           = sp,
        patch_id          = pid,
        patch_length_km   = patch_len,
        n_reaches_overlap = n_overlap,
        overlap_length_km = overlap_len_km,
        pct_overlap       = round(100 * overlap_len_km / patch_len, 1)
      )
    }
  }

  # ----------------------------------------------------------
  # C1-C3: Fragmentation after dams using scenario graphs
  # ----------------------------------------------------------
  # Structural fragmentation is uniform across species: every dam is a cut,
  # regardless of the species' passability. The species-specific passability
  # (0.8 / 0.5 / 0) enters only the PCI step (Module 10), not here.

  for (scenario_name in c("current", "future")) {

    g_scen <- if (scenario_name == "current") river_graph_current
    else                             river_graph_future

    message("  [", scenario_name, "] Computing fragments ...")
    frags <- get_fragments(g_scen, suitable_ids)

    if (is.null(frags)) {
      message("  [", scenario_name,
              "] No suitable reaches in scenario graph — skipping")
      next
    }

    # Rebuild cut graph to retrieve node names for the reach length join
    g_cut         <- delete_edges(g_scen, which(E(g_scen)$barrier))
    suitable_in_g <- suitable_ids[suitable_ids %in% as.character(V(g_cut)$name)]
    cut_subg      <- induced_subgraph(g_cut,
                                      vids = V(g_cut)[name %in% suitable_in_g])

    frag_dt <- data.table(
      species     = sp,
      subc_id     = as.integer(V(cut_subg)$name),
      fragment_id = frags$membership
    ) %>%
      left_join(
        network_dt %>% select(subc_id, reach_length_m),
        by = "subc_id"
      )

    # Per-reach membership (subc_id -> fragment_id) for the figures script,
    # which colours reaches on the fragment maps.
    fwrite(frag_dt,
           paste0("sdm/patch_metrics/fragment_membership_",
                  scenario_name, "_", sp, ".csv"))

    frag_summary_sp <- frag_dt %>%
      group_by(fragment_id) %>%
      summarise(
        n_reaches = n(),
        length_km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3),
        .groups   = "drop"
      ) %>%
      as.data.table()

    sp_pts_chr <- fish_pts_sub %>%
      filter(species == sp) %>%
      pull(subc_id) %>%
      as.character()

    frag_summary_sp <- frag_summary_sp %>%
      left_join(
        frag_dt %>%
          mutate(has_occurrence = subc_id %in% as.integer(sp_pts_chr)) %>%
          group_by(fragment_id) %>%
          summarise(n_occurrences = sum(has_occurrence), .groups = "drop"),
        by = "fragment_id"
      ) %>%
      mutate(
        species          = sp,
        scenario         = scenario_name
      ) %>%
      as.data.table()

    fwrite(frag_summary_sp,
           paste0("sdm/patch_metrics/fragments_",
                  scenario_name, "_", sp, ".csv"))

    fragment_rows[[paste(sp, scenario_name)]] <- data.table(
      species          = sp,
      scenario         = scenario_name,
      n_fragments      = frags$no,
      total_length_km  = round(sum(frag_summary_sp$length_km), 3),
      n_occ_in_network = sum(sp_pts_chr %in% as.character(frag_dt$subc_id))
    )

    message("  [", scenario_name, "] Fragments: ", frags$no,
            " | Suitable: ",
            round(sum(frag_summary_sp$length_km), 3), " km")
  }

  # ----------------------------------------------------------
  # Species summary row
  # ----------------------------------------------------------
  all_patch_summary[[sp]] <- data.table(
    species                = sp,
    n_suitable_reaches     = n_suitable,
    total_length_km        = total_len_km,
    n_patches              = n_patches,
    median_patch_length_km = round(median(patch_summary$length_km), 3),
    max_patch_length_km    = round(max(patch_summary$length_km),    3),
    n_occ_points           = nrow(fish_pts_sub %>% filter(species == sp)),
    longest_point_km       = round(longest_point_km,   3),
    longest_habitat_km     = round(longest_habitat_km, 3),
    min_interpatch_dist_km = if (!is.null(dist_patch_long))
      round(min(dist_patch_long$dist_km, na.rm = TRUE), 3) else NA_real_,
    max_interpatch_dist_km = if (!is.null(dist_patch_long))
      round(max(dist_patch_long$dist_km, na.rm = TRUE), 3) else NA_real_,
    min_interpoint_dist_km = if (!is.null(dist_point_long))
      round(min(dist_point_long$dist_km, na.rm = TRUE), 3) else NA_real_,
    max_interpoint_dist_km = if (!is.null(dist_point_long))
      round(max(dist_point_long$dist_km, na.rm = TRUE), 3) else NA_real_
  )
}

# ============================================================
# STEP 7: Save combined outputs
# ============================================================

message("\n=== Step 7: Saving combined outputs ===")

rbindlist(all_patch_summary, fill = TRUE) %>%
  { print(.); . } %>%
  fwrite("sdm/patch_metrics/patch_summary_all.csv")
message("  Saved: sdm/patch_metrics/patch_summary_all.csv")

rbindlist(longest_path_rows, fill = TRUE) %>%
  fwrite("sdm/patch_metrics/longest_path_all.csv")
message("  Saved: sdm/patch_metrics/longest_path_all.csv")

if (length(dam_proximity_rows) > 0) {
  rbindlist(dam_proximity_rows, fill = TRUE) %>%
    fwrite("sdm/patch_metrics/dam_patch_proximity.csv")
  message("  Saved: sdm/patch_metrics/dam_patch_proximity.csv")
}

if (length(buffer_overlap_rows) > 0) {
  rbindlist(buffer_overlap_rows, fill = TRUE) %>%
    fwrite("sdm/patch_metrics/dam_buffer_overlap.csv")
  message("  Saved: sdm/patch_metrics/dam_buffer_overlap.csv")
}

frag_summary_all <- rbindlist(fragment_rows, fill = TRUE)
fwrite(frag_summary_all, "sdm/patch_metrics/fragment_summary_all.csv")
message("  Saved: sdm/patch_metrics/fragment_summary_all.csv")

# ============================================================
# STEP 8: C4 — Species impact ranking
# ============================================================

message("\n=== Step 8: Species impact ranking ===")

ranking <- frag_summary_all %>%
  filter(scenario == "current") %>%
  select(species, n_frag_current = n_fragments,
         length_current = total_length_km) %>%
  left_join(
    frag_summary_all %>%
      filter(scenario == "future") %>%
      select(species, n_frag_future = n_fragments,
             length_future = total_length_km),
    by = "species"
  ) %>%
  mutate(delta_fragments = n_frag_future - n_frag_current) %>%
  arrange(desc(delta_fragments)) %>%
  mutate(impact_rank = row_number()) %>%
  as.data.table()

print(ranking)
fwrite(ranking, "sdm/patch_metrics/species_impact_ranking.csv")
message("  Saved: sdm/patch_metrics/species_impact_ranking.csv")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("HABITAT FRAGMENTATION METRICS COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nExtent:    Sarantaporos subbasin (trimmed from SDM predictions)")
message("Threshold: ", toupper(THRESHOLD_METHOD))
message("Scenario graphs: river_graph_current.RDS / river_graph_future.RDS")
message("\nNOTE — structural fragmentation (Group C) is uniform across species:")
message("  every dam is a cut. Species passability (0.8/0.5/0) enters only PCI.")
message("\nNext: 02_habitat_fragmentation_figures.R")
