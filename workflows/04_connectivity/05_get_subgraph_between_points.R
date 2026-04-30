get_subgraph_between_points <- function(graph, species_reach_ids, upstream_buffer = 3) {

  species_reach_ids <- as.character(species_reach_ids)

  # Keep only IDs that exist in graph
  occurrence_nodes <- species_reach_ids[species_reach_ids %in% igraph::V(graph)$name]

  if (length(occurrence_nodes) == 0) {
    warning("No occurrence nodes found in graph")
    return(NULL)
  }

  if (length(occurrence_nodes) == 1) {
    # Only one node — just take upstream buffer from it
    upstream <- igraph::ego(graph, order = upstream_buffer,
                            nodes = occurrence_nodes, mode = "in")
    return(igraph::induced_subgraph(graph, unique(unlist(upstream))))
  }

  # 1. Find all shortest paths between all pairs of occurrence nodes
  #    Use mode = "all" to traverse regardless of edge direction
  all_path_nodes <- c()

  for (i in seq_along(occurrence_nodes)) {
    paths <- suppressWarnings(igraph::shortest_paths(
      graph   = graph,
      from    = occurrence_nodes[i],
      to      = occurrence_nodes[-i],
      mode    = "all",       # undirected traversal — follows river network topology
      output  = "vpath"
    )
    )$vpath

    path_nodes <- unlist(lapply(paths, as.integer))
    all_path_nodes <- c(all_path_nodes, path_nodes)
  }

  all_path_nodes <- unique(all_path_nodes)

  # 2. Upstream buffer from every occurrence node
  upstream_nodes <- igraph::ego(
    graph = graph,
    order = upstream_buffer,
    nodes = occurrence_nodes,
    mode  = "in"
  )
  upstream_node_ids <- unique(unlist(lapply(upstream_nodes, as.integer)))

  # 3. Combine and return subgraph
  all_node_ids <- unique(c(all_path_nodes, upstream_node_ids))
  igraph::induced_subgraph(graph, all_node_ids)
}

# ###################### test
# library(sf)
# library(igraph)
# library(dplyr)
#
#
#
# output_dir <- "connectivity/species_subnetworks"
# dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
#
#
# test_ids <- occurrences |> filter(species == "Barbus_prespensis") |> pull(subc_id)
# test_sub <- get_subgraph_between_points(river_graph_current, test_ids)
# igraph::vcount(test_sub)  # should be much smaller than 131029
#
#
#
# # Clean species names for filenames (replace spaces with underscores)
# species_list <- unique(occurrences$species)
#
# for (sp in species_list) {
#
#   sp_filename <- gsub(" ", "_", sp)
#
#   # --- 1. Species occurrence points ---
#   sp_points <- occurrences |>
#     filter(species == sp) |>
#     st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)
#
#   st_write(
#     sp_points,
#     file.path(output_dir, paste0(sp_filename, "_occurrences.gpkg")),
#     delete_dsn = TRUE,
#     quiet = TRUE
#   )
#
#   # --- 2. Species subnetwork ---
#   sp_reach_ids <- occurrences |>
#     filter(species == sp) |>
#     pull(subc_id)
#
#   sp_subgraph <- get_subgraph_between_points(
#     graph             = river_graph_current,
#     species_reach_ids = sp_reach_ids,
#     upstream_buffer   = 3
#   )
#
#   sp_subc_ids <- as.integer(igraph::V(sp_subgraph)$name)
#
#   sp_network <- subcatchments |>
#     filter(subc_id %in% sp_subc_ids)
#
#   st_write(
#     sp_network,
#     file.path(output_dir, paste0(sp_filename, "_subnetwork.gpkg")),
#     delete_dsn = TRUE,
#     quiet = TRUE
#   )
#
#   message("Written: ", sp_filename)
# }
#
# subcs <- occurrences %>% filter(species=="Barbus_prespensis") %>% pull(subc_id)
# sub<-get_subgraph_between_points(
#   graph             = river_graph_current,
#   species_reach_ids = subcs,
#   upstream_buffer   = 3
# )
#
#
# species_subgraphs <- occurrences |>
#   split(~species) |>
#   lapply(function(df) {
#     get_subgraph_between_points(
#       graph             = river_graph_current,
#       species_reach_ids = df$subc_id,
#       upstream_buffer   = 3
#     )
#   })
