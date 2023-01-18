#' @title Read a GeoPackage file
#'
#' @description Reads a GeoPackage vector file from disk either as a table
#' (data.table), as a directed graph object (igraph), a spatial dataframe (sf)
#' or a SpatVect object (terra).
#'
#' @param gpkg character. Full path of the GeoPackage file.
#' @param import_as character. "data.table", "graph", "sf", or "SpatVect".
#' "data.table" imports data as a data.table. "graph" imports the layer as a
#' directed graph (igraph object). This option is only possible for a network
#' layer (e.g. the stream network). "sf" imports the layer as a  spatial data
#' frame (sf object). "SpatVect" imports the layer as a SpatVector (terra
#' object). Default is "data.table".
#' @param layer_name character. Name of the specific data layer to import from
#' the GeoPackage. A specific data layer only needs to be defined if the
#' GeoPackage contains multiple layers. To see the available layers the function
#' st_layers() from the R package 'sf' can be used. Optional. Default is NULL.
#' @importFrom data.table as.data.table
#' @importFrom igraph graph_from_data_frame
#' @importFrom sf st_layers read_sf
#' @importFrom terra vect
#' @export
#'
#' @author Sami Domisch, Maria M.Üblacker
#'
#' @examples
#' # Download test data into temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#'
#' # Read the stream network as a graph
#' my_graph <- read_geopackage(gpkg = paste0(my_directory,
#'                                           "/hydrography90m_test_data",
#'                                           "/order_vect_1264942.gpkg"),
#'                             import_as = "graph")
#'
#' # Read the stream network as a data.table
#' my_dt <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/order_vect_1264942.gpkg"))
#'
#'
#' # Read the sub-catchments as a SF-object
#' my_sf <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/sub_catchment_1264942.gpkg"),
#'                          import_as = "sf",
#'                          layer_name = "sub_catchment_1264942")
#'
#' # Read the basin as SpatVect object
#' my_sv <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/basin_1264942.gpkg"),
#'                          import_as = "SpatVect")
#'




# Import a geopackage from disk as a data.table
read_geopackage <- function(gpkg, import_as = "data.table", layer_name = NULL) {

  # Test input arguments
  if (!file.exists(gpkg))
    stop(paste0("File: ", gpkg, "does not exist."))

  if (!grepl(".gpkg", gpkg, fixed = TRUE))
    stop("Input must be a GeoPackage file.")

  if (!any(c("data.table", "graph", "sf", "SpatVect") %in% import_as))
    stop("Please specify the input import type: one of 'data.table', 'graph',
         'sf','SpatVect'.")

  # Specify the layer name if no layer was defined
  if (missing(layer_name)) {
    layer_name <- st_layers(gpkg)$name
  }

  # Check number of available layers
  if (length(layer_name) > 1)
    stop("The GeoPackage contains multiple layers. Please, define the layer
         name you like to import.")


  # Avoid exponential numbers in the table and IDs,
  # only set this only within the function
  options(scipen = 999)
  # Print message
  if (import_as == "data.table")
    cat("Importing as a data.table...\n")

  if ( import_as == "graph")
    cat("Importing as a graph...\n")

  if ( import_as == "sf")
    cat("Importing as a sf spatial dataframe...\n")

  if (import_as == "SpatVect")
      cat("Importing as a terra SpatVect object...\n")

  sf_layer <- read_sf(gpkg, as_tibble = FALSE, layer = layer_name)

  # Convert to data.table
  network_table <- as.data.table(sf_layer)

  # If vertices is NULL, then the first two columns of the data.frame
  # are used as a symbolic edge list and additional columns as edge attributes.
  # The names of the attributes are taken from the names of the columns.
  # Remove the columns for the subsequent igraph functions.

  # Which columns to remove:
  keep_these <- which(network_table[, !names(network_table) %in%
                                        c("fid", "geom", "cat")] == TRUE)
  network_table <- network_table[, ..keep_these]

  if (import_as == "data.table") {
    return(network_table)
    }

  if (import_as == "graph") { # only for network
    cat("Building graph...\n")
    g_out <- graph_from_data_frame(network_table, directed = TRUE)
    return(g_out)
    }

  if (import_as == "sf") {
    return(sf_layer)
    }

  if (import_as == "SpatVect") {
    vect <- vect(sf_layer)
    return(vect)
    }

}
