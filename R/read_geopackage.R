#' @title Read a GeoPackage file
#'
#' @description Reads an entire, or a subset of a GeoPackage vector file
#' from disk either as a table (data.table), as a directed graph object
#' (igraph), a spatial dataframe (sf) or a SpatVect object (terra).
#'
#' @param gpkg character. Full path of the GeoPackage file.
#' @param import_as character. "data.table", "graph", "sf", or "SpatVect".
#' "data.table" imports data as a data.table. "graph" imports the layer as a
#' directed graph (igraph object). This option is only possible for a network
#' layer (e.g. the stream network) and it needs to contain the attributes "stream"
#' and "next_stream". "sf" imports the layer as a  spatial data
#' frame (sf object). "SpatVect" imports the layer as a SpatVector (terra
#' object). Default is "data.table".
#' @param layer_name character. Name of the specific data layer to import from
#' the GeoPackage. A specific data layer only needs to be defined if the
#' GeoPackage contains multiple layers. To see the available layers the function
#' st_layers() from the R package 'sf' can be used. Optional. Default is NULL.
#' @param subc_id numeric. Vector of the sub-catchment (or stream
#' segment) IDs in the form of (c(ID1, ID2, ...) for which the spatial objects
#' or attributes of the GeoPackage should be imported. Optional. Default is NULL.
#' @param name character. The attribute table column name of the stream segment
#' ("stream"), sub-catchment ("ID"), basin ("ID") or outlet ("ID") column which
#' is used for subsetting the GeoPackage prior importing. Optional. Default is
#' "stream".
#' @importFrom DBI dbConnect dbListTables dbGetQuery dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom data.table setDT
#' @importFrom igraph graph_from_data_frame
#' @importFrom sf st_layers read_sf
#' @importFrom terra vect
#' @export
#'
#' @author Sami Domisch, Marlene Schürz
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#'
#' # Read the stream network as a graph
#' my_graph <- read_geopackage(gpkg = paste0(my_directory,
#'                                           "/hydrography90m_test_data",
#'                                           "/order_vect_59.gpkg"),
#'                                           import_as = "graph")
#'
#' # Read the stream network as a data.table
#' my_dt <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/order_vect_59.gpkg"))
#'
#' # Read the stream network as a data.table for specific IDs
#' my_dt <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/order_vect_59.gpkg"),
#'                                        subc_id = c(513833203, 513833594))
#'
#' # Read the sub-catchments as a SF-object
#' my_sf <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/sub_catchment_59.gpkg"),
#'                                        import_as = "sf",
#'                                        layer_name = "sub_catchment")
#'
#' # Read a subset of sub-catchments as a SF-object
#' my_sf <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/sub_catchment_59.gpkg"),
#'                                        import_as = "sf",
#'                                        subc_id = c(513833203, 513833594),
#'                                        name = "ID")
#'
#' # Read the basin as SpatVect object
#' my_sv <- read_geopackage(gpkg = paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/basin_59.gpkg"),
#'                                        import_as = "SpatVect")
#'




# Import a geopackage from disk as a data.table
read_geopackage <- function(gpkg, import_as = "data.table", layer_name = NULL,
                            subc_id = NULL, name = "stream") {

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
  # set this only within the function
  options(scipen = 999)

  if (import_as == "data.table" || import_as == "graph") {

    # Print message
    if (import_as == "data.table")
      cat("Importing as a data.table...\n")

    if ( import_as == "graph")
      cat("Importing as a graph...\n")

    # Create a connection to the SQLite database
    cat("Opening SQL connection...\n")
    conn <- dbConnect(drv = RSQLite::SQLite(), dbname = gpkg)
    # List all tables
    # layer_name <- dbListTables(conn

    # Read the database
    cat("Reading GeoPackage file...\n")
    if (missing(subc_id)) {
    network_table <- dbGetQuery(conn = conn,
                                statement = paste0("SELECT * FROM '",
                                                   layer_name, "'"))
    } else {
      subc_id2 <- paste(subc_id, collapse = ", ")
      network_table <- dbGetQuery(conn=conn, statement=paste0("SELECT * FROM '",
                                                  layer_name, "' WHERE ", name,
                                                  " in (", subc_id2, ")"))
    }

    # Convert to data.table
    setDT(network_table)

    # If vertices is NULL, then the first two columns of the data.frame
    # are used as a symbolic edge list and additional columns as edge attributes.
    # The names of the attributes are taken from the names of the columns.
    # Remove the columns for the subsequent igraph functions.

    # Which columns to remove:
    keep_these <- which(network_table[, !names(network_table) %in%
                                        c("fid", "geom", "cat")] == TRUE)
    network_table <- network_table[, ..keep_these]

    # Close db connection
    cat("Closing SQL connection...\n")
    dbDisconnect(conn)

    if (import_as == "data.table") {
    return(network_table)
    }

    if (import_as == "graph") { # only for network
      cat("Building graph...\n")
      g_out <- graph_from_data_frame(network_table, directed = TRUE)
      return(g_out)
      }

  } else if (import_as == "sf") {
    cat("Importing as a sf spatial dataframe...\n")
    # tibble=F to avoid exponential numbers
    if (missing(subc_id)) {
    sf <- read_sf(gpkg, as_tibble = FALSE)
    } else {
    subc_id2 <- paste(subc_id, collapse = ", ")
    sf <- read_sf(gpkg, query=paste0("SELECT * FROM '",
                                    layer_name, "' WHERE ", name,
                                    " in (", subc_id2, ")"))
    }
    return(sf)

  } else if (import_as == "SpatVect") {
    cat("Importing as a terra SpatVect object...\n")
    if (missing(subc_id)) {
    vect <- vect(gpkg)
    } else {
    subc_id2 <- paste(subc_id, collapse = ", ")
    vect <- vect(gpkg, query=paste0("SELECT * FROM '",
                                  layer_name, "' WHERE ", name,
                                  " in (", subc_id2, ")"))
    return(vect)
    }
  }

}
