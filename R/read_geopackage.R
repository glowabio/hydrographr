#' @title Read a GeoPackage file
#'
#' @description Reads a GeoPackage vector file from disk either as a table
#' (data.table), as a directed graph object (igraph), a spatial dataframe (sf)
#' or a SpatVect object (terra).
#'
#' @param file_name character. Name of the GeoPackage file to import,
#' e.g. "order_vect_segment_h00v00.gpkg".
#' @param type character. Either "net" for importing a network, "subc" for
#' sub-catchments, "basin" for drainage basins, or "outlet" for outlet points.
#' @param SQL_table character. Name of the specific data to import from the
#' GeoPackage. This is set automatically for the Hydrography90m data,
#' but needs to be specified for any other data. Optional.
#' @param as_dt logical. If TRUE, import the GeoPackage as a data.table.
#' Default is FALSE.
#' @param as_graph "logical. If TRUE, import the GeoPackage as a directed graph
#' (igraph object). Only possible for a network layer. Default is FALSE.
#' @param as_sf logical. If TRUE, import the GeoPackage as a spatial dataframe
#' (sf object). Default is FALSE.
#' @param as_SpatVect logical. If TRUE, import the GeoPackage as a
#' SpatVector (terra object). Default is FALSE.

#' @importFrom DBI dbConnect dbListTables dbGetQuery dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom data.table setDT
#' @importFrom igraph graph_from_data_frame
#' @importFrom sf read_sf
#' @importFrom terra vect
#' @export
#'
#' @examples
#' # Download test data into temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Define
#'
#' # Read the stream network as a graph
#' my_graph <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "net", as_graph = T)
#'
#' # Read the stream network as a data.table
#' my_dt <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "net", as_dt = T)
#'
#' # Read the sub-catchments as a data.table
#' my_dt <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "subc", as_dt = T)
#'
#' # Read the basin as a SF-object
#' my_sf <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "basin", as_sf = T)
#'
#'#' # Read the basin as SpatVect object
#' my_sf <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "basin", as_SpatVect = T)
#' # Read the outlets as data.table
#' my_sf <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "outlet", as_sf = T)
#'
#' @author Sami Domisch



# Import a geopackage from disk as a data.table
read_geopackage <- function(file_name, type = NULL, SQL_table = NULL,
                            as_dt = FALSE, as_graph = FALSE, as_sf = FALSE,
                            as_SpatVect = FALSE) {

  # Test input arguments
  if (missing(file_name))
    stop("Please specify the input geopackage file.")

  if (!grepl(".gpkg", file_name, fixed = TRUE))
    stop("Input must be a GeoPackage file.")

  # If all options are missing
  if (as_dt == FALSE && as_graph == FALSE && as_sf == FALSE
      && as_SpatVect == FALSE) {
    stop("Please select one output type.\n")
  }

  if (missing(type))
    stop("Please specify 'net' or 'subc' as the input file type.")

  if (!any(c("net", "subc", "basin", "outlet") %in% type))
    stop("Please specify the input file type: one of 'net',
         'subc', 'basin' or 'outlet'.")

  if (any(c("subc", "basin", "outlet") %in% type) && as_graph == TRUE)
    stop("A graph can only be created from a network.")


  # Avoid exponential numbers in the table and IDs,
  # only set this only within the function
  options(scipen = 999)

  if (as_dt == TRUE || as_graph == TRUE) {
   if (as_dt == TRUE && as_graph == FALSE) cat("Importing as a data.table...\n")
   if (as_graph == TRUE) cat("Importing as a graph...\n")

  # create a connection to the SQLite database
  cat("Opening SQL connection...\n")
  conn <- dbConnect(drv = RSQLite::SQLite(), dbname = file_name) # get connection
  # SQL_table <- dbListTables(conn)
  # list all tables
  # specify the layer name
  if (missing(SQL_table)) {
    if (type == "net") {
      SQL_table <- "merged"
        } else if (type == "subc") {
          SQL_table <- "sub_catchment"
        } else if (type == "basin") {
          SQL_table <- "basin"
          } else if (type == "outlet") {
            SQL_table <- "outlet"
          }
  }

  cat("Reading GeoPackage file...\n")
  network_table <- dbGetQuery(conn = conn,
  statement = paste0("SELECT * FROM '", SQL_table, "'")) # Read the database
  setDT(network_table) # convert to data.table

  #If vertices is NULL, then the first two columns of d
  # are used as a symbolic edge list and additional columns
  # as edge attributes.
  # The names of the attributes are taken from the names of the columns.
  # Remove the columns for the subsequent igraph functions.

  # which columns to remove:
  toss_these <- which(
    network_table[, !names(network_table) %in% c("geom", "fid", "cat")] == TRUE
    )
  network_table <- network_table[, ..toss_these]

  cat("Closing SQL connection...\n")
  dbDisconnect(conn) # close db connection

  if (as_dt == TRUE && as_graph == FALSE) {
    return(network_table)
    }

  if (as_graph == TRUE && type == "net") { # only for network
    cat("Building graph...\n")
    g_out <- graph_from_data_frame(network_table, directed = TRUE)
    return(g_out)
    }

} else if (as_sf == TRUE) {
    cat("Importing as a sf spatial dataframe...\n")
    # tibble=F to avoid exponential numbers
    sf <- read_sf(file_name, as_tibble = FALSE)
    return(sf)
    } else if (as_SpatVect == TRUE) {
    cat("Importing as a terra SpatVect object...\n")
    sf <- read_sf(file_name)
    vect <- vect(sf)
    return(vect)
    }

}
