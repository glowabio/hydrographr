
#' Read a GeoPackage file
#'
#' Read a Geopackage vector file from disk either as a table (data.table),
#' as a directed graph object (igraph), a spatial dataframe (sf)
#' or a SpatVect object (terra).
#'
#' @param filename Name of the GeoPackage file to import,
#' e.g. "order_vect_segment_h00v00.gpkg"
#' @param type Either "net" for importing a network, "catch" for sub-catchments,
#' "basin" for drainage basins, or "outlet" for outlet points.
#' @param SQL_table Optional. Name of the specific data
#' to import from the GeoPackage.
#' This is set automatically for the Hydrography90m data,
#' and needs to be specified for any other data.
#' @param dt If TRUE, import the GeoPackage as a data.table.
#' @param g If TRUE, import the GeoPackage as a directed graph (igraph object).
#' Only possible for a network.
#' @param sf If TRUE, import the GeoPackage as a spatial dataframe (sf object).
#' @param SpatVect If TRUE, import the GeoPackage as a
#' SpatVecteor (terra object).

#' @importFrom DBI dbConnect dbListTables dbGetQuery dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom data.table setDT
#' @importFrom igraph graph_from_data_frame
#' @importFrom sf read_sf
#' @importFrom terra vect
#' @export
#'
#' @examples
#' # Read the stream network as a graph
#' my_graph <- read_geopackage("order_vect_59.gpkg", type="net", g=T)
#'
#' # Read the stream network as a data.table
#' my_dt <- read_geopackage("order_vect_59.gpkg", type="net", dt=T)
#'
#' # Read the sub-catchments as a data.table
#' my_dt <- read_geopackage("sub_catchment_59.gpkg", type="catch", dt=T)
#'
#' # Read the basin as a SF-object
#' my_sf <- read_geopackage("basin_59.gpkg", type="basin", sf=T)
#'
#'#' # Read the basin as SpatVect object
#' my_sf <- read_geopackage("basin_59.gpkg", type="basin", SpatVect=T)
#' # Read the outlets as data.table
#' my_sf <- read_geopackage("outlet_59.gpkg", type="outlet", sf=T)
#'
#' @author Sami Domisch



# Import a geopackage from disk as a data.table
read_geopackage <- function(filename, type = NULL, SQL_table = NULL,
                            dt = FALSE, g = FALSE, sf = FALSE,
                            SpatVect = FALSE) {

  # Test input arguments
  if (missing(filename)) stop("Please specify the input geopackage file.")
  if (!grepl(".gpkg", filename, fixed = TRUE)) stop(
    "Input must be a GeoPackage file."
    )
  # If all options are missing
  if (dt == FALSE && g == FALSE && sf == FALSE && SpatVect == FALSE) {
    stop("Please select one output type.\n")
  }
  if (missing(type)) stop(
    "Please specify 'net' or 'catch' as the input file type."
    )
  if (!any(c("net", "catch", "basin", "outlet") %in% type)) stop(
    "Please specify the input file type: 
    one of 'net', 'catch', 'basin' or 'outlet'."
    )
  if (any(c("catch", "basin", "outlet") %in% type) && g == TRUE) stop(
    "A graph can only be created from a network.")


  # Avoid exponential numbers in the table and IDs,
  # only set this only within the function
  options(scipen = 999)

  if (dt == TRUE || g == TRUE) {
    if (dt == TRUE && g == FALSE)  cat("Importing as a data.table...\n")
    if (g == TRUE)  cat("Importing as a graph...\n")

  # create a connection to the SQLite database
  cat("Opening SQL connection...\n")
  conn <- dbConnect(drv = RSQLite::SQLite(), dbname = filename) # get connection
  # SQL_table <- dbListTables(conn)
  # list all tables
  # specify the layer name
  if (missing(SQL_table)) {
    if (type == "net") {
      SQL_table <- "merged"
        } else if (type == "catch") {
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

  if (dt == TRUE && g == FALSE) {
    return(network_table)
    }

  if (g == TRUE && type == "net") { # only for network
    cat("Building graph...\n")
    g_out <- graph_from_data_frame(network_table, directed = TRUE)
    return(g_out)
    }

} else if (sf == TRUE) {
    cat("Importing as a sf spatial dataframe...\n")
    # tibble=F to avoid exponential numbers
    sf <- read_sf(filename, as_tibble = FALSE)
    return(sf)
    } else if (SpatVect == TRUE) {
    cat("Importing as a terra SpatVect object...\n")
    sf <- read_sf(filename)
    vect <- vect(sf)
    return(vect)
    }

}
