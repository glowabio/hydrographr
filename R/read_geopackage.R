
#' Read a GeoPackage file
#'
#' Read a Geopackage vector file from disk either as a table (data.table), as a directed graph object (igraph), a spatial dataframe (sf) or a SpatVect object (terra).
#'
#' @param filename Name of the GeoPackage file to import, e.g. "order_vect_segment_h00v00.gpkg"
#' @param type Either "net" for importing a network, or "catch" for sub-catchments.
#' @param SQL_table Optional. Name of the specific data to import from the GeoPackage. Defaults to "merged" in the case of the Hydrography90m stream network.
#' @param dt If TRUE, import the GeoPackage as a data.table.
#' @param g If TRUE, import the GeoPackage as a directed graph (igraph object). Only possible for a network.
#' @param sf If TRUE, import the GeoPackage as a spatial dataframe (sf object).
#' @param SpatVect If TRUE, import the GeoPackage as a SpatVecteor (terra object).

#' @importFrom DBI dbConnect dbListTables dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom data.table setDT
#' @importFrom igraph graph_from_data_frame
#' @importFrom sf vect
#' @importFrom terra vect
#' @export
#'




# Import a geopackage from disk as a data.table
read_geopackage <- function(filename, type=NULL, dt=F, g=F, sf=F, SpatVect=F, SQL_table=NULL) {

  # Test input arguments
  if(missing(filename)) stop("Please specify the input geopackage file.")
  if(!grepl(".gpkg", filename, fixed = TRUE)) stop("Input must be a GeoPackage file.")
  # If all options are missing
  if(dt==FALSE & g==FALSE & sf==FALSE & SpatVect==FALSE) {
    stop("Please select one output type.\n")
  }
  if(type!="net" & type!="catch") stop("Please specify 'net' or 'catch' as the input file type.")
  if(type=="catch" & g==TRUE) stop("A graph can only be created from a network.")


  # Avoid exponential numbers in the table and IDs, only set this only within the function
  options(scipen=999)

  if(dt==TRUE || g==TRUE) {
    if(dt==TRUE & g==F)  cat("Importing as a data.table...\n")
    if(g==TRUE)  cat("Importing as a graph...\n")

  # create a connection to the SQLite database
  cat("Opening SQL connection...\n")
  conn <- dbConnect(drv=RSQLite::SQLite(), dbname=filename) # get connection
  # SQL_table <- dbListTables(conn) # list all tables
  if(missing(SQL_table)) {
    if(type=="net") {
      SQL_table <- "merged"
        } else if(type=="catch") {
          SQL_table <- "sub_catchment"
        }
  }

  cat("Reading GeoPackage file...\n")
  network_table <- dbGetQuery(conn=conn, statement=paste0("SELECT * FROM '", SQL_table, "'")) # Read the database
  setDT(network_table) # convert to data.table
  #If vertices is NULL, then the first two columns of d are used as a symbolic edge list and additional columns as edge attributes. The names of the attributes are taken from the names of the columns. Remove the columns for the subsequent igraph functions.
  try(network_table[, c("geom","fid", "cat"):=NULL], silent = T)
  # network_table$geom <- NULL
  # network_table$fid <- NULL
  # network_table$cat <- NULL
  cat("Closing SQL connection...\n")
  dbDisconnect(conn) # close db connection

  if(dt==T & g==F) {
    return(network_table) }

  if (g==T & type=="net") { # only for network
    cat("Building graph...\n")
    g_out <- graph_from_data_frame(network_table, directed = T)
    return(g_out) }

} else if (sf==T) {
    cat("Importing as a sf spatial dataframe...\n")
    sf <- read_sf(filename, as_tibble=F) # tibble=F to avoid exponential numbers
    return(sf)  } else if (SpatVect==T) {
    cat("Importing as a terra SpatVect object...\n")
    sf <- read_sf(filename)
    vect <- vect(sf)
    return(vect)
    }

}


### Test function

# usePackage <- function(p){
#   if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
#   library(p, character.only = TRUE)
# }
#
# usePackage("DBI")
# usePackage("data.table")
# usePackage("igraph")
# usePackage("terra")
# usePackage("sf")
#
# path <- "D:/projects/hydrographr/hydrographr_data"
# setwd(path)



# my_table <- read_geopackage("order_vect_59.gpkg", dt=T) # loads a data.table
# my_graph <- read_geopackage("order_vect_59.gpkg", g=T) # loads as an directed graph
# my_sf <- read_geopackage("order_vect_59.gpkg", sf=T) # loads as an spatial dataframe
# my_vect <- read_geopackage("order_vect_59.gpkg", SpatVect=T) # loads as an terra object
#
#
# big file:
# filename="order_vect_segment_h10v08.gpkg"
# my_graph <- read_geopackage("order_vect_segment_h10v08.gpkg", g=T)
# my_table <- read_geopackage("order_vect_segment_h10v08.gpkg", g=T)
#
# small file
# filename="order_vect_segment_h00v00.gpkg"
# filename="sub_catchment_h00v00.gpkg"
# my_table <- read_geopackage("order_vect_segment_h00v00.gpkg", type="net", dt=T)
# my_graph <- read_geopackage("order_vect_segment_h00v00.gpkg", g=T)
# my_sf <- read_geopackage("order_vect_segment_h00v00.gpkg", sf=T)
# my_vect <- read_geopackage("order_vect_segment_h00v00.gpkg", SpatVect=T)



# my_table <- read_geopackage("sub_catchment_h00v00.gpkg", type="catch", SpatVect=T)


