
#' Import GeoPackage file as multiple formats
#'
#' Import a Geopackage vector file , either as a table (data.table), as a directed graph object (igraph), a spatial dataframe (sf) or a SpatVect object (terra).
#'
#' @param filename Name of the GeoPackage file to import, e.g. "order_vect_59.gpkg"
#' @param dt If TRUE, import the GeoPackage as a data.table
#' @param graph If TRUE, import the GeoPackage as a directed graph (igraph object)
#' @param sf If TRUE, import the GeoPackage as a spatial dataframe (sf object)
#' @param SpatVect If TRUE, import the GeoPackage as a SpatVecteor (terra object)

#' @importFrom DBI dbConnect dbListTables dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom data.table setDT
#' @importFrom igraph graph_from_data_frame
#' @importFrom sf vect
#' @importFrom terra vect
#' @export
#'





# To do:
# load demo data data

#
# usePackage <- function(p){
#   if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
#   library(p, character.only = TRUE)
# }
#
# usePackage("DBI")
# usePackage("data.table")
# usePackage("igraph")
# usePackage("terra")
#
#
# path <- "D:/projects/hydrographr/hydrographr_data"
# setwd(path)



# Import a geopackage from disk as a data.table
read_geopackage <- function(filename, dt=F, graph=F, sf=F, SpatVect=F) {
  # Avoid exponential numbers in the table and IDs, only set this only within the function
  options(scipen=999)
  # create a connection to the SQLite database
  conn <- dbConnect(drv=RSQLite::SQLite(), dbname=filename) # get connection
  conn_table <- dbListTables(conn) # ## list all tables
  conn_table <- conn_table[1] # "SELECT"
  network_table <- dbGetQuery(conn=conn, statement=paste0("SELECT * FROM '", conn_table, "'")) # Read the database
  setDT(network_table) # convert to data.table
  #If vertices is NULL, then the first two columns of d are used as a symbolic edge list and additional columns as edge attributes. The names of the attributes are taken from the names of the columns. Remove the columns for the subsequent igraph functions.
  network_table$geom <- NULL
  network_table$fid <- NULL
  network_table$cat <- NULL
  dbDisconnect(conn) # close db connection

  if(dt==T) {
  return(network_table)
  }

  if (graph==T) {
    g <- graph_from_data_frame(network_table, directed = T)
    return(g) }

   if(sf==T) {
    sf <- read_sf("order_vect_59.gpkg")
    return(sf)  }

   if (SpatVect==T) {
    sf <- read_sf("order_vect_59.gpkg")
    vect <- vect(sf)
    return(vect)  }

}


### Test function
my_table <- read_geopackage("order_vect_59.gpkg", dt=T) # loads a data.table
my_graph <- read_geopackage("order_vect_59.gpkg", graph=T) # loads as an directed graph
my_sf <- read_geopackage("order_vect_59.gpkg", sf=T) # loads as an directed graph
my_vect <- read_geopackage("order_vect_59.gpkg", SpatVect=T) # loads as an directed graph




