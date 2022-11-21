
#' Import GeoPackage file as a data.table or as a directed graph
#'
#' Import a Geopackage vector file into R, either as a data.table or as a directed igraph object.
#'
#' @param filename Name of the GeoPackage file to import, e.g. "order_vect_59.gpkg"
#' @param graph In case of a network, should the date be imported as a data.table (graph=FALSE, the default) or as a igraph object (graph=TRUE)

#' @importFrom DBI dbConnect dbListTables dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom data.table setDT
#' @importFrom igraph graph_from_data_frame
#' @export
#'





# To do:
# load demo data data
# load libraries


# usePackage <- function(p){
#   if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE) 
#   library(p, character.only = TRUE)
# }
# 
# usePackage("DBI")
# usePackage("data.table")
# usePackage("igraph")



path <- "D:/projects/hydrographr/hydrographr_data"
setwd(path)



# Import a geopackage from disk as a data.table
read_geopackage <- function(filename, graph=F) {
  # Avoid exponential numbers in the reclassification, only set this only within the function 
  options(scipen=999)
  # create a connection to the SQLite database
  conn <- dbConnect(drv=RSQLite::SQLite(), dbname=filename) # get connection
  conn_table <- dbListTables(conn) # ## list all tables
  conn_table <- conn_table[1] # "SELECT"
  network_table <- dbGetQuery(conn=conn, statement=paste0("SELECT * FROM '", conn_table, "'")) # Read the database
  setDT(network_table) # convert to data.table
  #If vertices is NULL, then the first two columns of d are used as a symbolic edge list and additional columns as edge attributes. The names of the attributes are taken from the names of the columns.
  network_table$geom <- NULL   
  network_table$fid <- NULL 
  network_table$cat <- NULL 
  dbDisconnect(conn) # close db connection
  
  if(graph==T) {
    g <- graph_from_data_frame(network_table, directed = T)
    if (graph==T) print(g)
    return(g) 
              } else {
  return(network_table)
                    }  
              }


### Test function
# my_table <- read_geopackage("order_vect_59.gpkg", graph=F) # loads a data.table
# my_graph <- read_geopackage("order_vect_59.gpkg", graph=T) # loads as an directed graph





# ### Could also use this, but not as a data.table, also keeping redundant information
# as.data.table(read_sf("order_vect_59.gpkg"))
