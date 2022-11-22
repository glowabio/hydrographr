
#' Convert a table to a graph
#'
#' Convert a table as an directed or undirected graph (igraph object), where the attributes will be attached to the edges (i.e. the stream segments). The input table needs to have the "stream" and "next_stream" (or any other name) as the first two columns.
#'
#' @param filename A data.frame or data.table that has the "stream" as the 1st column, and "next_stream" as the 2nd column. Column names are flexible.
#' @param directed If TRUE (the default), then the output will be a directed graph, else an undirected graph.

#' @importFrom data.table setDT
#' @importFrom igraph graph_from_data_frame
#' @export
#'



# usePackage <- function(p){
#   if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
#   library(p, character.only = TRUE)
# }
#
# usePackage("DBI")
# usePackage("data.table")
# usePackage("igraph")



# convert table to a directed graph
table_to_graph <- function(filename, directed=T) {
  # Avoid exponential numbers in the reclassification, only set this only within the function
  options(scipen=999)

  if(directed==TRUE) {
  g <- graph_from_data_frame(filename, directed = T, vertices=NULL)
  return(g)
  } else if (directed==FALSE) {
  g <- graph_from_data_frame(filename, directed = F, vertices=NULL)
      }

  }



# test function
# my_graph <- table_to_graph(my_table, directed=T)


