
# status 7Nov22 (Sami)

# Convert data.table to a directed graph


# To do:
# cast into package-format
# define packages
# load demo data


usePackage <- function(p){
  if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE) 
  library(p, character.only = TRUE)
}

usePackage("DBI")
usePackage("data.table")
usePackage("igraph")



# convert table to a directed graph
table_to_graph <- function(network_table, print=F) {
  # Avoid exponential numbers in the reclassification, only set this only within the function 
  options(scipen=999)
  g <- graph_from_data_frame(network_table, directed = T, vertices=NULL)
  if (print==T) print(g)  # print graph summary
  return(g)
  }



# test functions
my_graph <- table_to_graph(my_table)
my_graph <- table_to_graph(my_table, print=T)


