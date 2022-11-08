
# 7Nov22 sami

# For each segment, find all upstream connected segments and return a data-table.
# In addition, variables can be selected that should be aggregated, e.g. the upstream landcover.
# This function can also be used to create the connectivity table for Marxan by using aggregate_var="length" and aggregate_type=sum. The resulting table reports the upstream connectivity from each segment, along with the distance



# todo:
- cast into R-package format
- demo data
- use quotes or no quotes?

  
usePackage <- function(p){
  if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE) 
  library(p, character.only = TRUE)
}

usePackage("future.apply")
usePackage("doFuture")
usePackage("future.batchtools")
usePackage("dplyr")
usePackage("parallel")
  

# find_upstream <- function(input_graph, aggregate_var=c("length", "flow_accum"), aggregate_type="sum") {
find_upstream <- function(input_graph, aggregate_var, aggregate_type) {
  
  ### Get the path number as an additional column in the graph list.vs
  mapply_fun <- function(element,name){
    mutate(element,connected_to = name)
  }
  cat("Setting up parallel backend...\n")
  # Set up parallel backend
  n_cores <- detectCores(logical=F)-2
  registerDoFuture()
  plan(multisession, workers = n_cores)
  
  cat("Finding all upstream segments...\n")
  # Get all subcomponents
  l <- future_lapply(V(input_graph), function(x) subcomponent(input_graph, x, mode = c("out")))
  ### Get into datatable format
  l <- future_lapply(l, function(x) names(x))
  # specify names of list elements = connected_to
  l <- future_lapply(l, as.data.frame)
  ### Get the path number as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
  ### Merge as datatable
  dt <- rbindlist(l)
  names(dt)[1] <- "stream"
  setkey(dt, stream)
  
  # If aggregation was defined: 
  if(hasArg(aggregate_var)) {
  
  cat("Attaching the selected attributes...\n")
  ### Sort and remove the self-connection of the source stream
  dt <- dt[order(stream, connected_to),] 
  # dt <- subset(dt, stream != connected_to)
  
  # Get the attributes for each edge
  lookup_dt <- as.data.table(as_long_data_frame(input_graph)[c("ver[el[, 1], ]", aggregate_var)])
  names(lookup_dt)[1] <- "stream"
  
  # Merge the network attributes. The "connected_to" may have NAs which means that these are headwaters without upstream segments.
  # Keep these my coyping the edge ID into the column
  dt <- dt[lookup_dt, on="stream"] # left join, preserves ordering
  
  cat("Aggregating attributes...\n")
  
  dt_agg <- dt[,lapply(.SD, aggregate_type, na.rm=TRUE), 
                         .SDcols=aggregate_var,
                         by="stream"]
                
                      return(dt_agg)
                                }  else {
                                   return(dt)
                                        }
        
  cat("Clean up...\n")
  # Stop parallel backend
  plan(sequential, .cleanup = T) 
  
}



# test functions

# provides only the upstream connection to each stream
my_graph_largest_streams <- find_upstream(my_graph_largest)

# # provides besides the upstream connection to each stream, also the aggregation of variables
my_graph_largest_agg <- find_upstream(my_graph_largest, 
                                          aggregate_var=c("length", "flow_accum"),  
                                          aggregate_type=sum) # or mean, sd, ...


