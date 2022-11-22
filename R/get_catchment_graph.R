

# 7Nov22, sami

### identify largest drainage basin in a tile

- need to add the argument "outlet="segment_ID" "
- in the best case:
- get outlet IDs as table and scan for outlets in the given vector file. Use the outlet to run subcompartment to get the graph. Faster than the largest-drainage-function, and would get all drainages in the tile (which could be however incomplete if river stretches from adjacent tile)



usePackage <- function(p){
  if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE) 
  library(p, character.only = TRUE)
}

usePackage("future.apply")
usePackage("doFuture")
usePackage("future.batchtools")
usePackage("dplyr")
usePackage("parallel")





get_largest_catchment <- function(input_graph) {
  
  ### Get the path number as an additional column in the graph list.vs
  mapply_fun <- function(element,name){
    mutate(element,connected_to = name)
  }
  
  
  cat("Setting up parallel backend...\n")
  # Set up parallel backend
  n_cores <- detectCores(logical=F)-2
  registerDoFuture()
  plan(multisession, workers = n_cores)
  
  

  
  # start_time <- Sys.time()
  cat("Finding all upstream segments...\n")
  # Get all subcomponents
  l <- future_lapply(V(input_graph), function(x) subcomponent(input_graph, x, mode = c("out")))
  ### Get into datatable format
  l <- future_lapply(l, function(x) names(x))
  # names(l) <- seq.int(1:length(l)) # specify names of list elements = connected_to
  l <- future_lapply(l, as.data.frame)
  ### Get the path number as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
  ### Merge as datatable
  dt <- rbindlist(l)
  names(dt)[1] <- "stream"
  setkey(dt, stream)
  
  # end_time <- Sys.time()
  # end_time - start_time
  
  cat("Defining largest catchment...\n")
  # Which stream segment has the largest catchment = most upstream segments?
  seg_count <- dt[, .N, by=.(stream)]
  seg_count <- seg_count[order(-N),] # ordered
  
  ### Remove vertex -1 that represents the outlet, and keep the edge = segment flowing into the outlet
  if(seg_count[1,1]== -1) {
    seg_count <- seg_count[-1,]
  }
  
  # Get the edge id of the last catchment segement and all the upstream edges 
  largest_catch_id <- seg_count[1,1] 
  largest_catch_g <- subcomponent(input_graph, largest_catch_id$stream, mode = c("in"))
  
  cat("Subsetting the original graph...\n")
  # Subset the input graph
  input_graph_sub <- subgraph(input_graph,largest_catch_g)
  
  # Stop parallel backend
  plan(sequential, .cleanup = T) 
  
  return(input_graph_sub)
}



# Test function
my_graph_largest <- get_largest_catchment(my_graph)


