#' @title Get all upstream distances for all subc_id
#'
#' @description Calculates the upstream distance from each subc_id to all
#' upstream subc_id. The output can be directly used in spatial prioritization
#' analyses for e.g. Marxan, Gurobi etc. to specify the longitudinal connectivity.
#' Note that the stream segment and sub-catchment IDs are identical, and for
#' consistency, we use the term "subc_id".
#'
#' Note that the distance can be extremely long and for the subsequent spatial
#' prioritization analyses you might want to consider setting a cap at a
#' certain distance.
#'
#' @param network_table A data.table that includes the columns
#' \code{c(stream, next_stream, out_dist}, the latter specifying the distance
#' to the outlet (which is included in the Hydrography90m vector attribute table).
#' @param n_cores numeric. Number of cores used for parallelisation
#' in the case of multiple stream segments / s. Default is 1.
#' Currently, the parallelisation process requires copying the data to each
#' core. In case the graph is very large, and many segments are
#' used as an input, setting n_cores to a higher value can speed up the
#' computation. This comes however at the cost of possible RAM limitations
#' and even slower processing since the large data will be copied to each core.
#' Hence consider testing with n_cores = 1 first. Optional.
#'
#' @returns A data.table that reports the distance (in meters) from
#' each subc_id to all upstream subc_ids.
#'
#' @importFrom foreach foreach
#' @importFrom foreach foreach getDoParWorkers
#' @importFrom data.table setnames rbindlist
#' @importFrom parallel makePSOCKcluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @export
#'
#' @author Sami Domisch
#'
#' @references
#' Csardi G, Nepusz T: The igraph software package for complex network research,
#' InterJournal, Complex Systems 1695. 2006. \url{https://igraph.org}
#'
#' @seealso
#' \code{\link{read_geopackage()}} and \code{\link{get_catchment_graph()}} to
#' create a network graph. Alternatively, see
#' \code{\link{get_segment_neighbours()}} to obtain the upstream variables for
#' a specified neighbourhood, or \code{\link{get_upstream_variable()}} to
#' aggregate a set of variables across the upstream catchment.
#'
#' @note
#' Currently the attributes are not provided for the  (the selected
#' subc_id segment). If the attributes are also needed for the outlet subc_id,
#' then the next downstream sub_id can be selected (enlarge the study area)
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load stream network as a graph
#' my_graph <- read_geopackage(gpkg = paste0(my_directory,
#'                                          "/hydrography90m_test_data",
#'                                          "/order_vect_59.gpkg"),
#'                            import_as = "graph")
#'
#' # Pick a random subc_id
#' subc_id = "513867228"
#' # Get the upstream catchment as a data.table
#' network_table <- hydrographr::get_catchment_graph(g = my_graph ,
#'                                                   subc_id = subc_id,
#'                                                   mode = "in",
#'                                                   use_outlet = FALSE,
#'                                                   as_graph = FALSE,
#'                                                   n_cores = 1)
#'
#' ## Condense the table supplied to the function to save RAM
#' VAR <- "out_dist"
#' keep_these <- c("stream", "next_stream", VAR)
#' network_table <- network_table[, ..keep_these]
#'
#' ## Change to integers
#' network_table$stream <- as.integer(network_table$stream)
#' network_table$next_stream <- as.integer(network_table$next_stream)
#'
#'
#' ## Calculate the network distance (in meter) from each subc_id to
#' ## all upstream subc_id using four CPUs for the parallelization
#' result <- get_all_upstream_distances(network_table = network_table,
#'                                       n_cores = 4)
#'



get_all_upstream_distances <- function(network_table = network_table,
                                       n_cores = 1) {


  ## Check if input exists
  if(missing(network_table) == TRUE)
    stop("Please provide the input data.table.")

   ## Check if input is a data.table
  if(checkDT(network_table) != TRUE)
    stop("The input must be data.table object.")

  ## Check columns of upstream_dt
  if(all(c("stream", "next_stream", "out_dist") %ni% names(network_table)))
    stop("The input datatable needs to have")


## Register parallel backend
## Make cluster object
n_cores=4
cl <- makePSOCKcluster(n_cores) # outfile=""
registerDoParallel(cl) # register parallel backend
getDoParWorkers() # show number of workers


options(scipen = 999)

## List for storing the results
results <- list()

# .combine=rbindlist,
### Start loop in parallel
results <- foreach(outrow=1:nrow(network_table),
                   # .combine=rbind,
                   .packages = c("data.table"),
                   .errorhandling = "stop",
                   .verbose=F) %dopar% {

                     # Print main iterator
                     cat("Running subc_id", outrow, "of", nrow(network_table), "\n") # ------- use this to track each basins progress -------

                     # If tmp_row does not exist, or tmp_row is empty from last iteration, get new iterator from main file (1st column)
                     if (!exists("tmp_row")  ||  is.null(dim(tmp_row$stream))  ) {
                       # Subset by row in the shapefile
                       tmp_row <- network_table[outrow,]

                       # If next_stream is -1, skip as it's the last basin (write as ID2=NA and remove later)
                       if (tmp_row$next_stream == -1) {
                         # Get the main basin (Marxan ID1)
                         mybas <- tmp_row$stream
                         # Write into datatable
                         dt <- data.table(X1=mybas, X2=NA)

                         # Save result in list
                         results[[outrow]] <- dt

                       } else {

                         # Get the main basin (Marxan ID1), going upstream from the NextDown
                         mybas <- tmp_row$next_stream
                         nextup <- tmp_row$stream
                         # Write into dataframe
                         dt <- data.table(X1=mybas, X2=nextup)
                         # dt <- data.frame(matrix(NA, nrow = length(1), ncol = 2))
                         # dt[,1] <- mybas # insert FROM ID, NAs will be deleted later on
                         # dt[,2] <- nextup

                         # Get the next upstream basin
                         tmp_row <- network_table[network_table$next_stream %in% tmp_row$stream,]

                         # Multiple upstream basins can flow into the one below
                         if (!is.null(length(tmp_row$stream))) { # check if there are upstream basins

                           while(nrow(tmp_row) >= 1) { # run as long as there are upstream basins
                             # Print inner upstream iterator
                             # cat(">>> Connecting", nrow(tmp_row), "upstream basins \n") # ------- uncomment this to track each basins progress -------

                             # Write upstream basins to file
                             for (i in 1:nrow(tmp_row)) { # remove duplicates in the end (else start from 2nd item..)
                               nextup <- tmp_row$stream[i]
                               dt <- rbindlist(list(dt, data.table(X1=mybas, X2=nextup)))
                               # dt <- rbind(dt, data.table(X1=mybas, X2=nextup))
                             } # close for loop
                             # Get the next upstream basins
                             tmp_row <- network_table[network_table$next_stream %in% tmp_row$stream,]
                           } # close while loop for multiple basins
                         } # close if condition

                         # Remove duplicates and write into list
                         dt <- dt[!duplicated(dt),]
                         results[[outrow]] <- dt

                       } # close skipzero as NextDown
                     } # close if tmp_row does not exist or empty
                   } # close outer for loop


### Merge results
results <- rbindlist(results)

### Stop the cluster object
stopCluster(cl)

## Remove NAs
results <- results[complete.cases(results),]

### Check if basins in X1 also exist in the basin input file (NextDown could belong to adjacent watershed)
results <- results[results$X1 %in% network_table$stream,]

### Remove duplicates if any
results <- results[!duplicated(results),]

### Sort by stream (FROM), then next_stream (TO)
results <- results[with(results, order(X1, X2)), ]


### Remove rows where X1 == X2
# out$drop <- ifelse(out$X1 == out$X2, 1, 0)
# out <- subset(out, out$drop==0)
# out

### Change column names
setnames(results, c("stream", "stream_up"))
setkey(results, stream)



### Merge distance from each subc_id (in m) to main outlet separately

### Distance from each segement to the downstream pour point-segment
### Create ID column
results$seq_id <- seq.int(1:nrow(results))


results_stream <- merge(results, network_table,  by="stream", allow.cartesian=TRUE)
results_stream_up <- merge(results, network_table,  by.x="stream_up", by.y="stream", allow.cartesian=TRUE)

## Remove possible duplicates
results_stream <- results_stream[!duplicated(results_stream),]
results_stream_up <- results_stream_up[!duplicated(results_stream_up),]


### Sort by the seq_id
results_stream <- results_stream[with(results_stream, order(seq_id)), ]
results_stream_up <- results_stream_up[with(results_stream_up, order(seq_id)), ]

## Get the distance
results$dist_m <-  results_stream_up$out_dist - results_stream$out_dist
results$dist_m <- as.integer(round(results$dist_m))
results[,seq_id:=NULL] # remove id column
return(results)

}


