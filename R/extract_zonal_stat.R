#' Calculate zonal statistics based on environmental variable raster and vector layers.
#'
#' @param data_dir Character. Path to the directory containing all input data
#' @param out_path Character. Full path of the output file.
#' If not NULL, the output data.frame is exported as a csv in the given path
#' @param subc_ids Vector of sub-catchment ids or "all".
#' If "all", zonal statistics are calculated for all the sub-catchments of the given sub-catchment layer. A vector of the sub-catchment ids can be acquired from the extract_ids() function, by sub setting the resulting data.frame
#' @param subc_layer Character. Full path to the sub-catchment ID .tif layer
#' @param variables Character vector. Variable file names, e.g. slope_grad_dw_cel_h00v00.tif.
#' Variable names should remain intact in file names, even after prior file processing,
#' i.e., slope_grad_dw_cel should appear in the file name.
#' The files should be cropped to the extent of the sub-catchment layer
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %dopar%
#' @importFrom stringr str_c
#' @importFrom tidyr separate
#' @import dplyr
#' @export
#'

extract_zonal_stat <- function(data_dir, out_path = NULL, subc_ids,
                               subc_layer, variables) {

  # Introductory steps

  # check if the input is vector
  if (!is.vector(subc_ids)) {
    print("subc_ids should be either a vector of ids, or \"all\" ")
  }
  if (!is.vector(variables)) {
    print("The variables should be provided in a vector format")
  }


  # Create temporary output directories
  dir.create(paste0(data_dir, "/tmp"), showWarnings = FALSE)
  dir.create(paste0(data_dir, "/tmp/r_univar"), showWarnings = FALSE)

  calc_all <- 1

  # Create file with reclassification rules for the r.reclass function
  if (!identical(subc_ids, "all")) {
    calc_all <- 0
    reclass <- rbind.data.frame(data.frame(str_c(subc_ids, " = ", 1)),
                                "* = NULL")
    fwrite(reclass, paste0(data_dir,"/tmp/reclass_rules.txt"), sep = "\t",
           row.names = FALSE, quote = FALSE, col.names = FALSE)

    # Format subc_ids vector so that it can be read
    # as an array in the bash script
    subc_ids <- paste(unique(subc_ids), collapse = " ")

  }


  # Setting up parallelization
  #  Detect number of available cores
  # n_cores <- detectCores() - 1
  n_cores <- 6

  # Create the cluster
  my_cluster <- makeCluster(n_cores, type = "PSOCK")

  # Register it to be used by %dopar%
  registerDoParallel(cl = my_cluster)

  # Run the zonal statistics function in a foreach loop
  var_table <- NULL
  var_table <- foreach(
    ivar = variables,
    .combine = "cbind",
    .packages = c("data.table", "dplyr", "processx")
  ) %dopar% {

    # Get the variable name
    varname <- gsub(".tif", "", ivar)

    # Delete file if it exists
    if (file.exists(paste0(data_dir, "/tmp/r_univar/stats_",
                           varname, ".csv"))) {
      file.remove(paste0(data_dir, "/tmp/r_univar/stats_",
                         varname, ".csv"))
    }


    # Call the external .sh script extract_zonal_stat()
    # containing the grass functions
    run(system.file("sh", "extract_zonal_stat.sh",
                    package = "hydrographr"),
        args = c(data_dir, subc_ids, subc_layer, ivar, calc_all),
        echo = FALSE)$stdout

    print(varname)

    # Read in the resulting tables and sort them by subc_id,
    # to later join them using cbind
    var_table <- fread(paste0(data_dir,"/tmp/r_univar/stats_",
                              varname, ".csv")) %>% arrange(subc_id)

  }

  # Write out table if requested
  if(!is.null(out_path)) {
    fwrite(var_table, out_path, sep = ",",
           row.names = FALSE, quote = FALSE, col.names = TRUE)
  }

  # Return table
  return(var_table)



  # Stop cluster
  stopCluster(cl = my_cluster)

  # Delete temporary output directory
  unlink(paste0(data_dir, "/tmp/"), recursive = TRUE)

}
