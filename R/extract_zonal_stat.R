#' Calculate zonal statistics based on environmental variable raster and vector layers.
#'
#' @param data_dir Path to the directory containing all input data
#' @param out_path Full path of the output file. If not NULL, the output data.frame is exported as a csv in the given path
#' @param subc_ids Vector of sub-catchment ids, can be acquired from the extract_ids() function, by sub setting the resulting data.frame
#' @param subc_layer Full path to the sub-catchment ID .tif layer
#' @param variables Variable file names, e.g. slope_grad_dw_cel_h00v00.tif. Variable names should remain intact in file names, even after prior file processing, i.e., slope_grad_dw_cel should appear in the file name
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom rlang is_missing
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
    print("The subcatchment ids should be in a vector format")
  }
  if (!is.vector(variables)) {
    print("The variables should be given in a vector format")
  }

  # Create temporary output directories
  dir.create(paste0(data_dir, "/tmp"), showWarnings = FALSE)
  dir.create(paste0(data_dir, "/tmp/r_univar"), showWarnings = FALSE)

  # Create file with reclassification rules for the r.reclass function
  reclass <- rbind.data.frame(data.frame(str_c(subc_ids, " = ", 1)),
                              "* = NULL")
  fwrite(reclass, paste0(data_dir,"/tmp/reclass_rules.txt"), sep = "\t",
         row.names = FALSE, quote = FALSE, col.names = FALSE)

  # Format subc_ids vector so that it can be read
  # as an array in the bash script
  subc_ids <- paste(unique(subc_ids), collapse = " ")

  # Setting up parallelization
  #  Detect number of available cores
  n_cores <- detectCores() - 1

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
    varname <- gsub(".tif|.gpkg", "", ivar)

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
        args = c(data_dir, subc_ids, subc_layer, ivar),
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
  unlink(paste0(data_dir, "/tmp"))

}
