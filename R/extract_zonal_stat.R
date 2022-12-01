#' Calculate statistics based on environmental variable raster layers
#'
#' Add the column 'subcID' to the dataset in 'dataset_path' and export the
#' data frame in a csv.
#' In the current version the long-lat values must be in the 2nd and 3rd
#' column of the data set text file.
#'
#' @param dataset_path Path to data set text file with the lat long columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
#' @param subc_path Path to the sub-catchment ID .tif layer
#' @param basin_path Path to the basin ID .tif layer
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %dopar%
#' @importFrom stringr str_c
#' @import dplyr
#' @export
#'

extract_zonal_stat <- function(subc_ids, subc_layer, variables, n_cores) {

  # check if the input is vector?
  if (!is.vector(subc_ids)) {
    print("The subcatchment ids should be in a vector format")
    }
  if (!is.vector(variables)) {
    print("The variables should be given in a vector format")
    }

  dir.create("tmp", showWarnings = FALSE)
  dir.create("tmp/r_univar", showWarnings = FALSE)


  # Create file with reclassification rules for the r.reclass function
  reclass <- rbind.data.frame(data.frame(str_c(subc_ids, " = ", 1)),
                                                        "* = NULL")
  fwrite(reclass, "tmp/reclass_rules.txt", sep = "\t",
                  row.names = FALSE, quote = FALSE, col.names = FALSE)

  # Format subc_ids vector so that it can be read
  # as an array in the bash script
  subc_ids <- paste(unique(subc_ids), collapse = " ")

  # Setting up parallelization

  # Create the cluster
  my_cluster <- makeCluster(n_cores, type = "PSOCK")
  # Register it to be used by %dopar%
  registerDoParallel(cl = my_cluster)

  var_table <- NULL
  var_table <- foreach(
      ivar = variables,
      .combine = "cbind",
      .packages = c("data.table", "dplyr", "processx")
      ) %dopar% {

    # Get the variable name
    varname <- gsub(".tif|.gpkg", "", ivar)

    #Delete file if it exists
    if (file.exists(paste0("tmp/r_univar/stats_",
                            varname, ".csv"))) {
          file.remove(paste0("tmp/r_univar/stats_",
                            varname, ".csv"))
    }


    # Call the external .sh script extr_var_stat()
    # containing the grass functions
    run(system.file("sh", "extract_var_stat.sh",
                              package = "hydrographr"),
                              args = c(subc_ids, subc_layer, ivar),
                              echo = FALSE)$stdout

    # Read in the resulting tables and sort them by subc_id,
    # to later join them using cbind
    var_table <- fread(paste0("tmp/r_univar/stats_",
                        varname, ".csv")) %>% arrange(subc_id)
    # Return table
    var_table
}

  # Stop cluster
  stopCluster(cl = my_cluster)

  # Delete file with reclassification rules
  file.remove("tmp/reclass_rules.txt")


  # Keep only the first one of the subc_id columns
  subc_id <- var_table$subc_id
  var_table <- var_table %>%
              select(-matches("subc_id")) %>%
              mutate(subc_id = subc_id) %>%
              relocate(subc_id, .before = everything())
  var_table


}
