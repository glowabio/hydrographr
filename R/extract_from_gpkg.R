#' Extract values from the stream order .gpkg files.
#'
#' The function reads the attribute table of the stream network GeoPackage file
#' (.gpkg) stored on disk and extracts the data for one or more (or all) input
#' sub-catchment (i.e. stream segment) IDs. The output is a data.table, and only
#' the output is loaded into R.
#'
#' @param data_dir Character. Path to the directory containing all input data
#' @param out_path Character. The optional full path of the output file. The
#' output is loaded always into R, and if the out_path is specified, the output
#' will be stored as a .csv in this location (e.g. "/path/to/output.csv")
#' @param subc_ids A vector of sub-catchment IDs or "all".
#' If "all", the .gpkg values are extracted for all the segments
#' of the given .gpkg layer. The stream segment ids are the same as the
#' sub-catchment IDs. A vector of the sub-catchment IDs can be acquired
#' from the extract_ids() function, by sub-setting the resulting data.frame.
#' @param subc_layer Character. Full path to the sub-catchment ID .tif layer
#' @param variables Character vector of one or multiple .gpkg file names,
#' e.g. "order_vect_point_h18v04.gpkg".
#' @param n_cores Numeric. Number of cores for parallelization. Defaults to
#' detectCores() - 1.
#'
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom rlang is_missing
#' @importFrom parallel detectCores
#' @importFrom stringr str_c
#' @import dplyr
#'
#' @author Afroditi Grigoropoulou, Jaime Garcia Marquez
#' @export
#'
#' @examples
#' library(hydrographr)
#'
#' # Specify the working directory of the test data
#' DATADIR <- "path/to/test_data"
#'
#' # Download the test data
#' download_test_data(DATADIR)
#'
#' my_dt <- extract_from_gpkg(data_dir=DATADIR, subc_ids="all")
#'

extract_from_gpkg <- function(data_dir, out_path = NULL, subc_ids,
                               subc_layer, variables, n_cores = NULL) {

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

  # Make bash scripts executable
  make_sh_exec()

  calc_all <- 1

  # Create file with reclassification rules for the r.reclass function
  if (!identical(subc_ids, "all")) {
    calc_all <- 0
    reclass <- rbind.data.frame(data.frame(str_c(subc_ids, " = ", 1)),
                                "* = NULL")
    fwrite(reclass, paste0(data_dir, "/tmp/reclass_rules.txt"), sep = "\t",
           row.names = FALSE, quote = FALSE, col.names = FALSE)

    # Format subc_ids vector so that it can be read
    # as an array in the bash script
    subc_ids <- paste(unique(subc_ids), collapse = " ")

  }

  # Setting up parallelization if n_cores is not provided
  if (is.null(n_cores)) {

    #  Detect number of available cores
    n_cores <- detectCores() - 1

  }


    # Get the variable names
    varnames <- gsub("gpkg", variables)

    # Format subc_ids vector so that it can be read
    # as an array in the bash script
    variables_array <- paste(unique(variables), collapse = " ")


    # Delete output files if they exist
    for (varname in varnames) {

      if (file.exists(paste0(data_dir, "/tmp/r_univar/stats_",
                             varname, ".csv"))) {
        file.remove(paste0(data_dir, "/tmp/r_univar/stats_",
                           varname, ".csv"))
      }
    }


    # Call the external .sh script extract_from_gpkg.sh
    # containing the grass functions
    processx::run(system.file("sh", "extract_from_gpkg.sh",
                    package = "hydrographr"),
        args = c(data_dir, subc_ids, subc_layer,
        variables_array, calc_all, n_cores),
        echo = FALSE)$stdout

    out_filenames <- list.files(paste0(data_dir, "/tmp/r_univar/"),
                                pattern = "stats_.*.csv", full.names = TRUE)

    out_files <- lapply(out_filenames, fread)

    var_table <- setDT(
      unlist(out_files, recursive = FALSE),
      check.names = TRUE
    )[]


  # Write out table if requested
  if (!is.null(out_path)) {
    fwrite(var_table, out_path, sep = ",",
           row.names = FALSE, quote = FALSE, col.names = TRUE)
  }


  # Delete temporary output directory
  unlink(paste0(data_dir, "/tmp/"), recursive = TRUE)

  # Return table
  return(var_table)

}
