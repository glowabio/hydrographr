#' @title Extract values from the stream order .gpkg files.
#'
#' @description The function reads the attribute table of the stream network GeoPackage file
#' (.gpkg) stored on disk and extracts the data for one or more (or all) input
#' sub-catchment (i.e. stream segment) IDs. The output is a data.table, and only
#' the output is loaded into R.
#'
#' @param data_dir character. Path to the directory containing all input data.
#' @param subc_id a numeric vector of sub-catchment IDs or "all".
#' If "all", the attribute table is extracted for all the stream segments of
#' the input .gpkg layer. The stream segment IDs are the same as the
#' sub-catchment IDs. A vector of the sub-catchment IDs can be acquired
#' from the extract_ids() function, by sub-setting the resulting data.frame.
#' @param subc_layer character. Full path to the sub-catchment ID .tif layer
#' @param var_layer character vector of .gpkg files on disk,
#' e.g. "order_vect_point_h18v04.gpkg".
#' @param out_dir character. The directory where the output will be stored.
#' If the out_dir is specified, the attribute tables will be stored as .csv
#' files in this location, named after their input variable vector files
#' (e.g. "/path/to/stats_order_vect_point_h18v04.csv").
#' If NULL, the output is only loaded in R and not stored on disk.
#' @param n_cores numeric. Number of cores used for parallelization, in case
#' multiple .gpkg files are provided to var_layer.
#' If NULL, available cores - 1 will be used.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom rlang is_missing
#' @importFrom stringi stri_rand_strings
#' @importFrom stringr str_c
#' @importFrom parallel detectCores
#' @import dplyr
#' @export
#'
#' @author Afroditi Grigoropoulou, Jaime Garcia Marquez, Maria M. Üblacker
#'
#' @references
#' \url{https://grass.osgeo.org/grass82/manuals/v.in.ogr.html}
#'
#' @examples
#' # Download test data into temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Define path to the directory containing all input data
#' test_data <- paste0(my_directory, "/hydrography90m_test_data")
#'
#' # Define sub-catchment ID layer
#' subc_raster <- paste0(my_directory, "/hydrography90m_test_data",
#'                   "/subcatchment_1264942.tif")
#'
#' # Extract the attribute table of the file order_vect_59.gpkg for all the
#' # sub-catchment IDs of the subcatchment_1264942.tif raster layer
#' attribute_table <- extract_from_gpkg(data_dir = test_data,
#'                                      subc_id = "all",
#'                                      subc_layer = subc_raster,
#'                                      var_layer = "order_vect_59.gpkg",
#'                                      n_cores = 1)
#'
#' # Show the output table
#' attribute_table


extract_from_gpkg <- function(data_dir, subc_id, subc_layer, var_layer,
                              out_dir = NULL, n_cores = NULL, quiet = TRUE) {

  # Check if one of the arguments is missing
  if (missing(data_dir))
    stop("data_dir is missing.")

  if (missing(subc_id))
    stop("data_id is missing.")

  if (missing(subc_layer))
    stop("subc_layer is missing.")

  if (missing(var_layer))
    stop("var_layer is missing.")

  # Check if paths exists
    if (!file.exists(data_dir))
      stop(paste0("Path: ", data_dir, " does not exist."))


  if (!is.null(out_dir))
    if (!file.exists(out_dir))
      stop(paste0("Path: ", out_dir, " does not exist."))

  # Check if the input is vector
  if (!is.vector(subc_id))
    stop("subc_id should be either a vector of ids, or 'all'")

  # Check if file exists
    if (!file.exists(subc_layer))
      stop(paste0("File path: ", subc_layer, " does not exist."))

  # Check if subc_layer ends with .tif
  if (!endsWith(subc_layer, ".tif"))
    stop("subc_layer: Sub-catchment ID layer is not a .tif file.")

  # Check if the input is vector
  if (!is.vector(var_layer))
    stop("The var_layer should be provided in a vector format.")

  for(name in var_layer) {

  file <- paste(data_dir, name, sep = "/")

  if (!file.exists(file))
    stop(paste0("File: ", var_layer, " does not exist."))

  }

  # Check if n_cores is numeric
  if (!is.null(n_cores))
    if (!is.numeric(n_cores))
      stop(paste0("n_cores: Has to be numeric."))

  # Create random string to attach to the tmp folder
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  tmp <- paste0("/tmp_", rand_string)
  # Create temporary output directories
  dir.create(paste0(data_dir, tmp), showWarnings = FALSE)

  calc_all <- 1

  # Create file with reclassification rules for the r.reclass function
  if (!identical(subc_id, "all")) {
    calc_all <- 0
    reclass <- rbind.data.frame(data.frame(str_c(subc_id, " = ", 1)),
                                "* = NULL")
    fwrite(reclass, paste0(data_dir, tmp, "/reclass_rules.txt"), sep = "\t",
           row.names = FALSE, quote = FALSE, col.names = FALSE)

    # Format subc_id vector so that it can be read
    # as an array in the bash script
    subc_id <- paste(unique(subc_id), collapse = "/")

  }

  # Setting up parallelization if n_cores is not provided
  if (is.null(n_cores)) {

    #  Detect number of available cores
    n_cores <- detectCores(logical = FALSE) - 1

  }


    # Get the variable names
    varnames <- gsub("gpkg", "", var_layer)

    # Format subc_id vector so that it can be read
    # as an array in the bash script
    var_layer_array <- paste(unique(var_layer), collapse = "/")


    # Delete output files if they exist
    for (varname in varnames) {

      if (file.exists(paste0(data_dir, tmp, "/r_univar/stats_",
                             varname, ".csv"))) {
        file.remove(paste0(data_dir, tmp, "/r_univar/stats_",
                           varname, ".csv"))
      }
    }

    # Check operating system
    sys_os <- get_os()

    # Make bash scripts executable
    make_sh_exec()

    # Run the zonal statistics function

    if (sys_os == "linux" || sys_os == "osx") {
      # Call the external .sh script extract_from_gpkg.sh
      # containing the grass functions
      processx::run(system.file("sh", "extract_from_gpkg.sh",
                                package = "hydrographr"),
                    args = c(data_dir, subc_id, subc_layer,
                             var_layer_array, calc_all, n_cores, rand_string),
                    echo = !quiet)

    } else {

      # Check if WSL and Ubuntu are installed
      check_wsl()
      # Change paths for WSL
      wsl_data_dir <- fix_path(data_dir)
      wsl_subc_layer <- fix_path(subc_layer)
      wsl_sh_file <- fix_path(system.file("sh", "extract_from_gpkg.sh",
                                          package = "hydrographr"))

      # Open GRASS GIS session on WSL
      # Call the external .sh script extract_from_gpkg.sh
      processx::run(system.file("bat", "extract_from_gpkg.bat",
                                package = "hydrographr"),
                    args = c(wsl_data_dir, subc_id, wsl_subc_layer,
                             var_layer_array, calc_all, n_cores, rand_string,
                             wsl_sh_file),
                    echo = !quiet)

    }

    out_filenames <- list.files(paste0(data_dir, tmp),
                                pattern = "stats_.*.csv", full.names = TRUE)

    out_files <- lapply(out_filenames, fread)

    var_table <- setDT(unlist(out_files, recursive = FALSE),
                       check.names = TRUE) %>%
      select(!starts_with("subc_id."))


  # Write out table if requested
  if (!is.null(out_dir)) {
    fwrite(var_table, out_dir, sep = ",",
           row.names = FALSE, quote = FALSE, col.names = TRUE)
  }


  # Delete temporary output directory
  unlink(paste0(data_dir, tmp, "/"), recursive = TRUE)

  # Return table
  return(var_table)

}
