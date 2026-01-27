#' @title Calculate zonal statistics
#'
#' @description Calculate zonal statistics based on one or more environmental
#' variable raster .tif layers. This function aggregates data to
#' 12 summary statistics (mean, min, max, range, ...) for selected or all
#' sub-catchments of the input file. The sub-catchment raster (.tif) input
#' file is read directly from disk. The output is a data.table which is loaded
#' into R. This function can also be used for any zonal statistic calculation
#' by specifying the raster layer zones in the subc_layer parameter and
#' optionally, also the target zone IDs in the subc_id parameter.
#'
#' @param data_dir character. Path to the directory containing all input data.
#' @param subc_id Vector of sub-catchment IDs or "all".
#' If "all", zonal statistics are calculated for all sub-catchments
#' of the given sub-catchment raster layer. A vector of the sub-catchment IDs
#' can be acquired from the extract_ids() function, and by sub-setting
#' the resulting data.frame.
#' @param subc_layer character. Full path to the sub-catchment ID .tif layer.
#' @param var_layer character vector of variable raster layers on disk,
#' e.g. "slope_grad_dw_cel_h00v00.tif". Note that the variable name appears in
#' the output table columns (e.g. slope_grad_dw_cel_mean). To speed up the
#' processing, the selected variable raster layers can be cropped to the extent
#' of the sub-catchment layer, e.g. with \code{\link{crop_to_extent()}}.
#' @param out_dir character. The directory where the output will be stored.
#' If the out_dir and file_name are specified, the output table will be stored
#' as a .csv file in this location. If they are NULL, the output is only
#' loaded in R and not stored on disk.
#' @param file_name character. Name of the .csv file where the output table
#' will be stored. out_dir should also be specified for this purpose.
#' @param n_cores numeric. Number of cores used for parallelization, in case
#' multiple .tif files are provided to var_layer. Default is 1.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom parallel detectCores
#' @importFrom stringr str_c str_split
#' @importFrom stringi stri_rand_strings
#' @import dplyr
#' @export
#'
#' @author Afroditi Grigoropoulou, Jaime Garcia Marquez, Marlene Schürz
#'
#' @seealso
#' * \code{\link{report_no_data()}} to check the defined NoData value.
#' * \code{\link{set_no_data()}} to define a NoData value.
#' * \code{\link{crop_to_extent()}} to crop the data to the same extent as the
#' sub-catchments (subc_layer).
#' @md
#'
#' @references
#' \url{https://grass.osgeo.org/grass82/manuals/r.univar.html}
#'
#' @return Returns a table with
#' * sub-catchment ID (subc_id)
#' * number of cells with a value (data_cells)
#' * number of cells with a NoData value (nodata_cells)
#' * minimum value (min)
#' * maximum value (max)
#' * value range (range)
#' * arithmetic mean (mean)
#' * arithmetic mean of the absolute values (mean_abs)
#' * standard deviation (sd)
#' * variance (var)
#' * coefficient of variation (cv)
#' * sum (sum)
#' * sum of the absolute values (sum_abs).
#' @md
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Define full path to the sub-catchment ID .tif layer
#' subc_raster <-  paste0(my_directory, "/hydrography90m_test_data",
#'                        "/subcatchment_1264942.tif")
#'
#' # Define the directory where the output will be stored
#' output_folder <- paste0(my_directory, "/hydrography90m_test_data/output")
#' # Create output folder if it doesn't exist
#' if(!dir.exists(output_folder)) dir.create(output_folder)
#'
#' # Calculate the zonal statistics for all sub-catchments for two variables
#' stat <- extract_zonal_stat(data_dir = paste0(my_directory,
#'                                              "/hydrography90m_test_data"),
#'                            subc_id = c(513837216, 513841103,
#'                                        513850467, 513868394,
#'                                        513870312),
#'                            subc_layer = subc_raster,
#'                            var_layer = c("spi_1264942.tif",
#'                                          "sti_1264942.tif"),
#'                            out_dir = output_folder,
#'                            file_name = "zonal_statistics.csv",
#'                            n_cores = 2)
#' # Show output table
#' stat
#'

extract_zonal_stat <- function(data_dir,  subc_id, subc_layer, var_layer,
                               out_dir = NULL, file_name = NULL, n_cores = NULL,
                               quiet = TRUE) {

  # Introductory steps

  # check if the input is vector
  if (!is.vector(subc_id)) {
    print("subc_id should be either a vector of ids, or \"all\" ")
  }
  if (!is.vector(var_layer)) {
    print("The var_layer should be provided in a character vector format")
  }


  # Check if paths exist
  if (!file.exists(data_dir))
    stop(paste0("Path: ", data_dir, " does not exist."))

  if (!file.exists(subc_layer))
     stop(paste0("File path: ", subc_layer, " does not exist."))

  # Check if subc_layer ends with .tif
  if (!endsWith(subc_layer, ".tif"))
    stop(paste0("File path: ", subc_layer, " does not end with .tif"))
  # Check if var_layer exist
  for (name in var_layer){
    file <- paste(data_dir, name, sep ="/")
    if (!file.exists(file))
      stop(paste0("File: ", var_layer, " does not exist."))
  }

  # Create random string to attach to the tmp folder
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  tmp <- paste0("/tmp_", rand_string)
  # Create temporary output directories
  dir.create(paste0(data_dir, tmp), showWarnings = FALSE)
  dir.create(paste0(data_dir, tmp, "/r_univar"),
             showWarnings = FALSE)

  calc_all <- 1

  # Create file with reclassification rules for the r.reclass function
  if (!identical(subc_id, "all")) {
    calc_all <- 0
    reclass <- rbind.data.frame(data.frame(str_c(subc_id, " = ", 1)),
                                "* = NULL")
    fwrite(reclass, paste0(data_dir, tmp, "/reclass_rules.txt"), sep = "\t",
           row.names = FALSE, quote = FALSE, col.names = FALSE)

  }

  # Setting up parallelization if n_cores is not provided
  if (is.null(n_cores)) {

    #  Detect number of available cores
    # n_cores <- detectCores(logical = FALSE) - 1
    n_cores <- 1

  }

  # Get the variable names
  varnames <- gsub(".tif", "", var_layer)

  # Delete files if they exist
  for (varname in varnames) {

    if (file.exists(paste0(data_dir, tmp, "/r_univar/stats_",
                           varname, ".csv"))) {
      file.remove(paste0(data_dir, tmp, "/r_univar/stats_",
                         varname, ".csv"))
    }
  }

  # Format subc_id vector so that it can be read
  # as an array in the bash script
  var_layer_array <- paste(unique(var_layer), collapse = "/")

  # Call function report_no_data()
  no_data <- report_no_data(data_dir = data_dir, var_layer = var_layer,
                             n_cores = n_cores)
  print(noquote("The following NoData values are used for the calculation:"))
  print(no_data)


  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  # Run the zonal statistics function

  if (sys_os == "linux" || sys_os == "osx") {
    # Call the external .sh script extract_zonal_stat()
    # containing the grass functions
    processx::run(system.file("sh", "extract_zonal_stat.sh",
                  package = "hydrographr"),
                  args = c(data_dir, subc_layer,
                           var_layer_array, calc_all, n_cores, rand_string),
                  echo = !quiet)

    } else {
      # Check if WSL and Ubuntu are installed
      check_wsl()
      # Change paths for WSL
      wsl_data_dir <- fix_path(data_dir)
      wsl_subc_layer <- fix_path(subc_layer)
      wsl_sh_file <- fix_path(system.file("sh", "extract_zonal_stat.sh",
                                          package = "hydrographr"))

      # Open GRASS GIS session on WSL
      # Call external GRASS GIS command r.reclass
      processx::run(system.file("bat", "extract_zonal_stat.bat",
                                package = "hydrographr"),
                    args = c(wsl_data_dir, wsl_subc_layer,
                             var_layer_array, calc_all, n_cores, rand_string,
                             wsl_sh_file),
                    echo = !quiet)

    }

  # Read in the resulting variable statistics tables and join them
  out_filenames <- list.files(paste0(data_dir, tmp, "/r_univar/"),
                              pattern = "stats_.*.csv", full.names = TRUE)

  out_files <- lapply(out_filenames, fread)

  var_table <- setDT(unlist(out_files, recursive = FALSE),
                     check.names = TRUE) %>%
               select(!starts_with("subc_id."))

  # Write out master table if requested
  # Compose full out_path by combining out_dir and file_name
  if (!is.null(out_dir)) {
    out_path <- paste0(out_dir, "/", file_name)
    fwrite(var_table, out_path, sep = ",",
           row.names = FALSE, quote = FALSE, col.names = TRUE)
  }

  # Delete temporary output directory
  unlink(paste0(data_dir, tmp, "/"), recursive = TRUE)

  # Return table
  return(var_table)

}
