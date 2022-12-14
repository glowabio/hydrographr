#' Calculate zonal statistics based on one or more environmental variable
#' raster .tif layers.
#'
#' This function can be used to aggregate data across a set (or all)
#' sub-catchments. The sub-catchment raster (.tif) input file is stored on disk.
#' The output is a data.table which is loaded into R.
#'
#' @param data_dir character. Path to the directory containing all input data
#' @param subc_ids Vector of sub-catchment IDs or "all".
#' If "all", zonal statistics are calculated for all sub-catchments
#' of the given sub-catchment raster layer. A vector of the sub-catchment IDs
#' can be acquired from the extract_ids() function, and by sub-setting
#' the resulting data.frame.
#' @param subc_layer character. Full path to the sub-catchment ID .tif layer
#' @param var_layer character vector of variable raster layers on disk,
#' e.g. "slope_grad_dw_cel_h00v00.tif". Variable names should remain
#' intact in file names, even after file processing,
#' i.e., slope_grad_dw_cel should appear in the file name.
#' The files should be cropped to the extent of the sub-catchment layer to
#' speed up the computation.
#' @param out_dir character. The directory where the output will be stored.
#' If the out_dir and file_name are specified, the output table will be stored
#' as a .csv file in this location. If they are NULL, the output is only
#' loaded in R and not stored on disk.
#' @param file_name character. Name of the .csv file where the output table
#' will be stored. out_dir should also be specified for this purpose.
#' @param n_cores Numeric. Number of cores used for parallelization. If NULL,
#' the physical available CPUs - 1 are used.
#' @param quiet Logical. Whether the standard output will be printed or not.
#'
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom parallel detectCores
#' @importFrom stringr str_c str_split
#' @import dplyr
#' @export
#'
#' @author Afroditi Grigoropoulou, Jaime Garcia Marquez, Maria M. Üblacker
#'
#' @seealso
#' \code{\link{report_no_data}} to check the defined NoData value.
#' \code{\link{set_no_data}} to define a NoData value.
#'
#' @references
#' \url{https://grass.osgeo.org/grass82/manuals/r.univar.html}
#'
#' @examples
#' # Specify the working directory of the test data
#' my_directory <- getwd()
#'
#' # Download the test data
#' download_test_data(my_directory)
#'
#' # Calculate the zonal statistics for all sub-catchments for two variables
#' stat <- extract_zonal_stat(data_dir = paste0(my_directory,
#'                                              "/hydrography90m_test_data"),
#'                            subc_ids = subc_ids <- c(513837216, 513841103,
#'                                                     513850467, 513868394,
#'                                                     513870312),
#'                            subc_layer = paste0(my_directory,
#'                                                "/hydrography90m_test_data",
#'                                                "/subcatchment_1264942.tif"),
#'                            var_layer = c("spi_1264942.tif",
#'                                          "sti_1264942.tif"),
#'                            out_dir = paste0(my_directory,
#'                                      "/hydrography90m_test_data/output/"),
#'                            file_name = "zonal_statistics.csv",
#'                            n_cores = 2)
#' # Show output table
#' stat
#'

extract_zonal_stat <- function(data_dir,  subc_ids, subc_layer, var_layer,
                               out_dir = NULL, file_name = NULL, n_cores = NULL,
                               quiet = TRUE) {

  # Introductory steps

  # check if the input is vector
  if (!is.vector(subc_ids)) {
    print("subc_ids should be either a vector of ids, or \"all\" ")
  }
  if (!is.vector(var_layer)) {
    print("The var_layer should be provided in a character vector format")
  }


  # Check if paths exist
  for (path in c(subc_layer, data_dir)) {
    if (!file.exists(path))
      stop(paste0("File path: ", path, " does not exist."))
  }

  # Check if subc_layer ends with .tif
  for (path in c(subc_layer, var_layer)) {
    if (!endsWith(path, ".tif"))
      stop(paste0("File path: ", path, " does not end with .tif"))
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
  if (!identical(subc_ids, "all")) {
    calc_all <- 0
    reclass <- rbind.data.frame(data.frame(str_c(subc_ids, " = ", 1)),
                                "* = NULL")
    fwrite(reclass, paste0(data_dir, tmp, "/reclass_rules.txt"), sep = "\t",
           row.names = FALSE, quote = FALSE, col.names = FALSE)

    # Format subc_ids vector so that it can be read
    # as an array in the bash script
    subc_ids <- paste(unique(subc_ids), collapse = "/")

  }

  # Setting up parallelization if n_cores is not provided
  if (is.null(n_cores)) {

    #  Detect number of available cores
    n_cores <- detectCores(logical = FALSE) - 1

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

  # Format subc_ids vector so that it can be read
  # as an array in the bash script
  var_layer_array <- paste(unique(var_layer), collapse = "/")

  # Call function report_no_data()
  no_data <- report_no_data(data_dir = data_dir, var_layer = var_layer,
                             n_cores = n_cores)
  print(noquote("The following NoData values are used for the calculation:"))
  print(no_data)


  # Check operating system
  system <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  # Run the zonal statistics function

  if (system == "linux" || system == "osx") {
    # Call the external .sh script extract_zonal_stat()
    # containing the grass functions
    processx::run(system.file("sh", "extract_zonal_stat.sh",
                  package = "hydrographr"),
                  args = c(data_dir, subc_ids, subc_layer,
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
                    args = c(wsl_data_dir, subc_ids, wsl_subc_layer,
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
  out_path <- paste0(out_dir, "/", file_name)
  if (!is.null(out_path)) {
    fwrite(var_table, out_path, sep = ",",
           row.names = FALSE, quote = FALSE, col.names = TRUE)
  }

  # Delete temporary output directory
  unlink(paste0(data_dir, tmp, "/"), recursive = TRUE)

  # Return table
  return(var_table)

}
