#' Reports NoData value of raster layers.
#'
#' This function reports the defined NoData value of a raster layer. The NoData
#' value of a raster layer represents the absence of data. In computations the
#' NoData value can be treated in different ways. Either the NoData value will
#' be reported or the Nodata value will be ignored and a value is computed from
#' the available values of a specified location.
#'
#' @param data_dir character. Path to the directory containing all input data.
#' @param var_layer character vector of variable raster layers on disk,
#' e.g. "slope_grad_dw_cel_h00v00.tif".
#' @param n_cores numeric. Number of cores used for parallelization, in case
#' multiple .tif files are provided to var_layer.
#' @importFrom processx run
#' @importFrom tidyr separate
#' @importFrom parallel detectCores
#' @export
#'
#' @author Afroditi Grigoropoulou, Maria M. Üblacker
#'
#' @references
#' \url{https://gdal.org/programs/gdalinfo.html}
#'
#' @examples
#' # Download test data into temporary R folder
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Report the NoData value
#' report_no_data(data_dir = paste0(my_directory, "/hydrography90m_test_data"),
#'                variable = c("subcatchment_1264942.tif", "flow_1264942.tif",
#'                              "spi_1264942.tif"),
#'                n_core = 2)
#'


report_no_data <- function(data_dir, var_layer, n_cores = NULL) {

  # Check if path exists
  if (!dir.exists(data_dir))
    stop(paste0(data_dir, " does not exist."))

  # Setting up parallelization if n_cores is not provided
  if (is.null(n_cores)) {

    #  Detect number of available cores
    n_cores <- detectCores(logical = FALSE) - 1

  }

  # Format var_layer vector so that it can be read
  # as an array in the bash script
  var_layer_array <- paste(unique(var_layer), collapse = "/")

  # Check operating system
  system <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (system == "linux" || system == "osx") {
  # Call the external .sh script report_no_data()
  reports <- processx::run(system.file("sh", "report_no_data.sh",
                             package = "hydrographr"),
                 args = c(data_dir, var_layer_array, n_cores),
                 echo = FALSE)$stdout

  } else {
    # Check if WSL and Ubuntu are installed
    check_wsl()
    # Change paths for WSL
    wsl_data_dir <- fix_path(data_dir)
    wsl_sh_file <- fix_path(system.file("sh", "report_no_data.sh",
                                         package = "hydrographr"))

    # Open GRASS GIS session on WSL
    # Call external GRASS GIS command r.reclass
    reports <-  processx::run(system.file("bat", "report_no_data.bat",
                                           package = "hydrographr"),
                            args = c(wsl_data_dir, var_layer_array, n_cores,
                                     wsl_sh_file),
                            echo = FALSE)$stdout
  }
  # Format output message
  reports <- as.data.table(strsplit(reports, "\n")) %>%
    separate(col = V1, c("Raster", "NoData"), sep = "=")
  return(reports)

}
