#' Change or set the NoData value for a raster layer.
#' The change happens in-place, meaning that the original file is overwritten
#' on disk.
#'
#' @param data_dir character. Path to the directory containing all input layers
#' @param var_layer character vector of var_layer layers on disk,
#' e.g. c("sti_h16v02.tif", "slope_grad_dw_cel_h00v00.tif").
#' The original files will be overwritten.
#' @param no_data numeric. The desired NoData value
#'
#' @importFrom processx run
#' @importFrom stringr str_split
#' @export
#'
#' @author Afroditi Grigoropoulou, Maria M. Üblacker
#'
#'@examples
#' # Download test data into temporary R folder
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Define no data value
#' set_no_data(data_dir = paste0(my_directory, "/hydrography90m_test_data"),
#'             var_layer = "cti_1264942.tif",
#'             no_data = -9999)
#'

set_no_data <- function(data_dir, var_layer, no_data) {

  # Check if path exists
  if (!dir.exists(data_dir))
    stop(paste0(data_dir, " does not exist."))

  # Check if file exists
  if (!file.exists(paste(data_dir, var_layer, sep = "/")))
    stop(paste0(data_dir, "/", var_layer, " does not exist."))

  # Check if no data value is numeric
  if (!is.numeric(no_data))
    stop("no_data: Value has to be numeric.")

  # Check operating system
  system <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (system == "linux" || system == "osx") {
  # Call the external .sh script set_no_data.sh
  # containing the gdal function
  output <- processx::run(system.file("sh", "set_no_data.sh",
                             package = "hydrographr"),
                 args = c(data_dir, var_layer, no_data),
                 echo = FALSE)$stdout
  } else {
    # Check if WSL and Ubuntu are installed
    check_wsl()
    # Change paths for WSL
    wsl_data_dir <- fix_path(data_dir)
    wsl_sh_file <- fix_path(system.file("sh", "set_no_data.sh",
                                        package = "hydrographr"))

    # Open GRASS GIS session on WSL
    # Call external GRASS GIS command r.reclass
    output <- processx::run(system.file("bat", "set_no_data.bat",
                                        package = "hydrographr"),
                            args = c(wsl_data_dir, var_layer, no_data,
                                     wsl_sh_file),
                            echo = FALSE)$stdout
  }
  # Format output message
  output <- str_split(output, "\n")[[1]][1]
  return(output)

}
