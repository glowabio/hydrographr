#' Change or set the NoData value for a raster layer.
#' The change happens in-place, meaning that the original file is overwritten
#' on disk.
#'
#' @param data_dir Character. Path to the directory containing all input data.
#' @param variable Character vector. Variable file name,
#' e.g. slope_grad_dw_cel_h00v00.tif. The original file will be overwritten.
#' @param no_data The desired NoData value
#'
#' @importFrom processx run
#' @importFrom stringr str_split
#'
#' @author Afroditi Grigoropoulou
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
#' set_no_data(data_dir=DATADIR, variable="cti_1264942.tif", no_data=-9999)
#'

set_no_data <- function(data_dir, variable, no_data) {

  # Call the external .sh script set_no_data.sh
  # containing the gdal function
  output <- run(system.file("sh","set_no_data.sh",
                             package = "hydrographr"),
                 args = c(data_dir, variable, no_data),
                 echo = FALSE)$stdout

  # Format output message
  output <- str_split(output, "\n")[[1]][1]
  return(output)

}
