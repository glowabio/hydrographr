#' Change or set no data value to raster layers.
#' The change happens in-place, meaning that the original file is overwritten
#'
#' @param data_dir Character. Path to the directory containing all input data.
#' @param variable Character vector. Variable file name,
#' e.g. slope_grad_dw_cel_h00v00.tif. Variable names should remain
#' intact in file names, even after prior file processing,
#' i.e., slope_grad_dw_cel should appear in the file name
#' The files should be cropped to the extent of the sub-catchment layer
#' @param no_data The desired no data value
#' @importFrom processx run
#' @importFrom stringr str_split
#' @author Afroditi Grigoropoulou
#' @export
#'

set_no_data <- function(data_dir, variable, no_data) {

  # Call the external .sh script set_no_data.sh
  # containing the gdal function
  output <- processx::run(system.file("sh", "set_no_data.sh",
                             package = "hydrographr"),
                 args = c(data_dir, variable, no_data),
                 echo = FALSE)$stdout

  # Format output message
  output <- str_split(output, "\n")[[1]][1]
  return(output)

}
