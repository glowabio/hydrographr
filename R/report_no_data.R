#' Report no data value of raster layers.
#'
#' @param data_dir Character. Path to the directory containing all input data.
#' @param variables Character vector. The names of the raster files.
#' e.g. slope_grad_dw_cel_h00v00.tif.
#' #' @param n_cores Numeric. Number of cores used for parallelization
#' @importFrom processx run
#' @importFrom stringr str_split
#' @importFrom parallel detectCores
#' @author Afroditi Grigoropoulou
#' @export
#'

report_no_data <- function(data_dir, variables, n_cores = NULL) {

  # Setting up parallelization if n_cores is not provided
  if (is.null(n_cores)) {

    #  Detect number of available cores
    n_cores <- detectCores() - 1

  }

  # Format variables vector so that it can be read
  # as an array in the bash script
  variables_array <- paste(unique(variables), collapse = " ")

  # Make bash scripts executable
  make_sh_exec()

  # Call the external .sh script report_no_data()
  reports <- processx::run(system.file("sh", "report_no_data.sh",
                             package = "hydrographr"),
                 args = c(data_dir, variables_array, n_cores),
                 echo = FALSE)$stdout


  # Format output message
  reports <- str_split(reports, "\n")
  return(reports)

}
