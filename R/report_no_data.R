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


  # Check parallel backend depending on the OS
  if (get_os() == "windows") {
    plan(multisession, workers = n_cores)
  }
  if (get_os() == "osx" || get_os() == "linux")  {
    plan(multicore, workers = n_cores) # multicore?
  }
}

  # Format variables vector so that it can be read
  # as an array in the bash script
  variables_array <- paste(unique(variables), collapse = " ")


  ###################
  if (get_os() == "linux" || get_os() == "osx"){

    reports <- processx::run(system.file("sh", "report_no_data.sh",
                                         package = "hydrographr"),
                             args = c(data_dir, variables_array, n_cores),
                             echo = FALSE)$stdout
    # Format output message
    reports <- str_split(reports, "\n")

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_data_dir <- fix_path(data_dir)
    wsl_sh_file <- fix_path(system.file("sh",
                                        "report_no_data.sh",
                                        package = "hydrographr"))

    reports <- run(system.file("bat", "report_no_data.bat",
                    package = "hydrographr"),
        args = c(wsl_data_dir, variables_array,
                 n_cores, wsl_sh_file),
        echo = !quiet)

    # Format output message
    reports <- str_split(reports, "\n")
  }



  #################




  reports

}
