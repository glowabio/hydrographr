#' @title Get precit table
#'
#' @description This function ...
#'
#' @param variable character vector of variable names.
#' @param statistics character vector of statics names. Possible values are C("sd", "mean") or "ALL". 
#' Default "ALL"
#' @param tile_id character. The IDs of the tiles of interest.
#' @param out_dir character. The path to environmental variables.
#' @param out_file character. The path to the output file.
#' @param n_cores numeric. Number of cores used for parallelization.
#' @param read logical. If TRUE, the table with environmental variables gets read into R.
#' If FALSE, the table is only stored on disk. Default is TRUE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra ext
#' @export
#'
#' @return The function returns...
#'
#' @author Jaime García Márquez, Yusdiel Torres-Cambas
#'
#' @examples
      
get_predict_table <- function(variable, statistics = "ALL", tile_id, read = TRUE, quiet = TRUE) {

    # Check if a vector with variable names was provided
    if(missing(variable))
        stop("Please provide at least the name of one variable. Possible names are: ")

    # Check if a vector with tile ids was provided
    if (missing(tile_id)) {
       stop("Please provide at least one tile ID")
    }

    # Check if a path to the output file is provided
    if (missing(out_file)) {
       stop("Please provide a path to the output file")
    }


  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Check if read is logical
  if (!is.logical(read))
    stop("read: Has to be TRUE or FALSE.")

}