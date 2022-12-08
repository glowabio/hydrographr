#' Snap data points to the next stream segment
#'
#' Snaps data points to the stream segment within the
#' sub-catchment the data point is located.
#'
#'
#' @param data Data.frame with lat/lon columns in WGS84.
#' @param lon Column name with longitude coordinates as character string.
#' @param lat Column name with latitude coordinates as character string.
#' @param site_id Column name with unique site ids as character string
#' @param basin_id Column name with basin ids as character string.
#' @param subc_id Column name with sub-catchment ids as character string.
#' @param basin_path Full path of the basin .tif file.
#' @param subc_path Full path of hte sub-catchment .tif file.
#' @param stream_path Full path of stream network .gpkg file.
#' @param cores Number of cores used for palatalization. By default only 1 core
#' is used.
#' @param quiet TRUE or FALSE;
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'

snap_to_subc_segment <- function(data, lon, lat, site_id, basin_id, subc_id,
                                 basin_path, subc_path, stream_path, cores = 1,
                                 quiet = TRUE) {
  # Check operating system
  system <- get_os()

  # Check if any of the arguments is missing
  defined <- ls()
  passed <- names(as.list(match.call())[-1])

  if (any(!defined %in% passed)) {
    stop(paste0(paste(setdiff(defined, passed), collapse=", "), " is missing."))
  }

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if lon, lat, side_id, basin_id, and subc_id column names
  # are character strings
  for(name in  c(lon, lat, site_id, basin_id, subc_id)){
  if (!is.character(name))
    stop(paste0("Column name ", name, " is not a character string."))
  }

  # Check if lon, lat, side_id, basin_id, and subc_id column names exist
  for(name in c(lon, lat, site_id, basin_id, subc_id)){
    if (is.null(data[[name]]))
      stop(paste0("Column name '", name,"' does not exist."))
  }

   # Check if paths exists
  for(path in c(basin_path, subc_path, stream_path)){
   if (!file.exists(path))
     stop(paste0("File path: ", path, " does not exist."))
  }

  # Check if basin_path and subc_path ends with .tif
  for(path in c(basin_path, subc_path)){
    if (!endsWith(path, ".tif"))
      stop(paste0("File path: ", path, " does not end with .tif"))
  }

  # Check if basin_path and subc_path ends with .gpkg
     if (!endsWith(stream_path, ".gpkg"))
      stop(paste0("File path: ", stream_path, " does not end with .gpkg"))
  # Check if value of cores numeric
   if(!is.integer(cores))
    stop("cores: Value has to be integer.")


  # Check if quiet is logical
  if(!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Make bash scripts executable
  make_sh_exec()



}
