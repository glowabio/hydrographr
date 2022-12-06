#' Snap data points to the next stream segment
#' Snaps data/sampling points to the stream segment within the
#' sub-catchment the data point is located
#'
#'
#' @param data Data.frame with lat lon columns in WGS84
#' @param lon Column name with longitude coordinates as character string
#' @param lat Column name with latitude coordinates as character string
#' @param site_id Column name with unique site ids as character string
#' @param basin_id Column name with basin ids as character string
#' @param subc_id Column name with sub-catchment ids as character string
#' @param basin_path Full path of basin .tif file
#' @param subc_path Full path of sub-catchment .tif file
#' @param stream_path Full path of stream network .gpkg file
#' @param cores Number of cores used for palatalization
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'

snap_to_network <- function(data, site_id, lon, lat, ...
                            quiet = TRUE) {
