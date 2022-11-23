#' Merge raster or spatial vector tiles
#'
#' @param tile_path Path to raster tile or spatial vector
#' @importFrom processx run
#' @export
#'

merge_tiles <- function(tile_path) {
	 if (missing(tile_path) || is.na(tile_path)) {
   tile_path <- "NOT_ASSIGNED" 
   } else {
 	merge_tiles <- run(system.file("sh", "merge_tiles.sh",
                           package = "hydrographr"),
                   args = c(tile_path),
                   echo = FALSE)$stdout }
 	 return(merge_tiles)
 }