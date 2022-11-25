#' Merge raster or spatial vector tiles
#'
#' @param tile_path Path to raster tile or spatial vector
#' @param output_path Path to write the output
#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra vect
#' @param rraster_read If TRUE merged raster .tif layer gets read into R.
#' TRUE is set by default.
#' @param rvector_read If TRUE merged spatial vector gets read into R.
#' FALSE is set by default.
#' @export
#'

merge_tiles <- function(tile_path, output_path, rraster_read = TRUE, rvector_read = FALSE) {
	if (missing(tile_path) || is.na(tile_path)) {
    tile_path <- "NOT_ASSIGNED" 
    }
  if (missing(output_path) || is.na(output_path)) {
    output_path <- "NOT_ASSIGNED" 
    } else {
      # Check operating system
      system <- get_os()
      if (system == "linux") {
 	    merge_tiles <- run(system.file("sh", "merge_tiles.sh",
                           package = "hydrographr"),
                   args = c(tile_path, output_path),
                   echo = TRUE)

       } else if (system == "windows") {
       # Check if WSL and Ubuntu are installed
       check_wsl()
       # Change path for WSL
       wsl_tile_path <- fix_path(tile_path)
       wsl_output_path <- fix_path(output_path)
       wsl_sh_file <- fix_path(
         system.file("sh", "merge_tiles.sh",
                    package = "hydrographr"))

       run(system.file("bat", "merge_tiles.bat",
                    package = "hydrographr"),
        args = c(wsl_tile_path, wsl_output_path, wsl_sh_file),
        echo = !quiet)
       }
    }

 	if (rraster_read == TRUE) {
      # Read merged .tif layer
      merge_tiles <- rast(paste0(output_path,"/","basin.tif"))

      return(merge_tiles)
    }
    
    if (rvector_read == FALSE) {
      # Read merged vector layer
      merge_tiles <- vect(paste0(output_path,"/","basin_dissolved.gpkg")) 

      return(merge_tiles)
    } else {
      # Print message
      print(paste0("Merge saved under: ", output_path))
    }
 }