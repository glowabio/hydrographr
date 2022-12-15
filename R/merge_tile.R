#' @title  Merge multiple raster or spatial vector objects

#' @description Merge multiple raster or spatial vector objects from disk
#' to form a new raster or spatial vector object with a larger spatial extent.
#' At least a raster .tif or spatial vector geopackage file must be provided.
#' If rraster`_`read = TRUE (default), the outputs are a .tif
#' (saved under output`_`path) and a SpatRaster (terra package) object,
#' otherwise if rraster`_`read = FALSE, the .tif file is the only output.
#' If rvector`_`read = TRUE, the outputs are a .gpkg (saved under output`_`path)
#' and a SpatVector (terra package) object,
#' otherwise if rvector`_`read = FALSE (default),
#' the .tif file is the only output

#' @param tile_path Path to raster or spatial vector files
#' @param output_path Path to write the merged output file
#' @param rraster_read If TRUE (default), the outputs are a .tif file
#' (saved under output`_`path) and a SpatRaster (terra package) object.
#' If rraster`_`read = FALSE, only the .tif file will be generated. If TRUE,
#' then the merged raster layer gets also read into R as a SpatRaster
#' object.
#' @param rvector_read If TRUE (default is FALSE), the outputs are a .gpkg
#' (saved under output`_`path) and a SpatVector (terra object), and the
#' merged vector object gets read into R as a SpatVect object.
#' If TRUE, then rraster_read needs to be set to FALSE.

#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra vect
#' @export
#'
#' @return A .tif raster file or spatial vector object that is always written
#' to disk, and optionally loaded into R.
#'
#' @examples
#'
#'
#'
#' @author
#'

merge_tiles <- function(tile_path, output_path,
rraster_read = TRUE, rvector_read = FALSE) {

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
                    echo = FALSE)

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
        echo = FALSE)
       }
    }

  if (rraster_read == TRUE) {
      # Print message
      cat("Merged file saved under: ", output_path)
      # Read merged .tif layer
      merged_tiles <- rast(paste0(output_path, "/", "basin.tif"))

      return(merged_tiles)
    } else if (rvector_read == TRUE) {

      # Print message
      cat("Merged file saved under: ", output_path)
      # Read merged vector layer
      merged_tiles <- vect(paste0(output_path, "/", "basin_dissolved.gpkg"))

      return(merged_tiles)

    } else {
      # Print message
      cat("Merged file saved under: ", output_path)
    }
 }
