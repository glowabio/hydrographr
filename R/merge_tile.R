#' @title  Merge multiple raster or spatial vector objects

#' @description Merge multiple raster or spatial vector objects from disk
#' to form a new raster or spatial vector object with a larger spatial extent.
#' A directory with at least a raster .tif or spatial vector geopackage file
#' must be provided.
#' If read`_`raster = TRUE (default), the outputs are a .tif
#' (saved under out`_`dir) and a SpatRaster (terra package) object,
#' otherwise if read`_`raster = FALSE, the .tif file is the only output.
#' If read`_`vector = TRUE, the outputs are a .gpkg (saved under out`_`dir)
#' and a SpatVector (terra package) object,
#' otherwise if read`_`vector = FALSE (default),
#' the .gpkg file is the only output

#' @param tile_dir character. The directory containing the raster tiles or
#' spatial vectors to be merged
#' @param out_dir character. The directory where the output will be stored
#' @param read_raster logical. If TRUE, the merged raster .tif layer gets read
#' into R. If FALSE, the layer is only stored on disk. Default is TRUE
#' @param read_vector logical. If TRUE, the merged spatial vector gets read
#' into R. In this case, read_raster needs to be set to FALSE.
#' If FALSE, the vector is only stored on disk. Default is FALSE
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
#' @author Thomas Tomiczek
#'

merge_tiles <- function(tile_dir, out_dir,
read_raster = TRUE, read_vector = FALSE) {

  # Make bash scripts executable
  make_sh_exec()

  if (missing(tile_dir) || is.na(tile_dir)) {
    tile_dir <- "NOT_ASSIGNED"
    }
  if (missing(out_dir) || is.na(out_dir)) {
    out_dir <- "NOT_ASSIGNED"
    } else {
      # Check operating system
      system <- get_os()
      if (system == "linux") {
        merge_tiles <- processx::run(system.file("sh", "merge_tiles.sh",
                            package = "hydrographr"),
                    args = c(tile_dir, out_dir),
                    echo = FALSE)

       } else if (system == "windows") {
       # Check if WSL and Ubuntu are installed
       check_wsl()
       # Change path for WSL
       wsl_tile_dir <- fix_path(tile_dir)
       wsl_out_dir <- fix_path(out_dir)
       wsl_sh_file <- fix_path(
         system.file("sh", "merge_tiles.sh",
                    package = "hydrographr"))

       processx::run(system.file("bat", "merge_tiles.bat",
                    package = "hydrographr"),
        args = c(wsl_tile_dir, wsl_out_dir, wsl_sh_file),
        echo = FALSE)
       }
    }

  if (read_raster == TRUE) {
      # Print message
      cat("Merged file saved under: ", out_dir)
      # Read merged .tif layer
      merged_tiles <- rast(paste0(out_dir, "/", "basin.tif"))

      return(merged_tiles)
    } else if (read_vector == TRUE) {

      # Print message
      cat("Merged file saved under: ", out_dir)
      # Read merged vector layer
      merged_tiles <- vect(paste0(out_dir, "/", "basin_dissolved.gpkg"))

      return(merged_tiles)

    } else {
      # Print message
      cat("Merged file saved under: ", out_dir)
    }
 }
