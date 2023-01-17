#' @title  Merge raster or vector objects
#'
#' @description Merge multiple raster or spatial vector objects from disk
#' to form a new raster or spatial vector object with a larger spatial extent.
#' A directory with at least a raster .tif or spatial vector geopackage file
#' must be provided.
#' If read_raster = TRUE (default), the outputs are a .tif
#' (saved under out_dir) and a SpatRaster (terra package) object, otherwise if
#' read_raster = FALSE, the .tif file is the only output. If read_vector = TRUE,
#' the outputs are a .gpkg (saved under out_dir) and a SpatVector
#' (terra package) object, otherwise if read_vector = FALSE (default), the .gpkg
#' file is the only output.

#' @param tile_dir character. The directory containing only the raster or
#' spatial vectors tiles, which should be merged.
#' @param out_dir character. The directory where the output will be stored.
#' @param read_raster logical. If TRUE, the merged raster .tif layer gets read
#' into R. If FALSE, the layer is only stored on disk. Default is TRUE.
#' @param read_vector logical. If TRUE, the merged spatial vector gets read
#' into R. In this case, read_raster needs to be set to FALSE.
#' If FALSE, the vector is only stored on disk. Default is FALSE.
#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra vect
#' @export
#'
#' @references
#' \url{https://gdal.org/programs/gdalbuildvrt.html}
#'
#' \url{https://gdal.org/programs/gdal_translate.html}
#'
#' \url{https://gdal.org/programs/ogrmerge.html#ogrmerge}
#'
#' \url{https://gdal.org/programs/ogr2ogr.html}
#'
#' @author Thomas Tomiczek
#'
#' @return A .tif raster file or spatial vector object that is always written
#' to disk, and optionally loaded into R.
#'
#' @examples
#' # Download tiles into temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_tiles(variable = "basin",
#'                file_format = "tif",
#'                tile_id = c("h22v08","h22v10"),
#'                download_dir = my_directory)
#'
#' # Define folder containing only the tiles, which should me merged
#' tiles_folder <- paste0(my_directory, "/r.watershed/basin_tiles20d")
#' # Define output folder
#' output_folder <- paste0(my_directory, "/merged_tiles")
#' # Creat ouput folder if it doesn't exist
#' if(!dir.exists(output_folder)) dir.create(output_folder)
#'
#' # Merge tiles
#' merged_tiles <- merge_tiles(tile_dir = tiles_folder,
#'                             out_dir = output_folder)
#'

merge_tiles <- function(tile_dir, out_dir,
                        read_raster = TRUE, read_vector = FALSE) {
  # Check if paths exists
  if (!dir.exists(tile_dir))
    stop(paste0("Path: ", tile_dir, " does not exist."))

  if (!dir.exists(out_dir))
    stop(paste0("Path: ", out_dir, " does not exist."))

  if (read_raster == TRUE && read_vector == TRUE)
    stop("One of read_raster and read_vector has to be FALSE")

      # Check operating system
      system <- get_os()

      # Make bash scripts executable
      make_sh_exec()

      if (system == "linux" || system == "osx") {
        merge_tiles <- processx::run(system.file("sh", "merge_tiles.sh",
                            package = "hydrographr"),
                    args = c(tile_dir, out_dir),
                    echo = FALSE)

       } else {
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
        echo = TRUE)
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
