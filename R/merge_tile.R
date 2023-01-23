#' @title  Merge raster or vector objects
#'
#' @description Merge multiple raster or spatial vector objects from disk
#' to form a new raster or spatial vector object with a larger spatial extent.
#' A directory with at least two raster .tif or spatial vector geopackage files
#' should be provided.
#' Depending on the input, the output is a .tif or a .gpkg file (saved under
#' out_dir). If read = TRUE, the output is read into R as a SpatRaster
#' (terra package) object in case of .tif files, or as a SpatVector
#' (terra package) object in case of .gpkg files.

#' @param tile_dir character. The directory containing only the raster or
#' spatial vectors tiles, which should be merged.
#' @param tile_names character. The names of the files to be merged,
#' including the file extension (.tif or .gpkg).
#' @param out_dir character. The directory where the output will be stored.
#' @param file_name character. Name of the merged output file, including the
#' file extension (.tif or .gpkg).
#' @param read logical. If TRUE, the merged layer gets read
#' into R. If FALSE, the layer is only stored on disk. Default is FALSE.
#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra vect
#' @importFrom tools file_ext
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
#' @author Thomas Tomiczek, Jaime Garcia Marquez, Afroditi Grigoropoulou
#'
#' @return A .tif raster file or .gpkg spatial vector object that is always
#' written to disk, and optionally loaded into R.
#'
#' @examples
#' # Download tiles into the temporary R folder
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
#' # Create output folder if it doesn't exist
#' if(!dir.exists(output_folder)) dir.create(output_folder)
#'
#'
#' # Merge tiles
#' merged_tiles <- merge_tiles(tile_dir = tiles_folder,
#'                             tile_names = c("h22v08", "h22v10"),
#'                             out_dir = output_folder,
#'                             file_name = "basin_merged.tif",
#'                             read = TRUE)
#'

merge_tiles <- function(tile_dir, tile_names, out_dir, file_name,
                        read = FALSE) {
  # Check if paths exist
  if (!dir.exists(tile_dir))
    stop(paste0("Path: ", tile_dir, " does not exist."))

  if (!dir.exists(out_dir))
    stop(paste0("Path: ", out_dir, " does not exist."))

  # Check if tile_names exist
  for (name in tile_names){
    file <- paste(tile_dir, name, sep ="/")
    if (!file.exists(file))
      stop(paste0("File: ", file, " does not exist."))
  }

      # Check operating system
      sys_os <- hydrographr:::get_os()

      # Make bash scripts executable
      make_sh_exec()

      # Format tile_names vector so that it can be read
      # as an array in the bash script
      tile_names_array <- paste(unique(tile_names), collapse = ",")


      if (sys_os == "linux" || sys_os == "osx") {
        merge_tiles <- processx::run(system.file("sh", "merge_tiles.sh",
                            package = "hydrographr"),
                    args = c(tile_dir, tile_names_array, out_dir, file_name),
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
        args = c(wsl_tile_dir, tile_names_array, wsl_out_dir, file_name,
                 wsl_sh_file),
        echo = TRUE)
       }

  # Print message
  cat("Merged file saved under: ", out_dir,"\n")

  if (read == TRUE) {

    # Print message
    cat("Loading merged file\n")

    # Identify if merged file is .tif or .gpkg
    file_extension <- file_ext(paste0(out_dir, "/", file_name))

    # Read merged layer
    if(file_extension == "tif") {
      merged_tiles <- rast(paste0(out_dir, "/", file_name))

    } else if(file_extension == "gpkg") {
      merged_tiles <- vect(paste0(out_dir, "/", file_name))

    }

    return(merged_tiles)

  }


 }
