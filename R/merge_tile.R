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

#' @param tile_dir character. The directory containing the raster or
#' spatial vectors tiles, which should be merged.
#' @param tile_names character. The names of the files to be merged,
#' including the file extension (.tif or .gpkg).
#' @param out_dir character. The directory where the output will be stored.
#' @param file_name character. Name of the merged output file, including the
#' file extension (.tif or .gpkg).
#' @param name character. The attribute table column name of the stream segment
#' ("stream"), sub-catchment ("ID"), basin ("ID") or outlet ("ID") column which
#' is used for merging GeoPackages. Default is "stream".
#' @param compression character. Compression of the written output file.
#' Compression levels can be defined as "none", "low", or "high". Default is
#' "low".
#' @param bigtiff logical. Define whether the output file is expected to be a
#' BIGTIFF (file size larger than 4 GB). If FALSE and size > 4GB no file will be
#' written. Default is TRUE.
#' @param read logical. If TRUE, the merged layer gets read
#' into R. If FALSE, the layer is only stored on disk. Default is FALSE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom processx run
#' @importFrom terra rast vect
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
#'                             tile_names = c("basin_h22v08.tif", "basin_h22v10.tif"),
#'                             out_dir = output_folder,
#'                             file_name = "basin_merged.tif",
#'                             read = TRUE)
#'

merge_tiles <- function(tile_dir, tile_names, out_dir, file_name, name = "stream",
                        compression = "low", bigtiff = TRUE,
                        read = FALSE, quiet = TRUE) {
  # Check if paths exist
  if (!dir.exists(tile_dir))
    stop(paste0("Path: ", tile_dir, " does not exist."))

  if (!dir.exists(out_dir))
    stop(paste0("Path: ", out_dir, " does not exist."))

  # Check if tile_names exist
  for (iname in tile_names){
    file <- paste(tile_dir, iname, sep ="/")
    if (!file.exists(file))
      stop(paste0("File: ", file, " does not exist."))
  }

  # Check and translate compression into the compression type and the
  # compression level which is applied to the tiff file when writing it.
  if(compression == "none") {
    compression_type  <- "NONE"
    compression_level <- 0
  } else if (compression == "low") {
    compression_type  <- "DEFLATE"
    compression_level <- 2
  } else if (compression == "high") {
    compression_type  <- "DEFLATE"
    compression_level <- 9
  } else {
    stop("'compression' must be one of 'none', 'low', or 'high'.")
  }

  # Define whether BIGTIFF is used or not. BIGTIFF is required for
  # tiff output files > 4 GB.
  if(bigtiff) {
    bigtiff <- "YES"
  } else {
    bigtiff <- "NO"
  }

  # Check operating system
  sys_os <- hydrographr:::get_os()

  # Make bash scripts executable
  make_sh_exec()

  # Format tile_names vector so that it can be read
  # as an array in the bash script
  tile_names_array <- paste(unique(tile_names), collapse = "/")


  if (sys_os == "linux" || sys_os == "osx") {
    processx::run(system.file("sh", "merge_tiles.sh",
                              package = "hydrographr"),
                  args = c(tile_dir, tile_names_array, out_dir, file_name, name,
                           compression_type, compression_level, bigtiff),
                  echo = !quiet)

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
    args = c(wsl_tile_dir, tile_names_array, wsl_out_dir, file_name, name,
             compression_type, compression_level, bigtiff,
             wsl_sh_file),
    echo = !quiet)
  }

  if (file.exists(paste0(out_dir, "/", file_name))) {
    # Print message
    cat("Merged file saved under: ", out_dir,"\n")
  } else {
    stop("Output file was not written. File size may have been larger than 4GB",
         "\nSet bigtiff = TRUE, for writing large output files.")
  }

  if (read == TRUE) {
    # Identify if merged file is .tif or .gpkg
    file_extension <- file_ext(paste0(out_dir, "/", file_name))
    # Print message
    cat("Loading merged file\n")

    # Read merged layer
    if(file_extension == "tif") {
      merged_tiles <- rast(paste0(out_dir, "/", file_name))

    } else if(file_extension == "gpkg") {
      merged_tiles <- vect(paste0(out_dir, "/", file_name))

    }

    return(merged_tiles)

  }


 }
