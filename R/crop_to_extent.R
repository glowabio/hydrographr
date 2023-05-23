#' @title Crop raster to extent
#'
#' @description This function crops an input raster layer (.tif) given a
#' bounding box (xmin, ymin, xmax, ymax coordinates, or a spatial object from
#' which to extract a bounding box) or to the boundary of a polygon vector
#' layer (cutline source). The cropping is performed directly on disk, i.e.
#' the input layer does not need to be loaded into R. The output is always
#' written to disk, and can be optionally loaded into R as a SpatRaster
#' (terra package) object (using read = TRUE).
#'
#' @param raster_layer character. Full path to the input raster .tif layer.
#' @param vector_layer character. Full path to a vector layer that is used as a
#' cutline data source (similar to a mask operation).
#' @param bounding_box numeric vector of the coordinates of the corners of a
#' bounding box (xmin, ymin, xmax, ymax), SpatRaster, SpatVector,
#' or other spatial object.
#' @param out_dir character. The directory where the output will be stored.
#' @param file_name character. Name of the cropped output raster .tif file.
#' @param compression character. Compression of the written output file.
#' Compression levels can be defined as "none", "low", or "high". Default is
#' "low".
#' @param bigtiff logical. Define whether the output file is expected to be a
#' BIGTIFF (file size larger than 4 GB). If FALSE and size > 4GB no file will be
#' written. Default is TRUE.
#' @param read logical. If TRUE, the cropped raster .tif layer gets read into R.
#' If FALSE, the layer is only stored on disk. Default is TRUE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra ext
#' @export
#'
#' @return The function returns always a .tif raster file written to disk.
#' Optionally, a SpatRaster (terra object) can be loaded into R with
#' read = TRUE.
#'
#' @author Yusdiel Torres-Cambas
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Define full path to the input raster .tif layer ans vector layer
#' spi_raster <- paste0(my_directory, "/hydrography90m_test_data",
#'                      "/spi_1264942.tif")
#' basin_vector <- paste0(my_directory, "/hydrography90m_test_data",
#'                       "/basin_59.gpkg")
#'
#' # Crop the Stream Power Index to the basin
#' spi_basin <- crop_to_extent(raster_layer = spi_raster,
#'                             vector_layer = basin_vector,
#'                             out_dir = my_directory,
#'                             file_name = "spi_basin_cropped.tif",
#'                             read = TRUE)

crop_to_extent <- function(raster_layer, vector_layer = NULL,
                           bounding_box = NULL, out_dir, file_name,
                           compression = "low", bigtiff = TRUE,
                           read = TRUE, quiet = TRUE) {

  # Check that an input path and an output path were provided
  if (missing(raster_layer))
    stop("Please provide an input path")
  if (missing(out_dir))
    stop("Please provide an output directory")
  if (missing(file_name))
    stop("Please provide a name for the output file")
  # Check that at least a cutline source or a bounding box coordinates are
  # provided
  if (is.null(vector_layer) && is.null(bounding_box))
    stop("Please provide at least a cutline source, a bounding box
          coordinates or an spatial object from which extract an extent")

  # Check if file path exists
  if (!file.exists(raster_layer))
    stop(paste0("File path: ", raster_layer, " does not exist."))

  if (!is.null(vector_layer))
  if (!file.exists(vector_layer))
    stop(paste0("File path: ", vector_layer, " does not exist."))

  # Check if path exists
  if (!dir.exists(out_dir))
    stop(paste0("Path: ", out_dir, " does not exist."))

  # Check if raster_layer ends with .tif
  if (!endsWith(raster_layer, ".tif"))
    stop("raster_layer: Input raster is not a .tif file.")


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

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Check if quiet is logical
  if (!is.logical(read))
    stop("read: Has to be TRUE or FALSE.")

  # Compose output_path by combining out_dir and file_name
  output_path <- paste0(out_dir, "/", file_name)
  # Check operating system
  sys_os <- get_os()
  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {
    if (!is.null(vector_layer)) {
        # Call external gdalwarp command from GDAL library. Cut through
        # the border line option
        cat("Cropping...\n")
        processx::run(system.file("sh", "crop_to_extent_cl.sh",
                        package = "hydrographr"),
            args = c(raster_layer, vector_layer, output_path,
                     compression_type, compression_level, bigtiff),
            echo = FALSE)
        } else if (!is.null(bounding_box)) {
      # Check if bounding_box is a vector with corner coordinate values,
      # if FALSE try to extract the values from a spatial object
        if (is.vector(bounding_box) && length(bounding_box) == 4) {
          xmin <- bounding_box[1]
          ymin <- bounding_box[2]
          xmax <- bounding_box[3]
          ymax <- bounding_box[4]
        } else {
          bb <- as.vector(terra::ext(bounding_box))
          xmin <- bb[1]
          ymin <- bb[3]
          xmax <- bb[2]
          ymax <- bb[4]
        }

        # Call external gdalwarp command from GDAL library.
        # Cut through a polygon extent
        cat("Cropping...\n")
        processx::run(system.file("sh", "crop_to_extent_bb.sh",
                        package = "hydrographr"),
            args = c(raster_layer, xmin, ymin,
                     xmax, ymax, output_path,
                     compression_type, compression_level, bigtiff),
            echo = !quiet)
        }
    } else {
      # Check if WSL and Ubuntu are installed
      check_wsl()
      # Change paths for WSL
      wsl_raster_layer <- fix_path(raster_layer)
      wsl_vector_layer <- fix_path(vector_layer)
      wsl_output_path <- fix_path(output_path)
      wsl_sh_cl_file <- fix_path(
        system.file("sh", "crop_to_extent_cl.sh",
                    package = "hydrographr"))
      wsl_sh_bb_file <- fix_path(
        system.file("sh", "crop_to_extent_bb.sh",
                    package = "hydrographr"))
      if (!is.null(vector_layer)) {
        # Call external gdalwarp command from GDAL library
        cat("Cropping...\n")
        processx::run(system.file("bat", "crop_to_extent_cl.bat",
                        package = "hydrographr"),
            args = c(wsl_raster_layer, wsl_vector_layer, wsl_output_path,
                     compression_type, compression_level, bigtiff,
                     wsl_sh_cl_file),
            echo = !quiet)
      } else if (!is.null(bounding_box)) {
        # Check if bounding_box is a vector with corner coordinate values,
        # if FALSE try to extract the values from an spatial object
        if (is.vector(bounding_box) && length(bounding_box) == 4) {
          xmin <- bounding_box[1]
          ymin <- bounding_box[2]
          xmax <- bounding_box[3]
          ymax <- bounding_box[4]
        } else {
          bb <- as.vector(terra::ext(bounding_box))
          xmin <- bb[1]
          ymin <- bb[3]
          xmax <- bb[2]
          ymax <- bb[4]
        }
        # Call external gdalwarp command from GDAL library.
        # Cut through a polygon extent
        cat("Cropping...\n")
        processx::run(system.file("bat", "crop_to_extent_bb.bat",
                        package = "hydrographr"),
            args = c(wsl_raster_layer, xmin, ymin,
                     xmax, ymax, wsl_output_path,
                     compression_type, compression_level, bigtiff,
                     wsl_sh_bb_file),
            echo = !quiet)
      }
    }

  if (file.exists(output_path)) {
    # Print message
    cat("Cropped raster saved under: ", output_path)
  } else {
    stop("Output file was not written. File size may have been larger than 4GB",
         "\nSet bigtiff = TRUE, for writing large output files.")
  }

  if (read == TRUE) {
    # Read cropped .tif layer
    crop_rast <- rast(output_path)
    return(crop_rast)
  }
}

