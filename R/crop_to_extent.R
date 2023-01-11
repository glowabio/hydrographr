#' @title Crop a raster layer to a specific extent on disk
#'
#' @description This function crops an input raster layer directly on disk,
#' i.e. the input layer does not be needed to be loaded into R. The function
#' crops a raster .tif to a polygon border line if a vector layer
#' (cutline source) is provided, otherwise if a bounding box is provided
#' (xmin, ymin, xmax, ymax coordinates or a spatial object from which to extract
#' a bounding box), the raster is cropped to the extent of the bounding box. At
#' least a cutline source (vector_layer) or a bounding box (bounding_box)
#' must be provided. The output is always written to disk, and can be
#' optionally loaded directly into R as a SpatRaster (terra package) object
#' (using read = TRUE).
#'
#' @param raster_layer character. Full path to the input raster .tif layer
#' @param vector_layer character. Full path to a vector layer that is used as a
#' cutline data source (similar to a mask operation)
#' @param bounding_box numeric vector of the coordinates of the corners of a
#' bounding box (xmin, ymin, xmax, ymax), SpatRaster, SpatVector,
#' or other spatial object.
#' @param out_dir character. The directory where the output will be stored
#' @param file_name character. Name of the cropped output raster .tif file
#' @param read logical. If TRUE, the cropped raster .tif layer gets read into R.
#' If FALSE, the layer is only stored on disk. Default is TRUE
#'
#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra ext
#' @export
#'
#' @return The function returns always a .tif raster file written to disk.
#' Optionally, a SpatRaster (terra object) can be loaded into R with
#' read = TRUE.
#'
#' @author
#'
#' @examples
#' library(hydrographr)
#' library(data.table)
#'
#' # Specify the working directory of the test data
#' DATADIR <- "path/to/test_data"
#'
#' # Download the test data
#' download_test_data(DATADIR)
#'
#' # Crop the Stream Power Index to the basin
#' spi_basin <- crop_to_extent(raster_layer = paste0(DATADIR,
#' "/spi_1264942.tif"),
#' vector_layer = paste0(DATADIR, "/basin_59.gpkg"),
#' output_path = "/my/output/path/spi_basin_cropped.tif"),
#' read = TRUE)

crop_to_extent <- function(raster_layer,
                           vector_layer = NULL,
                           bounding_box = NULL,
                           out_dir, file_name,
                           read = TRUE) {

  # Check that an input path and an output path were provided
  if (missing(raster_layer))
    stop("Please provide an input path")
  if (missing(out_dir))
    stop("Please provide an output directory")
  if (missing(file_name))
    stop("Please provide a name for the output file")
  # Check that at least a cutline source or a bounding box coordinates are
  # provided
  if (is.null(vector_layer) && is.null(bounding_box)) {
    stop("Please provide at least a cutline source, a bounding box
          coordinates or an spatial object from which extract an extent")
  } else {
    # Compose output_path by combining out_dir and file_name
    output_path <- paste0(out_dir, "/", file_name)
    # Check operating system
    system <- get_os()
    if (system == "linux" || system == "osx") {
      if (!is.null(vector_layer)) {
        # Call external gdalwarp command from GDAL library. Cut through
        # the border line option
        cat("Cropping...\n")
        run(system.file("sh", "crop_to_extent_cl.sh",
                        package = "hydrographr"),
            args = c(raster_layer, vector_layer, output_path),
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
                     xmax, ymax, output_path),
            echo = FALSE)
      }
    } else if (system == "windows") {
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
                     wsl_sh_cl_file),
            echo = FALSE)
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
                     xmax, ymax, wsl_output_path, wsl_sh_bb_file),
            echo = FALSE)
      }
    }
    if (read == TRUE) {
      # Read cropped .tif layer
      cat("Cropped raster saved under: ", output_path)
      crop_rast <- rast(output_path)
      return(crop_rast)
    } else {
      # Print message
      cat("Cropped raster saved under: ", output_path)
    }
  }
}
