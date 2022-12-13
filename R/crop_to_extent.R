#' @title crop_to_extent
#'
#' @description Crop a raster .tif to a polygon border line if a vector layer
#' (cutline source) is provided, otherwise if a bounding box is provided
#' (xmin, ymin, xmax, ymax coordinates or a spatial object from which to extract
#'  a bounding box), the raster is cropped to the extent of the bounding box. At
#'  least a cutline source (vector_path) or a bounding box (bound_box)
#'  must be provided. If rcrop_read = TRUE (default), the outputs are a .tif
#'  (saved under output_path) and a SpatRaster (terra package) object,
#'  otherwise if rcrop_read = FALSE, the .tif file is the only output.
#'
#' @param raster_path Path to the raster .tif layer
#' @param vector_path Path to a vector layer that is used as a cutline data
#' source
#' @param bound_box The coordinates of the corner of a bounding box (xmin, ymin,
#' xmax, ymax) or a SpatRaster, SpatVector, or other spatial object.
#' @param output_path Path for the raster .tif output
#' @param rcrop_read If TRUE the cropped raster .tif layer gets read into R.
#' TRUE is set by default.
#'
#' @importFrom processx run
#' @importFrom terra rast
#' @importFrom terra ext
#' @export
#'
#' @return A .tif raster file and a SpatRaster object, or a .tif raster file if
#' rcrop_read = FALSE
#'

crop_to_extent <- function(raster_path, vector_path = NULL, bound_box = NULL,
                           output_path,
                           rcrop_read=TRUE) {
  # Check that an input path and an output path were provided
  if (missing(raster_path)) stop("Please provide an input path")
  if (missing(output_path)) stop("Please provide an output path")
  # Check that at least a cutline source or a bounding box coordinates are
  # provided
  if (is.null(vector_path) & is.null(bound_box)){
    stop("Please provide at least a cutline source, a bounding box
          coordinates or an spatial object from which extract an extent")
  } else {
    # Check operating system
    system <- get_os()
    if (system == "linux") {
      if (!is.null(vector_path)){
        # Call external gdalwarp command from GDAL library. Cut through
        # the border line option
        cat("Cropping...\n")
        processx::run(system.file("sh", "crop_to_extent_cl.sh",
                        package = "hydrographr"),
            args = c(raster_path, vector_path, output_path),
            echo = FALSE)
      } else if (!is.null(bound_box)) {
      # Check if bound_box is a vector with corner coordinate values, if FALSE
      # try to extract the values from a spatial object
        if (is.vector(bound_box) & length(bound_box) == 4) {
          xmin <- bound_box[1]
          ymin <- bound_box[2]
          xmax <- bound_box[3]
          ymax <- bound_box[4]
        } else {
          bb <- as.vector(terra::ext(bound_box))
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
            args = c(raster_path, xmin, ymin,
                     xmax, ymax, output_path),
            echo = FALSE)
      }
    } else if (system == "windows") {
      # Check if WSL and Ubuntu are installed
      check_wsl()
      # Change paths for WSL
      wsl_raster_path <- fix_path(raster_path)
      wsl_vector_path <- fix_path(vector_path)
      wsl_output_path <- fix_path(output_path)
      wsl_sh_cl_file <- fix_path(
        system.file("sh", "crop_to_extent_cl.sh",
                    package = "hydrographr"))
      wsl_sh_bb_file <- fix_path(
        system.file("sh", "crop_to_extent_bb.sh",
                    package = "hydrographr"))
      if (!is.null(vector_path)){
        # Call external gdalwarp command from GDAL library
        cat("Cropping...\n")
        processx::run(system.file("bat", "crop_to_extent_cl.bat",
                        package = "hydrographr"),
            args = c(wsl_raster_path, wsl_vector_path, wsl_output_path,
                     wsl_sh_cl_file),
            echo = FALSE)
      } else if (!is.null(bound_box)) {
        # Check if bound_box is a vector with corner coordinate values, if FALSE
        # try to extract the values from an spatial object
        if (is.vector(bound_box) & length(bound_box) == 4) {
          xmin <- bound_box[1]
          ymin <- bound_box[2]
          xmax <- bound_box[3]
          ymax <- bound_box[4]
        } else {
          bb <- as.vector(terra::ext(bound_box))
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
            args = c(wsl_raster_path, xmin, ymin,
                     xmax, ymax, wsl_output_path, wsl_sh_bb_file),
            echo = FALSE)
      }
    }
    if (rcrop_read == TRUE) {
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
