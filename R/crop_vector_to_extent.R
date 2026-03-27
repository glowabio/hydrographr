#' @title Crop vector to extent
#'
#' @description This function crops an input vector layer (.shp, .gpkg,
#' .geojson) given a bounding box (xmin, ymin, xmax, ymax coordinates, or a
#' spatial object from which to extract a bounding box) or to the boundary of
#' a polygon vector layer (cutline source). The cropping is performed directly
#' on disk using ogr2ogr, i.e. the input layer does not need to be loaded
#' into R. The output is always written to disk, and can be optionally loaded
#' into R as an sf object (using read = TRUE).
#'
#' @param vector_layer character. Full path to the input vector layer
#' (.shp, .gpkg, .geojson).
#' @param clip_layer character. Full path to a polygon vector layer used as
#' the clipping boundary (similar to a mask operation). Optional.
#' @param bounding_box numeric vector of the coordinates of the corners of a
#' bounding box (xmin, ymin, xmax, ymax), SpatRaster, SpatVector,
#' or other spatial object.
#' @param out_dir character. The directory where the output will be stored.
#' @param file_name character. Name of the cropped output vector file.
#' Supported formats: .gpkg, .shp, .geojson.
#' @param read logical. If TRUE, the cropped vector layer gets read into R
#' as an sf object. If FALSE, the layer is only stored on disk.
#' Default is TRUE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @importFrom processx run
#' @importFrom sf st_read
#' @importFrom terra ext
#' @export
#'
#' @return The function always writes a vector file to disk.
#' Optionally, an sf object can be loaded into R with read = TRUE.
#'
#' @note Shapefile output (.shp) truncates column names to 10 characters
#' due to format limitations. Use .gpkg or .geojson to preserve full
#' column names.
#'
#' @author Afroditi Grigoropoulou
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Define full path to the input vector layer
#' basin_vector <- paste0(my_directory, "/hydrography90m_test_data",
#'                        "/basin_59.gpkg")
#'
#' # Crop a vector layer to a bounding box
#' cropped_sf <- crop_vector_to_extent(
#'   vector_layer = basin_vector,
#'   bounding_box = c(8.5, 42.24, 8.8, 42.3),
#'   out_dir = my_directory,
#'   file_name = "basin_cropped.gpkg",
#'   read = TRUE
#' )

crop_vector_to_extent <- function(vector_layer, clip_layer = NULL,
                                  bounding_box = NULL,
                                  out_dir, file_name,
                                  read = TRUE, quiet = TRUE) {

  # Check that an input path and an output path were provided
  if (missing(vector_layer))
    stop("Please provide an input path.")
  if (missing(out_dir))
    stop("Please provide an output directory.")
  if (missing(file_name))
    stop("Please provide a name for the output file.")

  # Check that at least a clip source or a bounding box is provided
  if (is.null(clip_layer) && is.null(bounding_box))
    stop("Please provide at least a clip layer or bounding box
          coordinates or a spatial object from which to extract an extent.")

  # Check if input file exists
  if (!file.exists(vector_layer))
    stop(paste0("File path: ", vector_layer, " does not exist."))

  if (!is.null(clip_layer))
    if (!file.exists(clip_layer))
      stop(paste0("File path: ", clip_layer, " does not exist."))

  # Check if output directory exists
  if (!dir.exists(out_dir))
    stop(paste0("Path: ", out_dir, " does not exist."))

  # Check if input is a supported vector format
  supported_in <- c(".shp", ".gpkg", ".geojson")
  if (!any(sapply(supported_in, function(ext) endsWith(vector_layer, ext))))
    stop("Input must be a .shp, .gpkg, or .geojson file.")

  # Detect output format from file extension
  if (endsWith(file_name, ".gpkg")) {
    output_format <- "GPKG"
  } else if (endsWith(file_name, ".shp")) {
    output_format <- "ESRI Shapefile"
  } else if (endsWith(file_name, ".geojson")) {
    output_format <- "GeoJSON"
  } else {
    stop("Output file must be a .gpkg, .shp, or .geojson file.")
  }

  if (output_format == "ESRI Shapefile") {
    warning("Shapefile format truncates column names to 10 characters. ",
            "Use .gpkg or .geojson to preserve full column names.")
  }

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Check if read is logical
  if (!is.logical(read))
    stop("read: Has to be TRUE or FALSE.")

  # Compose output_path by combining out_dir and file_name
  output_path <- paste0(out_dir, "/", file_name)

  # Check operating system
  sys_os <- get_os()
  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    if (!is.null(clip_layer)) {

      # GeoJSON does not support -overwrite; delete existing file first
      if (file.exists(output_path)) file.remove(output_path)

      # Crop using a polygon clip layer
      cat("\nCropping...\n")
      processx::run(system.file("sh", "crop_vector_to_extent_cl.sh",
                                package = "hydrographr"),
                    args = c(vector_layer, clip_layer, output_path, output_format),
                    echo = !quiet)

    } else if (!is.null(bounding_box)) {
      # Extract bounding box coordinates
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

      # GeoJSON does not support -overwrite; delete existing file first
      if (file.exists(output_path)) file.remove(output_path)

      # Crop using bounding box
      cat("\nCropping...\n")
      processx::run(system.file("sh", "crop_vector_to_extent_bb.sh",
                                package = "hydrographr"),
                    args = c(vector_layer, xmin, ymin, xmax, ymax,
                             output_path, output_format),
                    echo = !quiet)
    }

  } else {
    # Check if WSL and Ubuntu are installed
    check_wsl()
    # Change paths for WSL
    wsl_vector_layer <- fix_path(vector_layer)
    wsl_output_path <- fix_path(output_path)
    wsl_sh_cl_file <- fix_path(
      system.file("sh", "crop_vector_to_extent_cl.sh",
                  package = "hydrographr"))
    wsl_sh_bb_file <- fix_path(
      system.file("sh", "crop_vector_to_extent_bb.sh",
                  package = "hydrographr"))

    if (!is.null(clip_layer)) {
      wsl_clip_layer <- fix_path(clip_layer)


      # Crop using a polygon clip layer
      cat("\nCropping...\n")
      processx::run(system.file("bat", "crop_vector_to_extent_cl.bat",
                                package = "hydrographr"),
                    args = c(wsl_vector_layer, wsl_clip_layer, wsl_output_path,
                             output_format, wsl_sh_cl_file),
                    echo = !quiet)

    } else if (!is.null(bounding_box)) {
      # Extract bounding box coordinates
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

      # Crop using bounding box
      cat("\nCropping...\n")
      processx::run(system.file("bat", "crop_vector_to_extent_bb.bat",
                                package = "hydrographr"),
                    args = c(wsl_vector_layer, xmin, ymin, xmax, ymax,
                             wsl_output_path, output_format, wsl_sh_bb_file),
                    echo = !quiet)
    }
  }

  if (file.exists(output_path)) {
    if (!quiet) cat("Cropped vector saved under: ", output_path, "\n")
  } else {
    stop("Output file was not written.")
  }

  if (read == TRUE) {
    crop_vect <- sf::st_read(output_path, quiet = quiet)
    return(crop_vect)
  }
}
