#' @title Snap points to stream segment based on distance or flow accumulation
#'
#' @description Snap points to the next stream segment within a defined radius
#' (in map pixels) or a minimum flow accumulation.
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the longitude / latitude coordinates in WGS84.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param id  character. The name of a column containing unique IDs for
#' each row of "data" (e.g., occurrence or site IDs). The unique IDs need to be
#' numeric and less than 10 characters long.
#' @param stream_layer character. Full path of the stream network .tif file
#' @param accu_layer character. Full path of the flow accumulation .tif file.
#' Needed if the point should be snapped to the next stream segment having
#' an accumulation value higher than the flow accumulation threshold
#' (set by 'accumulation'). This prevents points from being snapped to small
#' stream tributaries. Optional. Default is NULL.
#' @param method character. One of "distance", "accumulation", or "both".
#' Defines if the points are snapped using the distance or flow accumulation
#' (see "Details" for more information). If method is set to "both" the output
#' will contain the new coordinates for both calculations.
#' Default is "distance" (in map pixels).
#' @param distance numeric. Maximum radius in map pixels. The points will be
#' snapped to the next stream within this radius. Default is 500.
#' @param accumulation numeric. Minimum flow accumulation. Points will be
#' snapped to the next stream with a flow accumulation equal or higher than the
#' given value. Default is 0.5.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr left_join
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#' @note
#' Duplicated rows will be removed from the input data.
#'
#' @details
#' The function makes use of the r.stream.snap function available in GRASS GIS to
#' simultaneously snap a number of points to the stream network. A distance threshold
#' can be specified and points will be snapped to any stream segment within this
#' distance radius (in map pixels). However, to avoid snapping to small tributaries, an
#' accumulation threshold can be used and the snapping occurs on stream segment
#' with equal or higher accumulation threshold and within the given distance
#' radius.
#'
#' @returns
#' Returns a data.frame with the snapped coordinates and the sub-catchment ID
#' of the snapped stream segment. If the sub-catchment ID is NA, no stream segment
#' was found within the given distance (method = "distance") or no stream segment
#' wad found within the given distance and a flow accumulation equal or higher
#' than the given threshold (method = "accumulation"). "out-bbox" means that the
#' provided coordinates are not within the extend (bounding box) of the
#' provided stream network layer.
#'
#'
#' @author Marlene Schürz, Jaime Garcia Marquez
#'
#' @references
#' \url{https://grass.osgeo.org/grass78/manuals/addons/r.stream.snap.html}
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load occurrence data
#' species_occurrence <- read.table(paste0(my_directory,
#'                             "/hydrography90m_test_data/spdata_1264942.txt"),
#'                               header = TRUE)
#'
#' # Define full path to stream network and flow accumulation
#' stream_raster <- paste0(my_directory,
#'                      "/hydrography90m_test_data/stream_1264942.tif")
#' flow_raster <- paste0(my_directory,
#'                      "/hydrography90m_test_data/flow_1264942.tif")
#'
#' # To calculate the new (snapped) coordinates for a radius and a flow
#  # accumulation threshold
#' snapped_coordinates <- snap_to_network(data = species_occurrence,
#'                                        lon = "longitude",
#'                                        lat = "latitude",
#'                                        id = "occurrence_id",
#'                                        stream_layer = stream_raster,
#'                                        accu_layer = flow_raster,
#'                                        method = "both",
#'                                        distance = 300,
#'                                        accumulation = 0.8)
#'
#' # Show head of output table
#' head(snapped_coordinates)
#'


snap_to_network <- function(data, lon, lat, id, stream_layer,
                            accu_layer = NULL, method = "distance", distance = 500,
                            accumulation = 0.5, quiet = TRUE) {
  # Check if data.frame is defined
  if (missing(data))
    stop("Input data is missing.")

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if column name is defined
  if (missing(lon))
    stop("Column name of longitudinal coordinate is not defined.")

  # Check if column name is defined
  if (missing(lat))
    stop("Column name of latitudinal coordinate is not defined.")

  # Check if lon/lat column names are character vectors
  if (!is.character(lon))
    stop("lon: Column name is not a character vector.")
  if (!is.character(lat))
    stop("lat: Column name is not a character vector.")

  # Check if lon/lat column names exist
  if (is.null(data[[lon]]))
    stop(paste0("Column name '", lon, "' does not exist."))
  if (is.null(data[[lat]]))
    stop(paste0("Column name '", lat, "' does not exist."))
  if (is.null(data[[id]]))
    stop(paste0("Column name '", id, "' does not exist."))

  # Check if values of the lon/lat columns are numeric
  if (!is.numeric(data[[lon]]))
    stop(paste0("Column ", lon, " has to be numeric."))
  if (!is.numeric(data[[lat]]))
    stop(paste0("Column ", lat, " has to be numeric."))

  # Check if id is less than 9 characters
  if(any(nchar(data[[id]]) > 9))
    stop("The id column has to be less than 10 characters long.")
  # Add here: if condition to check if lat/long columns are in WGS84

  # Check if stream_layer is defined
  if (missing(stream_layer))
    stop("stream_layer is missing.")
  # Check if accu_layer is defined
  # If method is set to "accumulation" or "both"
  if (method == "accumulation" || method == "both")
    if (is.null(accu_layer))
      stop(paste0("accu_layer is missing."))

  # Check if stream_layer exists
  if (!file.exists(stream_layer))
    stop(paste0("stream_layer: ", stream_layer, " does not exist."))
  # Check if accu_layer exists
  if (!is.null(accu_layer))
    if (!file.exists(accu_layer))
      stop(paste0("accu_layer: ", accu_layer, " does not exist."))

  # Check if stream_layer ends with .tif
  if (!endsWith(stream_layer, ".tif"))
    stop("stream_layer: Stream network raster is not a .tif file.")
  # Check if accu_layer ends with .tif
  if (!is.null(accu_layer))
  if (!endsWith(accu_layer, ".tif"))
    stop("accu_layer: Flow accumulation raster is not a .tif file.")

  # Check if method is set properly
  if (!(method == "distance" || method == "accumulation" || method == "both"))
    stop("method: Has to be 'distance', 'accumulation', or 'both'.")

  # Check if values of distance and accumulation are numeric
  if (!is.numeric(distance))
    stop("distance: Value has to be numeric.")
  if (!is.numeric(accumulation))
    stop("accumulation: Value has to be numeric.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Create random string to attach to the file name of the temporary
  # output coordinates and input ids file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  # Select columns with lon/lat coordinates
  columns <- c(id, lon, lat)
  coord <- as.data.table(data)[, ..columns]
  # Remove duplicated rows across entire data frame
  coord <- coord[!duplicated(coord), ]

  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".csv")
  ## Note: Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = ",")
  # Path for tmp regional unit ids text file
  snap_tmp_path <- paste0(tempdir(), "/snapped_points_", rand_string, ".txt")

  # Check operating system
  sys_os <- get_os()
  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    # Convert NULL argument to "NA" so that the bash script can evaluate
    # the argument
    accu_layer <- ifelse(is.null(accu_layer), "NA", accu_layer)

    processx::run(system.file("sh", "snap_to_network.sh",
                    package = "hydrographr"),
        args = c(coord_tmp_path, id, lon, lat,
                 stream_layer, accu_layer, method, distance, accumulation,
                 snap_tmp_path, tempdir()),
        echo = !quiet)


  } else {

    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_stream_layer <- fix_path(stream_layer)
    wsl_accu_layer <- ifelse(is.null(accu_layer), "NA", fix_path(accu_layer))
    wsl_snap_tmp_path <- fix_path(snap_tmp_path)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_sh_file <- fix_path(system.file("sh", "snap_to_network.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "snap_to_network.bat",
                    package = "hydrographr"),
        args = c(wsl_coord_tmp_path, id, lon, lat,
                 wsl_stream_layer, wsl_accu_layer, method, distance, accumulation,
                 wsl_snap_tmp_path, wsl_tmp_path, wsl_sh_file),
        echo = !quiet)
  }
  snapped_coord <- fread(snap_tmp_path,
                         keepLeadingZeros = TRUE,
                         header = TRUE, sep = " ")


  # Remove files in the tmp folder
  file.remove(coord_tmp_path, snap_tmp_path)

  # Return snapped coordinates
  return(snapped_coord)

}
