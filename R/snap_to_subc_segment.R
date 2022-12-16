#' Snaps data points to the stream segment within the sub-catchment
#'
#' @description
#' Snaps data points to the stream segment of the
#' sub-catchment the data point is located.

#' @param data Data.frame with lat/lon columns in WGS84.
#' @param lon Column name of longitude coordinates as character string.
#' @param lat Column name of latitude coordinates as character string.
#' @param site_id Column name of unique site IDs as character string. Each row
#' of the column needs to have an unique ID.
#' @param basin_id Column name of basin IDs as character string. If NULL
#' basin ID will be extracted automatically beforehand.
#' @param subc_id Column name of sub-catchment IDs as character string. If
#' NULL sub-catchment ID will be extracted automatically beforehand.
#' @param basin_path Full path of the basin .tif layer.
#' @param subc_path Full path of the sub-catchment .tif layer.
#' @param stream_path Full path of stream network .gpkg file.
#' @param cores Number of cores used for parallelization. By default the process
#' does not run in parallel.
#' @param quiet Whether the standard output will be printed or not.
#'
#' @importFrom parallel detectCores
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select left_join
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#' @details
#' The function uses the network preparation and maintenance module of
#' GRASS GIS (v.net), to connect a vector lines map (stream network) with a
#' points map (occurrence/sampling points). After masking the stream segment and
#' the sub-catchment where the target point is located, the connect operation
#' snaps the point to the stream segment using a distance threshold. This
#' threshold is automatically calculated as the longest distance between two
#' points within the sub-catchment. In this way the snapping will always take
#' place.This operation creates a new node on the vector line (i.e. stream
#' segment) from which the new snapped coordinates can be extracted.
#'
#' @author Jaime Garcia Marquez, Maria M. Üblacker
#'
#' @references
#' \url{https://grass.osgeo.org/grass82/manuals/v.net.html}
#'
#' @seealso
#' \code{\link{snap_to_network}} to snap the data points to the next stream
#' segment within a given radius and/or a given flow accumulation threshold
#' value.
#' \code{\link{extract_ids}} to extract basin and sub-catchment IDs.
#'
#'@examples
#' # Download test data into temporary R folder
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load occurrence data
#' species_occurence <- read.table(paste0(my_directory,
#'                                 "/hydrography90m_test_data/spdata_1264942.txt"),
#'                                 header = TRUE)
# Define full path to the basin and sub-catchments raster layer
#' basin_rast <- paste0(my_directory,
#'                      "/hydrography90m_test_data/basin_1264942.tif")
#' subc_rast <- paste0(my_directory,
#'                     "/hydrography90m_test_data/basin_1264942.tif")
#'
#' # Define full path to the vector file of the stream network
#' stream_vect <- paste0(my_directory,
#'                       "/hydrography90m_test_data/order_vect_59.gpkg")
#'
# EITHER
# Extract basin and sub-catchment IDs from the Hydrography90m layers beforehand
#' hydrography90m_ids <- extract_ids(data = species_occurence,
#'                                   lon = "longitude",
#'                                   lat = "latitude",
#'                                   site_id = "occurrence_id",
#'                                   subc_path = subc_rast,
#'                                   basin_path = basin_rast)
#'
#' # Snap data points to the stream segment of the provided sub-catchment ID
#' snapped_coordinates <- snap_to_subc_segment(data = hydrography90m_ids,
#'                                             lon = "longitude",
#'                                             lat = "latitude",
#'                                             site_id = "occurrence_id",
#'                                             basin_id = "basin_id",
#'                                             subc_id = "subcatchment_id",
#'                                             basin_path = basin_rast,
#'                                             subc_path = subc_rast,
#'                                             stream_path = stream_vect,
#'                                             cores = 2)
#' # Show head of output table
#' head(snapped_coordinates)
#'
#' # OR
#' # Automatically extract the basin and sub-catchment IDs and
#' # snap the data points to the stream segment
#' snapped_coordinates <- snap_to_subc_segment(data = species_occurence,
#'                                             lon = "longitude",
#'                                             lat = "latitude",
#'                                             site_id = "occurrence_id",
#'                                             basin_path = basin_rast,
#'                                             subc_path = subc_rast,
#'                                             stream_path = stream_vect,
#'                                             cores = 2)
#' # Show head of output table
#' head(snapped_coordinates)
#'


snap_to_subc_segment <- function(data, lon, lat, site_id, basin_id = NULL,
                                 subc_id = NULL, basin_path, subc_path,
                                 stream_path, cores = 1, quiet = TRUE) {
  # Check operating system
  system <- get_os()

  # Check if any of the arguments is missing
  for (arg in  c(data, lon, lat, site_id, basin_path, subc_path,
                stream_path)) {
    if (missing(arg))
      stop(paste0(quote(arg), " is missing."))
    }

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if lon, lat, site_id, basin_id, and subc_id column names
  # are character strings
  for (name in  c(lon, lat, site_id)) {
  if (!is.character(name))
    stop(paste0("Column name ", name, " is not a character string."))
  }

  if (!is.null(basin_id))
    if (!is.character(basin_id))
      stop(paste0("Column name ", basin_id, " is not a character string."))

  if (!is.null(subc_id))
    if (!is.character(subc_id))
      stop(paste0("Column name ", subc_id, " is not a character string."))

  # Check if lon, lat, site_id, basin_id, and subc_id column names exist
  for (name in c(lon, lat, site_id)) {
    if (is.null(data[[name]]))
      stop(paste0("Column name '", name, "' does not exist."))
  }

  if (!is.null(basin_id))
    if (is.null(data[[basin_id]]))
      stop(paste0("Column name '", basin_id, "' does not exist."))

  if (!is.null(subc_id))
    if (is.null(data[[subc_id]]))
      stop(paste0("Column name '", subc_id, "' does not exist."))

  # Check site_id for duplicated IDs
  if (length(unique(data[[site_id]])) != length(data[[site_id]]))
    stop(paste0("Column '", site_id, "' has rows with duplicated IDs."))


   # Check if paths exists
  for (path in c(basin_path, subc_path, stream_path)) {
   if (!file.exists(path))
     stop(paste0("File path: ", path, " does not exist."))
  }

  # Check if basin_path and subc_path ends with .tif
  for (path in c(basin_path, subc_path)) {
    if (!endsWith(path, ".tif"))
      stop(paste0("File path: ", path, " does not end with .tif"))
  }

  # Check if basin_path and subc_path ends with .gpkg
  if (!endsWith(stream_path, ".gpkg"))
    stop(paste0("File path: ", stream_path, " does not end with .gpkg"))

  # Check if value of cores numeric
  if (!is.numeric(cores))
    stop("cores: Value has to be numeric.")

  # Check if cpus are available.
  if (cores >= detectCores()) {
    stop("Number of cores set is higher than number of available cores.")
  }

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # If basin_id and/or subc_id is NULL
  # Extract ids first
  if (is.null(basin_id) && is.null(subc_id)) {
    # Extract subc and basin ids
    subc_basin_ids <- extract_ids(data = data, lon = lon, lat = lat,
                                  subcatchment_path = subc_path,
                                  basin_path = basin_path, quiet = quiet)
    # Join with data and select columns needed for the bash script
    ids <- left_join(data, subc_basin_ids, by = c(lon, lat)) %>%
      select(matches(c(site_id, lon, lat)), basin_id, subcatchment_id)

  } else if (is.null(basin_id) && !is.null(subc_id)) {
    # Extract basin ids
    basin_ids <- extract_ids(data = data, lon = lon, lat = lat,
                             subcatchment_path = NULL,
                             basin_path = basin_path, quiet = quiet)
    # Join with data and select columns needed for the bash script
    ids <- left_join(data, subc_basin_ids, by = c(lon, lat)) %>%
      select(matches(c(site_id, lon, lat)), basin_id, matches(subc_id))


  } else if (!is.null(basin_id) && is.null(subc_id)) {
    # Extract sub-catchment ids
    subc_ids <- extract_ids(data = data, lon = lon, lat = lat,
                            subcatchment_path = subc_path,
                            basin_path = NULL, quiet = quiet)
    # Join with data and select columns needed for the bash script
    ids <- data %>%
      left_join(., subc_basin_ids, by = c(lon, lat)) %>%
      select(matches(c(site_id, lon, lat, basin_id)),  subcatchment_id)
#
  } else {
    # Select columns needed for the bash script
    ids <- data %>%
      select(matches(c(site_id, lon, lat, basin_id, subc_id)))
  }

  # Create random string to attach to the file name of the temporary
  # output coordinates and input ids file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Export taxon point ids
  ids_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".csv")
  fwrite(ids, ids_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = ",")
  # Path for tmp regional unit ids text file
  snap_tmp_path <- paste0(tempdir(), "/snapped_points", rand_string, ".csv")

  # Make bash scripts executable
  make_sh_exec()

  if (system == "linux" || system == "osx") {

    processx::run(system.file("sh", "snap_to_subc_segment.sh",
                    package = "hydrographr"),
        args = c(ids_tmp_path, lon, lat, basin_path, subc_path, stream_path,
                 cores, snap_tmp_path,  tempdir()),
        echo = !quiet)


  } else {

    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_ids_tmp_path <- fix_path(ids_tmp_path)
    wsl_basin_path <- fix_path(basin_path)
    wsl_subc_path <- fix_path(subc_path)
    wsl_stream_path <- fix_path(stream_path)
    wsl_snap_tmp_path <- fix_path(snap_tmp_path)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_sh_file <- fix_path(system.file("sh", "snap_to_subc_segment.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "snap_to_subc_segment.bat",
                    package = "hydrographr"),
        args = c(wsl_ids_tmp_path, lon, lat, wsl_basin_path,
                 wsl_subc_path, wsl_stream_path, cores,
                 wsl_snap_tmp_path, wsl_tmp_path, wsl_sh_file),
        echo = !quiet)
  }
  snapped_coord <- fread(paste0(tempdir(), "/snapped_points",
                                rand_string, ".csv"),
                         keepLeadingZeros = TRUE, header = TRUE, sep = ",")

  # Remove files in the tmp folder
  file.remove(snap_tmp_path)

  # Return snapped coordinates
  return(snapped_coord)

}
