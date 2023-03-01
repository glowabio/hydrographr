#' @title Calculate euclidean or along the network distance between points
#'
#' @description
#' Calculate euclidean or along the network distance between points
#'

#' @param data a data.frame or data.table with lat/lon coordinates in WGS84.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param id character. The name of a column containing unique IDs for each row
#' of "data" (e.g., occurrence or site IDs).
#' @param basin_id character. The name of the column with the basin IDs.
#' If NULL and distance is set to 'network' or 'both', the basin IDs will be
#' extracted automatically. Default is NULL.
#' @param basin_layer character. Full path to the basin ID .tif layer.
#' @param stream_layer character. Full path of the stream network .gpkg file.
#' @param distance character. One of "euclidean", "network", or "both".
#' If "euclidean", the euclidean distances between all pairs of points are
#' calculated. If "network", the shortest path along the network between all
#' pairs of points is calculated. (see "Details" for more information).
#' If method is set to "both", both distance measures are calculated.
#' Default is "both".
#' @param n_cores numeric. Number of cores used for parallelization.
#' Default is 1.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom parallel detectCores
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select left_join
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#' @details
#' To calculate the euclidian distance between all pair of points the function
#' uses the v.distance command of GRASS GIS, which has been set up to produce a
#' square matrix of distances. The calculation of distances along the stream
#' network has been implemented with the command v.net.allpairs of GRASS GIS. 
#' The along the network distance calculation is done for all pair points 
#' located within the same basin. If the points are located in
#' different basins the function can be run in parallel (i.e., each core for
#' the distance calculations of all points within one basin). The
#' distance between points located in different
#' basins is zero because they are not connected through the network.
#' 
#' @returns
#' If distance='euclidean', a distance matrix, in meters, of the euclidean distances between
#' all the pairs of points (object of class data.frame). If distance='network',
#' a data.frame with three columns: from_id, to_id, dist. The 'dist' column
#' includes the distance, in meters, of the shortest path along the network from the node
#' from_id to the node to_id. If distance='both', a list containing both objects
#' is returned.
#'
#' @author Afroditi Grigoropoulou, Maria M. Üblacker, Jaime Garcia Marquez
#'
#' @references
#' \url{https://grass.osgeo.org/grass82/manuals/v.net.allpairs.html}
#' \url{https://grass.osgeo.org/grass82/manuals/v.distance.html}
#'
#' @seealso
#' \code{\link{snap_to_network}} to snap the data points to the next stream
#' segment within a given radius and/or a given flow accumulation threshold
#' value.
#' \code{\link{extract_ids}} to extract basin and sub-catchment IDs.
#'
#'@examples
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
#  # Define full path to the basin raster layer
#' basin_rast <- paste0(my_directory,
#'                      "/hydrography90m_test_data/basin_1264942.tif")
#'
#' # Define full path to the sub-catchment raster layer
#' subc_rast <- paste0(my_directory,
#'                     "/hydrography90m_test_data/subcatchment_1264942.tif")
#'
#' # Define full path to the vector file of the stream network
#' stream_vect <- paste0(my_directory,
#'                       "/hydrography90m_test_data/order_vect_59.gpkg")
#'
#' # Automatically extract the basin and sub-catchment IDs and
#' # snap the data points to the stream segment
#' snapped_coordinates <- snap_to_subc_segment(data = species_occurrence,
#'                                             lon = "longitude",
#'                                             lat = "latitude",
#'                                             id = "occurrence_id",
#'                                             basin_layer = basin_rast,
#'                                             subc_layer = subc_rast,
#'                                             stream_layer = stream_vect,
#'                                             n_cores = 2)
#' # Show head of output table
#' head(snapped_coordinates)
#'
#' # Get the euclidean distance and the distance along the network between all
#' # pairs of points
#' distance_table <- get_distance(data = snapped_coordinates,
#'                                lon = "lon_snap",
#'                                lat = "lat_snap",
#'                                id = "occurrence_id",
#'                                basin_id = "basin_id",
#'                                basin_layer = basin_rast,
#'                                stream_layer = stream_vect,
#'                                distance = "network",
#'                                n_cores = 2)
#' # Show table
#' distance_table


get_distance <- function(data, lon, lat, id, basin_id = NULL,
                         basin_layer = NULL, stream_layer = NULL,
                         distance = "all", n_cores = 1, quiet = TRUE) {


  # Check if any of the arguments is missing
  for (arg in  c(data, lon, lat, id, basin_layer, stream_layer)) {
    if (missing(arg))
      stop(paste0(quote(arg), " is missing."))
    }

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if lon, lat, id, basin_id column names
  # are character strings
  for (name in  c(lon, lat, id)) {
  if (!is.character(name))
    stop(paste0("Column name ", name, " is not a character string."))
  }

  if (!is.null(basin_id))
    if (!is.character(basin_id))
      stop(paste0("Column name ", basin_id, " is not a character string."))


  # Check if lon, lat, id, basin_id column names exist
  for (name in c(lon, lat, id)) {
    if (is.null(data[[name]]))
      stop(paste0("Column name '", name, "' does not exist."))
  }

  if (!is.null(basin_id))
    if (is.null(data[[basin_id]]))
      stop(paste0("Column name '", basin_id, "' does not exist."))


  # Check id for duplicated IDs
  if (length(unique(data[[id]])) != length(data[[id]]))
    stop(paste0("Column '", id, "' has rows with duplicated IDs."))

  # Check if distance is set properly
  if (!(distance == "both" || distance == "euclidean" || distance == "network"))
    stop("distance: Has to be 'euclidean', 'network', or 'both'.")

   # Check if paths exists
  for (path in c(basin_layer, stream_layer)) {
   if (!file.exists(path))
     stop(paste0("File path: ", path, " does not exist."))
  }

  # Check if basin_layer ends with .tif
  for (path in basin_layer) {
    if (!endsWith(path, ".tif"))
      stop(paste0("File path: ", path, " does not end with .tif"))
  }

  # Check if basin_layer ends with .gpkg
  if (!is.null(stream_layer))
    if (!endsWith(stream_layer, ".gpkg"))
      stop(paste0("File path: ", stream_layer, " does not end with .gpkg"))

  # Check if value of n_cores numeric
  if (!is.numeric(n_cores))
    stop("n_cores: Value has to be numeric.")

  # Check if cpus are available.
  if (n_cores >= detectCores()) {
    stop("Number of n_cores set is higher than number of available cores.")
  }

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # If basin_id is NULL
  # Extract ids first
  if (is.null(basin_id)) {
    # Extract basin ids
    basin_ids <- extract_ids(data = data, lon = lon, lat = lat,
                             subc_layer = NULL,basin_layer = basin_layer,
                             quiet = quiet)

    # Join with data and select columns needed for the bash script
    # Note: "id" should be the first column
    ids <- left_join(data, basin_ids, by = c(lon, lat)) %>%
      select(matches(c(id, lon, lat)), basin_id)

  } else {
    # Select columns needed for the bash script
    ids <- data %>%
      select(matches(c(id, lon, lat, basin_id)))
  }

  # Create random string to attach to the file name of the temporary
  # output tables and input ids file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Export taxon point ids
  ids_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".csv")
  fwrite(ids, ids_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = ",")
  # Path for distance output tmp dir
  dist_tmp_dir <- paste0(tempdir(), "/distance")
  # Create distance general tmp dir
  dir.create(dist_tmp_dir, showWarnings = FALSE)

  # Create output directories for distance tables
  # and assign paths to the output files within the directories
  if(distance == "euclidean" || distance == "both") {
    dir.create(paste0(tempdir(), "/distance/dist_eucl"), showWarnings = FALSE)
    # Path for tmp euclidean distance input csv file
    dist_eucl_tmp_path <- paste0(tempdir(),
                                 "/distance/dist_eucl/dist_euclidean_",
                                 rand_string, ".csv")
  } else {dist_eucl_tmp_path <- NULL}

  if(distance == "network" || distance == "both") {
    dir.create(paste0(tempdir(), "/distance/dist_net"), showWarnings = FALSE)
    # Path for tmp network distance input csv file
    dist_net_tmp_path <- paste0(tempdir(),
                                "/distance/dist_net/dist_network_",
                                rand_string, ".csv")

  } else {dist_net_tmp_path <- NULL}


  # Convert NULL argument to "NA" so that the bash script can evaluate
  # the argument
  basin_id <- ifelse(is.null(basin_id), "NA", basin_id)
  stream_layer <- ifelse(is.null(stream_layer), "NA", stream_layer)
  basin_layer <- ifelse(is.null(basin_layer), "NA", basin_layer)
  dist_eucl_tmp_path <- ifelse(is.null(dist_eucl_tmp_path), "NA",
                               dist_eucl_tmp_path)
  dist_net_tmp_path <- ifelse(is.null(dist_net_tmp_path), "NA",
                              dist_net_tmp_path)


  # Check operating system
  sys_os <- get_os()
  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    processx::run(system.file("sh", "get_distance.sh",
                    package = "hydrographr"),
        args = c(dist_tmp_dir, ids_tmp_path, lon, lat, basin_id,
                 stream_layer, basin_layer, dist_eucl_tmp_path,
                 dist_net_tmp_path, n_cores, distance),
        echo = !quiet)


  } else {

    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_dist_tmp_dir <- fix_path(dist_tmp_dir)
    wsl_ids_tmp_path <- fix_path(ids_tmp_path)
    wsl_stream_layer <- fix_path(stream_layer)
    wsl_basin_layer <- fix_path(basin_layer)
    wsl_dist_eucl_tmp_path <- fix_path(dist_eucl_tmp_path)
    wsl_dist_net_tmp_path <- fix_path(dist_net_tmp_path)
    wsl_sh_file <- fix_path(system.file("sh", "get_distance.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "get_distance.bat",
                    package = "hydrographr"),
        args = c(wsl_dist_tmp_dir, wsl_ids_tmp_path, lon, lat, basin_id,
                 wsl_stream_layer, wsl_basin_layer, wsl_dist_eucl_tmp_path,
                 wsl_dist_net_tmp_path, n_cores, distance, wsl_sh_file),
        echo = !quiet)
  }

   # Import distance tables
  if(distance == "euclidean" || distance == "both") {
    dist_eucl_table <- fread(dist_eucl_tmp_path,
                             keepLeadingZeros = TRUE, header = TRUE, sep = ",")

  }
  if(distance == "network" || distance == "both") {
    dist_net_table <- fread(dist_net_tmp_path,
                            keepLeadingZeros = TRUE, header = TRUE, sep = ",")
  }


  # Return data frames or a list of the 2 data frames
  if(distance == "euclidean") {
    return(dist_eucl_table)
  }
  if(distance == "network") {
    return(dist_net_table)
  }
  if(distance == "both") {
    return(list(dist_eucl_table, dist_net_table))
  }


  # Remove files in the tmp folder
  file.remove(c(dist_eucl_tmp_path, dist_net_tmp_path))

}
