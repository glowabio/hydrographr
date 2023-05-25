#' @title Delineate the upstream catchment

#' @description Delineates the upstream catchment of a given point, where the
#' point is considered as the outlet of the catchment.
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the longitude / latitude coordinates in WGS84. Note that the points need to
#' be snapped to the stream network with \code{\link{snap_to_network()}} or
#' \code{\link{snap_to_subc_segment()}}.
#' @param id character. The name of a column containing unique IDs for each row
#' of "data" (e.g., occurrence or site IDs).
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param direction_layer character. Full path to the flow direction raster file.
#' @param out_dir Full path to the directory where the output(s) will be stored.
#' The output file name includes the "id" which helps identifying  the upstream
#' corresponding catchment.
#' @param n_cores numeric. Number of cores used for parallelisation.
#' If NULL, available cores - 1 will be used. Default is NULL.
#' @param compression character. Compression of the written output file.
#' Compression levels can be defined as "none", "low", or "high". Default is
#' "low", referring to compression type "DEFLATE" and compression level 2.
#' "high" refers to compression level 9.
#' @param bigtiff logical. Define whether the output file is expected to be a
#' BIGTIFF (file size larger than 4 GB). If FALSE and size > 4GB no file will be
#' written. Default is TRUE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom parallel detectCores
#' @export
#'
#' @author Jaime Garcia Marquez, Afroditi Grigoropoulou, Maria M. Üblacker
#'
#' @references
#' * \url{https://grass.osgeo.org/grass82/manuals/r.water.outlet.html}
#' * \url{https://grass.osgeo.org/grass82/manuals/r.region.html}
#' @md
#'
#' @seealso
#' * \code{\link{snap_to_network}} to snap the data points to the next stream
#' segment within a given radius and/or a given flow accumulation threshold
#' value.
#' * \code{\link{snap_to_subc_segment}} to snap the data points to the next stream
#' segment within the sub-catchment the point is located.
#' * \code{\link{extract_ids}} to extract basin and sub-catchment IDs.
#' @md
#'
#' @examples
#' # Download test data into temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Before running the function get_upstream_catchment(), snap the points to
#' # to the stream segment. There are multiple ways to snap the points. Here is
#' # one example:
#'
#' # Load occurrence data
#' species_occurence <- read.table(paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/spdata_1264942.txt"),
#'                               header = TRUE)
#'
#' # Define full path to the basin and sub-catchments raster layer
#' basin_raster <- paste0(my_directory,
#'                        "/hydrography90m_test_data/basin_1264942.tif")
#' subc_raster <- paste0(my_directory,
#'                       "/hydrography90m_test_data/subcatchment_1264942.tif")
#'
#' # Define full path to the vector file of the stream network
#' stream_vector <- paste0(my_directory,
#'                         "/hydrography90m_test_data/order_vect_59.gpkg")
#'
#' # Automatically extract the basin and sub-catchment IDs and
#' # snap the data points to the stream segment
#' snapped_coordinates <- snap_to_subc_segment(data = species_occurence,
#'                                             lon = "longitude",
#'                                             lat = "latitude",
#'                                             id = "occurrence_id",
#'                                             basin_layer = basin_raster,
#'                                             subc_layer = subc_raster,
#'                                             stream_layer = stream_vector,
#'                                             n_cores = 2)
#'
#' # Define full path to the direction .tif
#' direction_raster <- paste0(my_directory,
#'                            "/hydrography90m_test_data/direction_1264942.tif")
#' # Define the path for the output file(s)
#' output_folder <-  paste0(my_directory, "/upstream_catchments")
#' if(!dir.exists(output_folder)) dir.create(output_folder)
#' # Get the upstream catchment for each point location
#' get_upstream_catchment(snapped_coordinates,
#'                        lon = "lon_snap",
#'                        lat = "lat_snap",
#'                        id = "occurrence_id",
#'                        direction_layer = direction_raster,
#'                        out_dir = output_folder,
#'                        n_cores = 2)


get_upstream_catchment <- function(data, id, lon, lat, direction_layer = NULL,
                                   out_dir = NULL, n_cores = NULL,
                                   compression = "low", bigtiff = TRUE,
                                   quiet = TRUE) {

  # Check if data.frame is defined
  if (missing(data))
    stop("data: Input data.frame is missing.")

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
  if (!is.character(id))
    stop("id: Column name is not a character vector.")

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

  # Check if direction_layer exists
  if (!file.exists(direction_layer))
    stop(paste0("direction_layer: ", direction_layer, " does not exist."))
  # Check if accu_path exists
    if (!dir.exists(out_dir))
      stop(paste0("out_dir: ", out_dir, " does not exist."))

  # Check if direction_layer ends with .tif
  if (!endsWith(direction_layer, ".tif"))
    stop("direction_layer: Stream network raster is not a .tif file.")

  # Check if value of cores numeric
  if (!is.numeric(n_cores))
    stop("n_cores: Value has to be numeric.")

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

  # Create random string to attach to the file name of the temporary
  # points_dataset.txt and ids.txt file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Select columns with lon/lat coordinates and occurrence/site id
  columns <- c(id, lon, lat)
  coord_id <- as.data.table(data)[, ..columns]

  # Export occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_id_", rand_string, ".txt")
  ## Note:Only export lon/lat and id columns
  fwrite(coord_id, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")


  # Setting up parallelization if n_cores is not provided
  n_cores <- ifelse(is.null(n_cores), detectCores(logical = FALSE) - 1, n_cores)

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()


  if (sys_os == "linux" || sys_os == "osx") {

    # Call the external .sh script get_upstream_catchment() containing the
    # GRASS functions
    processx::run(system.file("sh", "get_upstream_catchment.sh",
                              package = "hydrographr"),
                  args = c(coord_tmp_path, id, lon, lat, direction_layer,
                           out_dir, n_cores,
                           compression_type, compression_level, bigtiff),
                  echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_dire_path <- ifelse(is.null(direction_layer), 0,
                            fix_path(direction_layer))
    wsl_out_dir <- fix_path(out_dir)
    wsl_sh_file <- fix_path(
      system.file("sh", "get_upstream_catchment.sh",
                  package = "hydrographr"))

    run(system.file("bat", "get_upstream_catchment.bat",
                                package = "hydrographr"),
                    args = c(wsl_coord_tmp_path, id, lon, lat, wsl_dire_path,
                             wsl_out_dir, n_cores, compression_type, compression_level,
                             bigtiff, wsl_sh_file),
                    echo = !quiet)

  }

  # Remove all files in the tmp folder
  file.remove(coord_tmp_path)

  # Print output location
  cat("Output file(s) saved under: ", out_dir,"\n")
}
