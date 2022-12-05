#' Snap data points to the next stream segment
#' Snaps data/sampling points to the next stream segment within a defined radius
#' or a minimum flow accumulation.
#'
#'
#' @param data Data.frame with lat lon columns in WGS84
#' @param lon Name of the longitude column as character string
#' @param lat Name of the latitude column as character string
#' @param stream_path Full path of the stream network .tif file
#' @param accu_path Full path of the flow accumulation .tif file. Needed if
#' the the point should be snapped to next stream segment with a minimum flow
#' accumulation.
#' @param calc Options "dist", "accu", or "both". Default is "dist".
#' Defines if the points are snapped using the distance or flow accumulation.
#' If calc is set to "both" the output will contain the new coordinates for both
#' calculations.
#' @param dist Maximum radius in meters; Point will be snapped to the next
#' stream within a defined radius. By default the distance is 500 meter.
#' @param accu Minimum flow accumulation; Point will be snapped to the next
#' stream with a flow accumulation equal or higher than the given value. By
#' default the flow accumulation value is set to 0.5.
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'

snap_to_network <- function(data, lon, lat, stream_path, accu_path = NULL,
                            calc = "dist", dist = 500, accu = 0.5,
                            quiet = TRUE) {
  # Check if data.frame is defined
  if (missing(data))
    stop("Input data is missing.")

  # Check if input data is of type data.frame,
  # data.table or tibble
  if(!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if column name is defined
  if (missing(lon))
    stop("Column name of longitudinal coordinate is not defined.")

  # Check if column name is defined
  if (missing(lat))
    stop("Column name of latitudinal coordinate is not defined.")

  # Check if lon/lat column names are character strings
  if(!is.character(lon))
    stop("lon: Column name is not a character string.")
  if(!is.character(lat))
    stop("lat: Column name is not a character string.")

  # Check if lon/lat column names exist
  if(is.null(data[[lon]]))
    stop(paste0("Column name '",lon ,"' does not exist."))
  if(is.null(data[[lat]]))
    stop(paste0("Column name '",lat ,"' does not exist."))

  # Check if values of the lon/lat columns are numeric
  if(!is.numeric(data[[lon]]))
    stop(paste0("Column ", lon, " has to be numeric."))
  if(!is.numeric(data[[lat]]))
    stop(paste0("Column ", lat, " has to be numeric."))

  # Add here: if condition to check if lat/long columns are in WGS84

  # Check if stream_path is defined
  if (missing(stream_path))
    stop("stream_path is missing.")
  # Check if accu_path is defined
  # If calc is set to "accu" or "both"
  if (calc == "accu" || calc == "both")
    if(is.null(accu_path))
      stop(paste0("accu_path is missing."))

  # Check if stream_path exists
  if (!file.exists(stream_path))
    stop(paste0("stream_path: ", stream_path, " does not exist."))
  # Check if accu_path exists
  if (!is.null(accu_path))
    if(!file.exists(accu_path))
      stop(paste0("accu_path: ", accu_path, " does not exist."))

  # Check if stream_path ends with .tif
  if (!endsWith(stream_path, ".tif"))
    stop("stream_path: Stream network raster is not a .tif file.")
  # Check if accu_path ends with .tif
  if (!is.null(accu_path))
  if (!endsWith(accu_path, ".tif"))
    stop("accu_path: Flow accumulation raster is not a .tif file.")

  # Check if calc is set probably
  if (!(calc == "dist" || calc == "accu" || calc == "both"))
    stop("calc: Has to be 'dist', 'accu', or 'both'.")

  # Check if values of dist and accu are numeric
  if(!is.numeric(dist))
    stop("dist: Value has to be numeric.")
  if(!is.numeric(accu))
    stop("accu: Value has to be numeric.")

  # Check if quiet is logical
  if(!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Check operating system
  system <- get_os()
  # Make bash scripts executable
  make_sh_exec()

  # Create random string to attach to the file name of the temporary
  # output coordinates and input ids file
  rand_string <- stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
  # Select columns with lon/lat coordinates
  coord <- data %>%
    select(matches(c(lon, lat)))
  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".csv")
  ## Note: Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = ",")
  # Path for tmp regional unit ids text file
  snap_tmp_path <- paste0(tempdir(), "/snapped_points", rand_string, ".txt")

  if (system == "linux" || system == "osx"){

    # Convert NULL argument to "NA" so that the bash script can evaluate
    # the argument
    accu_path <- ifelse(is.null(accu_path), "NA", accu_path)

    run(system.file("sh", "snap_to_network.sh",
                    package = "hydrographr"),
        args = c(coord_tmp_path, lon, lat,
                 stream_path, accu_path, calc, dist, accu,
                 snap_tmp_path, tempdir()),
        echo = !quiet)


  } else {

    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_stream_path <- fix_path(stream_path)
    wsl_accu_path <- ifelse(is.null(accu_path), "NA", fix_path(accu_path))
    wsl_snap_tmp_path <- fix_path(snap_tmp_path)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_sh_file <- fix_path(system.file("sh", "snap_to_network.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "snap_to_network.bat",
                    package = "hydrographr"),
        args = c(wsl_coord_tmp_path, lon, lat,
                 wsl_stream_path, wsl_accu_path, calc, dist, accu,
                 wsl_snap_tmp_path, wsl_tmp_path, wsl_sh_file),
        echo = !quiet)
  }
  snapped_coord <- fread(paste0(tempdir(), "/snapped_points", rand_string, ".txt"),
                             keepLeadingZeros = TRUE, header = TRUE, sep = " ")

  # Remove files in the tmp folder
  file.remove(coord_tmp_path, snap_tmp_path)

  # Return snapped coordinates
  return(snapped_coord)

}
