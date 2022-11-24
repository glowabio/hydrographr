#' Snap data point to the next stream reach
#' Snappes data points to the next stream reach within a defined radius
#' or based on a flow accumulation threshold,
#'
#'
#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column as character string
#' @param lat Name of the latitude column as character string
#' @param stream_path Full path to the stream network .tif file
#' @param radius Radius distance in meters; Point will be snapped to the next
#' stream within a certain distance.
#' @param flow_path Full path to the flow accumulation .tif file.
#' @param thre Flow accumulation threshold value; Point will be snapped to the next
#' stream with a flow accumulation equal or higher than threshold value.
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#'
snap_to_network <- function(data, lon, lat, stream_path = NULL, radius = 500,
                            flow_path = NULL, thre = 0.5, quiet = TRUE) {
  # Check operating system
  system <- get_os()
  # Make bash scripts executable
  make_sh_exec()

  # Convert NULL argument to "NA" so that the bash script can evaluate
  # the argument
  stream_path <- ifelse(is.null(stream_path), "NA", stream_path)
  flow_path <- ifelse(is.null(flow_path), "NA", flow_path)

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

  run(system.file("sh", "snap_to_network.sh",
                  package = "hydrographr"),
      args = c(coord_tmp_path, lon, lat,
               stream_path, radius, flow_path, thre,
               snap_tmp_path, tempdir()),
      echo = !quiet)


  } else {

    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_stream_path <- fix_path(stream_path)
    wsl_flow_path <- fix_path(flow_path)
    wsl_snap_tmp_path <- fix_path(ids_tmp_path)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_sh_file <- fix_path(system.file("sh", "snap_to_network.sh",
                                        package = "hydrographr"))

    run(system.file("bat", "snap_to_network.bat",
                    package = "hydrographr"),
        args = c(wsl_coord_tmp_path, lon, lat,
                 wsl_stream_path, radius, wsl_flow_path, thre,
                 wsl_snap_tmp_path, wsl_tmp_path, wsl_sh_file),
        echo = !quiet)
  }


  }



}
