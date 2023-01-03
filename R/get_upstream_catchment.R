#' Calculates the upstream basin taking each point as the outlet
#'
#' @param data Data.frame of snapped coordinates
#' @param id Name of the column containing the occurrence/site id
#' @param lon Name of the longitude column (WGS 84)
#' @param lat Name of the latitude column (WGS 84)
#' @param direction_layer Full path to raster file with the direction variable
#' @param out_path Full path to the directory where the output(s) will be stored
#' @param n_cores Numeric. Number of cores used for parallelization
#' @param quiet If FALSE, process is printed
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom parallel detectCores
#' @export
#'
#'
get_upstream_catchment <- function(data, id = character(0), lon = character(0),
                        lat = character(0), direction_layer = NULL,
                        out_path = NULL, n_cores = NULL, quiet = TRUE) {

  system <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  # Create random string to attach to the file name of the temporary
  # points_dataset.txt and ids.txt file
  rand_string <- stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
  # Select columns with lon/lat coordinates and occurrence/site id
  coord_id <- data %>%
    select(matches(c(id, lon, lat)))
  # Export occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_id_", rand_string, ".txt")
  ## Note:Only export lon/lat and id columns
  fwrite(coord_id, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")


  # Setting up parallelization if n_cores is not provided
  n_cores <- ifelse(is.null(n_cores), detectCores() - 1, n_cores)


  if (system == "linux" | system == "osx"){


    # Call the external .sh script get_upstream_catchment() containing the
    # GRASS functions
    processx::run(system.file("sh", "get_upstream_catchment.sh",
        package = "hydrographr"),
        args = c(coord_tmp_path, id, lon, lat, direction_layer, out_path,
                 n_cores),
        echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_dire_path <- ifelse(is.null(direction_layer), 0,
                            fix_path(direction_layer))
    wsl_sh_file <- fix_path(
      system.file("sh", "get_upstream_catchment.sh",
                  package = "hydrographr"))

    run(system.file("bat", "get_upstream_catchment.bat",
                                package = "hydrographr"),
                    args = c(wsl_coord_tmp_path, id, lon, lat, wsl_dire_path,
                             wsl_bas_path, wsl_tmp_path, n_cores, wsl_sh_file),
                    echo = !quiet)

  }

  # Remove all files in the tmp folder
  file.remove(coord_tmp_path)

  # Print output location
  print(paste0("The output is in ", out_path))

}

