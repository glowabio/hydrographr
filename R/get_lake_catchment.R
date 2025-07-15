#' @title Get lake catchments based on intersection points between stream network and lakes
#'
#' @description Delineate lake catchments in GeoTIFF format based on intersection points between
#' stream network (i.e. Hydrography90m) and lake geospatial files (e.g. HydroLAKES).
#' The function uses the lake intersection tables from get_lake_intersection.R as
#' input data.
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the stream segment ids of the intersection points between lake and stream network;
#' (i.e., output of get_lake_intersection)
#' @param flow character. The name of the flow accumulation column used for
#' sorting the intersection table;
#' i.e. either flow_accu (flow accumulation value at intersection point pixel),
#' flow_accu_max (maximum flow accumulation value of 3 x 3 neighboring pixels),
#' @param lake_id character. The name of the column containing lake ids;
#' (i.e., output "lake_ID" of get_lake_intersection)
#' flow_accu_mean (mean flow accumulation value of 3 x 3 neighboring pixels).
#' Default is flow_accu_mean
#' @param n integer. Number of intersection points used for lake catchment delineation;
#' e.g. n=1 equals first row of intersection table and lake outlet;
#' n=20 equals first 20 rows of intersection table; Default is all.
#' @param direction character. Full path to Hydrography90m flow direction tif file
#' @param lake_basin character. Full path to output catchment tif files
#' @param n_cores integer. Number of cores used in parallelization; Default is one.
#' @param quiet logical. If FALSE, the standard output will be printed.;
#' Default is TRUE.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @export
#'
#' @note
#' For the function to work we need the output of the function get_lake_intersection
#' that are the lake_reference{ID}.txt and intersection table (i.e. coord_lake{ID}.txt)
#'
#' @author Jaime Garcia Marquez, Thomas Tomiczek
#'
#' @references
#' add reference manual html here
#' https://grass.osgeo.org/grass82/manuals/
#'
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' data <- fread(paste0(my_directory,
#'                        "/hydrography90m_test_data",
#'                        "/coord_lake_1.txt"),
#'                       header = TRUE)
#'
#' direction <- (paste0(my_directory,
#'                     "/hydrography90m_test_data",
#'                   "/direction_1264942.tif"))
#'
#' catch <- (paste0(my_directory,
#'                    "/hydrography90m_test_data/"))
#'
#' get_lake_catchment(data, direction = direction, lake_basin = catch)



get_lake_catchment <- function(data, flow = "flow_accu_mean",
                               lake_id = "lake_ID", n = "all", direction,
                               lake_basin, n_cores = 1, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  lake_dat <- as.data.table(data)
  # Remove duplicated rows across entire data frame
  lake_dat <- lake_dat[!duplicated(lake_dat), ]
  #  sort intersection table
  lake_dat <- lake_dat[order(-get(flow))]

  if (n == "all") {
    lake_dat <- lake_dat
  } else {
    # select the n number or all intersection points
    lake_dat <- lake_dat[1:n, ]
  }

  # Create random string
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  lak_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".txt")
  ## write to tempory file to convert to txt file
  fwrite(lake_dat, lak_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    # Call the external .sh script extract_ids() containing the gdal function
    processx::run(system.file("sh", "get_lake_catchment.sh", package = "hydrographr"),
                  args = c(lak_tmp_path, lake_id, direction, tempdir(), lake_basin, n_cores),
                  echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_lak_tmp_path <- fix_path(lak_tmp_path)
    wsl_direction <- fix_path(direction)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_lake_basin <- fix_path(lake_basin)
    wsl_sh_file <- fix_path(
                            system.file("sh", "get_lake_catchment.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "get_lake_catchment.bat",
                              package = "hydrographr"),
                  args = c(wsl_lak_tmp_path, lake_id, wsl_direction, wsl_tmp_path,
                           wsl_lake_basin, n_cores, wsl_sh_file, echo = !quiet))

  }
  # Return message
  print(paste0("Lake catchment tif files are stored under", lake_basin))

}
