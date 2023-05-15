#' @title Get Hydrography90m regional unit IDs
#'
#' @description Identifies the IDs of the regional units within the Hydrography90m data
#' in which the input points are located. The IDs are required to then download
#' the data using download_tiles().
#' Input is a point data frame.
#'
#' @param data a data.frame or data.table with lat/lon coordinates in WGS84.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#' @author Afroditi Grigoropoulou
#'
#' @references
#' \url{https://gdal.org/programs/gdallocationinfo.html}
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Read the species data
#' species_occurence <- read.table(paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/spdata_1264942.txt"),
#'                               header = TRUE)
#'
#' # Get the regional unit ID
#' get_regional_unit_id(species_occurence, lon = "longitude",
#'                     lat = "latitude")


# provide points as an input and get the regional units
# where the points belong (without the full extent)

get_regional_unit_id <- function(data, lon, lat, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if lon, lat, column names
  # are character vectors
  for (name in  c(lon, lat)) {
    if (!is.null(name))
      if (!is.character(name))
        stop(paste0("Column name ", name, " is not a character vector."))
  }

  # Check if lon, lat, column names exist
  for (name in c(lon, lat)) {
    if (!is.null(name))
      if (is.null(data[[name]]))
        stop(paste0("Column name '", name, "' does not exist."))
  }

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Increase time to allow downloading the reg unit file
  options(timeout = max(300, getOption("timeout")))

  # global file of regional units ids
  reg_unit_file <- paste0(tempdir(), "/regional_unit_ovr.tif")

  # If the required file does not already exist,
  # download it in the tempdir()
  if (!file.exists(reg_unit_file)) {
    print("Downloading the global regional unit file")
    download.file("https://drive.google.com/uc?export=download&id=1ykV0jRCglz-_fdc4CJDMZC87VMsxzXE4&confirm=t",
                  destfile = reg_unit_file, mode = "wb")

  }

  # Create random string to attach to the file name of the temporary
  # output coordinates and input ids file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  # Select columns with lon/lat coordinates
  coord <- data %>%
    select(matches(c(lon, lat)))

  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".txt")

  ## Note:Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # Path for tmp regional unit ids text file
  ids_tmp_path <- paste0(tempdir(), "/reg_unit_ids", rand_string, ".txt")

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    processx::run(system.file("sh", "get_regional_unit_id.sh",
                  package = "hydrographr"),
      args = c(coord_tmp_path, lon, lat,
               reg_unit_file,  ids_tmp_path),
      echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_reg_unit_file <- fix_path(reg_unit_file)
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_ids_tmp_path <- fix_path(ids_tmp_path)
    wsl_sh_file <- fix_path(system.file("sh",
                                        "get_regional_unit_id.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "get_regional_unit_id.bat",
                    package = "hydrographr"),
        args = c(wsl_coord_tmp_path, lon, lat,
                 wsl_reg_unit_file, wsl_ids_tmp_path,
                 wsl_sh_file),
        echo = !quiet)
  }
  # Read in the file containing the ids
  data_reg_unit_ids <- fread(ids_tmp_path, keepLeadingZeros = TRUE,
                             header = TRUE, sep = " ")

  # Remove all files in the tmp folder
  file.remove(ids_tmp_path)

  # Return vector of regional unit ids
  data_reg_unit_ids$reg_unit_id

}
