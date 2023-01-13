#' Identifies the IDs of the regional units within the Hydrography90m data
#' in which the input points are located. The IDs are required to then download
#' the data using download_tiles().
#' Input is a point data frame.
#'
#' @param data a data.frame or data.table with lat/lon coordinates in WGS84.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#' @examples
#' library(hydrographr)
#' library(data.table)
#'
#' # Specify the working directory of the test data
#' DATADIR <- "path/to/test_data"
#'
#' # Download the test data
#' download_test_data(DATADIR)
#'
#' # Read the species data
#' species <- fread(paste0(DATADIR, "/spdata_1264942.txt"))
#'
#' # Get the regional unit ID
#' my_IDs <- get_regional_unit_id(species, lon="longitude", lat="latitude")
#'
#'
#' @author Afroditi Grigoropoulou
#'


# provide points as an input and get the regional units
# where the points belong (without the full extent)

get_regional_unit_id <- function(data, lon, lat, quiet = TRUE) {

  system <- get_os()

  # Make bash scripts executable
  make_sh_exec()

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

  if (system == "linux" || system == "osx") {

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
