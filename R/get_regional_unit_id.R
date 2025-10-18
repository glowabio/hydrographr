#' @title Get Hydrography90m regional unit IDs
#'
#' @description Given the coordinates of input points (in WGS84), the function
#' identifies the IDs of the regional units of the Hydrography90m in which the
#' points are located. Input is a point data frame. The regional units refer to
#' non-interrupted basins (as opposed to the 20°x20° tiles).
#' These IDs can then be used to download the Hydrography90m regional unit
#' raster mask(s) using \code{\link{download_tiles()}}.
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the longitude / latitude coordinates in WGS84.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @param tempdir character. The directory to be used as temp folder. Optional.
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
#' species_occurrence <- read.table(paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/spdata_1264942.txt"),
#'                               header = TRUE)
#'
#' # Get the regional unit ID
#' get_regional_unit_id(species_occurrence, lon = "longitude",
#'                     lat = "latitude")


# provide points as an input and get the regional units
# where the points belong (without the full extent)

get_regional_unit_id <- function(data, lon, lat, quiet = TRUE, tempdir = NULL) {

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

  # Use tempdir passed by user, or the default one:
  if (is.null(tempdir)) { tempdir <- tempdir() }

  # global file of regional units ids
  reg_unit_file <- paste0(tempdir, "/regional_unit_ovr.tif")

  # define download paths (two options):
  nimbus_url_base <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"
  nimbus_url_full <- paste0(nimbus_url_base, "global&files=regional_unit_ovr.tif")
  gdrive_url_base <- "https://drive.google.com/uc?export=download&id="
  gdrive_url_full <- paste0(gdrive_url_base, "1ykV0jRCglz-_fdc4CJDMZC87VMsxzXE4&confirm=t")

  # If the required file does not already exist,
  # download it into the tempdir()
  if (file.exists(reg_unit_file)) {
    message(paste("Will use this file (already downloaded):", reg_unit_file))
  } else {
    message(paste0("Downloading the global regional unit file to ",
                   reg_unit_file, "..."))
    # first try Nimbus, then GDrive:
    tryCatch(
      {
        download.file(nimbus_url_full, destfile = reg_unit_file, mode = "wb")
      },
      warning = function(warn) {
        message(paste("Download failed, reason: ", warn[1]))
        download.file(gdrive_url_full, destfile = reg_unit_file, mode = "wb")
      },
      error = function(err) {
        message(paste("Download failed, reason: ", err[1]))
        download.file(gdrive_url_full, destfile = reg_unit_file, mode = "wb")
      }
    )
  }

  # Adding a check for correct file size and contents.
  # Why?
  # The correct file is 123262363 bytes on disk (123,3 MB).
  # When downloading from GDrive, you may receive a file
  # sized 2433 bytes (2,4 kB), containing text about a virus scan:
  #  > "Google Drive - Virus scan warning [...]
  #  > Google Drive can't scan this file for viruses. regional_unit_ovr.tif
  #  > - (118M) is too large for Google to scan for viruses.
  #  > Would you still like to download this file?"

  if (file.size(reg_unit_file) < 10000) { # bytes

    gdrive_url_base <- "https://drive.google.com/uc?export=download&id="
    gdrive_url_full <- paste0(gdrive_url_base, "1ykV0jRCglz-_fdc4CJDMZC87VMsxzXE4&confirm=t")

    # Checking the actual text content (only first 10 lines):
    first_lines <- readLines(reg_unit_file, warn = FALSE)[1:10]
    if (any(grepl("still like to download", first_lines, fixed = TRUE))) {
      msg <- paste("Downloading the file 'regional_unit_ovr.tif' went wrong,",
                   "as you manually need to confirm skipping the virus check.",
                   "\nPlease, download manually at", gdrive_url_full, "or",
                   nimbus_url_full, "and store to", reg_unit_file,
                   ". Stopping.")
      stop(msg)
      # TODO: Need to write unit test for these cases - a user ran into a stupid bug
      # here that was caused by the nimbus_url_full not being defined, as the file
      # had already been downloaded.

    } else {
      # In case the text is in a different locale and does not contain those
      # exact words, still warn the user:
      msg <- paste0("Downloading the file 'regional_unit_ovr.tif' probably",
                    " went wrong, it is only ", file.size(reg_unit_file),
                    " bytes.\nIf this function fails, please download",
                    " manually at ", gdrive_url_full, " or ",
                    nimbus_url_full, " and store to ", reg_unit_file, ".")
      warning(msg)
    }
  }

  # Create random string to attach to the file name of the temporary
  # output coordinates and input ids file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  # Select columns with lon/lat coordinates
  coord <- data %>%
    select(matches(c(lon, lat)))

  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir, "/coordinates_", rand_string, ".txt")

  ## Note:Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # Path where tmp regional unit ids text file will be written
  ids_tmp_path <- paste0(tempdir, "/reg_unit_ids", rand_string, ".txt")

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

  # If something went wrong, e.g. a corrupted regional_unit_ovr.tif is used,
  # the file is written but contains no ids:
  if (nrow(data_reg_unit_ids) == 0) {
    warning(paste("No regional unit could be extracted. Check whether",
                  "your coordinates are ok, and whether", reg_unit_file,
                  "is a valid raster file."))
  }

  # Remove all files in the tmp folder
  file.remove(ids_tmp_path)

  # Check if zero is in there:
  if (0 %in% data_reg_unit_ids$reg_unit_id) {
    message("The regional units contain zero, so some point(s) were in",
            " the ocean instead of on land.")
  }


  # Return vector of regional unit ids
  data_reg_unit_ids$reg_unit_id

}
