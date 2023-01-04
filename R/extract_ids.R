#' Extract sub-catchment and/or basin IDs for a point location
#'
#' Extracts the ID value of the basin and/or sub-catchment raster layer at a
#' given point location.
#'
#' @param data A data.frame or data.table with lat/lon coordinates in WGS84.
#' @param lon Column name of longitude coordinates as a character vector.
#' @param lat Column name of latitude coordinates as a character vector.
#' @param site_id  Column name of the unique ID as a character vector.
#' Alternatively, an ID for the point location can be defined.
#' @param basin_path Full path to the basin ID .tif layer on disk.
#' @param subc_path Full path to the sub-catchment ID .tif layer on disk.
#' @param quiet Whether the standard output will be printed or not (deafult
#' is TRUE).
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @export
#'
#' @details
#' For the extraction of a value at a given point location from the basin
#' and/or sub-catchment raster layer of the Hydrography90m dataset, the GDAL
#' function 'gdallocationinfo' is used. The point locations have to be defined
#' by coordinates of the WGS84 reference system. The function can also be used
#' to extract any value from a given raster layer with a WGS84 projection, such
#' as e.g. environmental information that is stored in the input raster file.
#'
#' @note
#' Duplicated rows will be removed.
#'
#' @author Afroditi Grigoropoulou, Maria Üblacker
#'
#' @references
#' \url{https://gdal.org/programs/gdallocationinfo.html}
#'
#' @examples
#' # Download test data into temporary R folder
#' download_test_data(tempdir())
#'
#' # Load occurrence data
#' species_occurrence <- read.table(
#' paste0(tempdir(), "/hydrography90m_test_data/spdata_1264942.txt"),
#' header = TRUE)
#'
#' # Define full path to the basin and sub-catchments raster layer
#' basin_rast <- paste0(tempdir(),
#'                      "/hydrography90m_test_data/basin_1264942.tif")
#' subc_rast <- paste0(tempdir(),
#'                     "/hydrography90m_test_data/basin_1264942.tif")
#'
#' # Extract basin and sub-catchment IDs from the Hydrography90m layers
#' hydrography90m_ids <- extract_ids(data = species_occurrence,
#'                                   lon = "longitude",
#'                                   lat = "latitude",
#'                                   site_id = "occurrence_id",
#'                                   subc_path = subc_rast,
#'                                   basin_path = basin_rast)
#'
#' # Show the output table
#' hydrography90m_ids


extract_ids <- function(data, lon, lat, site_id = NULL, basin_path = NULL,
                        subc_path = NULL, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if lon, lat, side_id, basin_id, and subc_id column names
  # are character vectors
  for (name in  c(lon, lat, site_id)) {
    if (!is.null(name))
        if (!is.character(name))
          stop(paste0("Column name ", name, " is not a character vector."))
  }

  # Check if lon, lat, site_id, basin_id, and subc_id column names exist
  for (name in c(lon, lat, site_id)) {
    if (!is.null(name))
      if (is.null(data[[name]]))
        stop(paste0("Column name '", name, "' does not exist."))
  }

  # Check if paths exists
  for (path in c(basin_path, subc_path)) {
    if (!file.exists(path))
      stop(paste0("File path: ", path, " does not exist."))
  }

  # Check if basin_path and subc_path ends with .tif
  for (path in c(basin_path, subc_path)) {
    if (!endsWith(path, ".tif"))
      stop(paste0("File path: ", path, " does not end with .tif"))
  }

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")


  # Create random string to attach to the file name of the temporary
  # points_dataset.txt and ids.txt file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Select columns with lon/lat coordinates
  if (is.null(site_id)) {
    coord <- data %>%
      select(matches(c(lon, lat)))
    # Remove duplicated rows across entire data frame
    coord <- coord[!duplicated(coord), ]

  } else {
    coord <- data %>%
      select(matches(c(lon, lat, site_id)))
    # Remove duplicated rows across entire data frame
    coord <- coord[!duplicated(coord), ]

  }
  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".txt")
  ## Note:Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")
  # Path for tmp ids.txt file
  ids_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".txt")

  # Check operating system
  system <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (system == "linux" || system == "osx") {

    # Convert null arguments to 0 so that bash can evaluate the variables
    subc_path <- ifelse(is.null(subc_path), 0, subc_path)
    bas_path <- ifelse(is.null(basin_path), 0, basin_path)

    # Call the external .sh script extract_ids() containing the gdal function
    processx::run(system.file("sh", "extract_ids.sh", package = "hydrographr"),
                  args = c(coord_tmp_path, lon, lat, subc_path, bas_path,
                           tempdir(), ids_tmp_path),
                  echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_subc_path <- ifelse(is.null(subc_path), 0,
                            fix_path(subc_path))
    wsl_bas_path <- ifelse(is.null(basin_path), 0, fix_path(basin_path))
    wsl_tmp_path <- fix_path(tempdir())
    wsl_ids_tmp_path <- fix_path(ids_tmp_path)
    wsl_sh_file <- fix_path(
      system.file("sh", "extract_ids.sh",
                  package = "hydrographr"))

    processx::run(system.file("bat", "extract_ids.bat",
                              package = "hydrographr"),
                  args = c(wsl_coord_tmp_path, lon, lat, wsl_subc_path,
                           wsl_bas_path, wsl_tmp_path, wsl_ids_tmp_path,
                           wsl_sh_file, echo = !quiet))

  }
  # Read in the file containing the ids
  data_ids <- fread(paste0(tempdir(),  "/ids_", rand_string, ".txt"),
                    keepLeadingZeros = TRUE, header = TRUE, sep = " ")

  # Remove all files in the tmp folder
  file.remove(coord_tmp_path, ids_tmp_path)

  # Return data frame
  return(data_ids)

}
