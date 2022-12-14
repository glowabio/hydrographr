#' Adds sub-catchment and/or basin IDs to a dataframe of points
#'
#' @param data Data.frame with lat/lon columns
#' @param lon Name of the longitude column (WGS 84)
#' @param lat Name of the latitude column (WGS 84)
#' @param subcatchment_path Full path to the sub-catchment ID .tif layer
#' @param basin_path Full path to the basin ID .tif layer
#' @param quiet If FLASE, process is printed
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#'
extract_ids <- function(data, lon = character(0), lat = character(0),
                        subcatchment_path = NULL, basin_path = NULL,
                        quiet = TRUE) {

  system <- get_os()

  # Create random string to attach to the file name of the temporary
  # points_dataset.txt and ids.txt file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Select columns with lon/lat coordinates
  coord <- data %>%
    select(matches(c(lon, lat)))
  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".txt")
  ## Note:Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")
  # Path for tmp ids.txt file
  ids_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".txt")


  if (system == "linux" || system == "osx") {

    # Convert null arguments to 0 so that bash can evaluate the variables
    subc_path <- ifelse(is.null(subcatchment_path), 0, subcatchment_path)
    bas_path <- ifelse(is.null(basin_path), 0, basin_path)

    # Call the external .sh script extract_ids() containing the gdal function
    run(system.file("sh", "extract_ids.sh", package = "hydrographr"),
        args = c(coord_tmp_path, lon, lat, subc_path, bas_path, tempdir(),
                 ids_tmp_path),
        echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_subc_path <- ifelse(is.null(subcatchment_path), 0,
                            fix_path(subcatchment_path))
    wsl_bas_path <- ifelse(is.null(basin_path), 0, fix_path(basin_path))
    wsl_tmp_path <- fix_path(tempdir())
    wsl_ids_tmp_path <- fix_path(ids_tmp_path)
    wsl_sh_file <- fix_path(
      system.file("sh", "extract_ids.sh",
                  package = "hydrographr"))

    run(system.file("bat", "extract_ids.bat",
                                package = "hydrographr"),
                    args = c(wsl_coord_tmp_path, lon, lat, wsl_subc_path,
                             wsl_bas_path, wsl_tmp_path, wsl_ids_tmp_path,
                             wsl_sh_file),
                    echo = !quiet)

  }
  # Read in the file containing the ids
  data_ids <- fread(paste0(tempdir(),  "/ids_", rand_string, ".txt"),
                    keepLeadingZeros = TRUE, header = TRUE, sep = " ")

  # Remove all files in the tmp folder
  file.remove(coord_tmp_path, ids_tmp_path)

  # Return data frame
  return(data_ids)

}