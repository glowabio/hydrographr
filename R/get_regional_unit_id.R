#' Adds sub-catchment and/or basin IDs to a dataframe of points
#'
#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column as character string
#' @param lat Name of the latitude column as character string
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#'


# provide points as an input and get the regional units
# where the points belong (without the full extent)

get_regional_unit_id <- function(data, lon, lat, quiet = TRUE) {

  system <- get_os()
  # global file of regional units ids
  reg_unit_file <- system.file("data", "regional_unit_ovr.tif",
                               package = "hydrographr")

  # Create random string to attach to the file name of the temporary
  # output coordinates and input ids file
  rand_string <- stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
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

  if (system == "linux" | system == "osx"){

    run(system.file("sh", "get_regional_unit_id.sh",
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

    run("C:/Users/maria/Documents/Glowabio/Code/hydrographr/inst/bat/get_regional_unit_id.bat",
      #system.file("bat", "get_regional_unit_id.bat",
         #                           package = "hydrographr"),
                        args = c(wsl_coord_tmp_path, lon, lat,
                                 wsl_reg_unit_file, wsl_ids_tmp_path,
                                 wsl_sh_file),
                        echo = !quiet)
  }
  # Read in the file containing the ids
  data_reg_unit_ids <- fread(paste0(tempdir(), "/reg_unit_ids", rand_string, ".txt"),
                             keepLeadingZeros = TRUE, header = TRUE, sep = " ")

  # Remove all files in the tmp folder
  file.remove(ids_tmp_path)

  # Return vector of regional unit ids
  return(data_reg_unit_ids$reg_unit_id)



  #  # To get tile ids based on the given extent of a study area:


}




