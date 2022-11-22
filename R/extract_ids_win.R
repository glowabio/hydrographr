#' Join basin and/or sub-catchment IDs with occurrence points
#'
#' Add the column 'subcID' to the dataset in 'dataset_path' and export the
#' data frame in a csv.Coordinates need to be in WS84.
#'
#' @param dataset_path Path to data set text file with the lat long columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
#' @param subc_path Path to the sub-catchment ID .tif layer
#' @param basin_path Path to the basin ID .tif layer
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @importFrom rlang is_missing
#' @export
#'

#'
extract_ids <- function(data, lon, lat, subc_path, basin_path, quiet=TRUE) {

  # Check operating system
  system <- get_os()

  cord <- data %>%
    select(all_of(lon), all_of(lat))

  # Create random string to attach to the file name of the temporary
  # rules .txt file
  rand_string <- stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
  # Path to the tmp text file with the coordinates
  cord_path <- paste0(tempdir(), "/coordinates_", rand_string, ".txt")
  # Path to the tmp output file with the IDs
  ids_path <- paste0(tempdir(), "/ids_", rand_string, ".txt")
  # Write rules as a .txt file to the temporary folder
  fwrite(cord, cord_path, sep = " ", col.names = FALSE)


  if (system == "linux") {

  } else if (system == "windows") {

    # Check if WSL and Ubuntu are installed
    check_wsl()
    # Change paths for WSL
    wsl_subc_path <- fix_path(subc_path)
    wsl_basin_path <- fix_path(basin_path)
    wsl_cord_path <- fix_path(cord_path)
    wsl_ids_path <- fix_path(ids_path)
    #wsl_sh_file <- fix_path(
    #  system.file("sh", "extract_ids.sh",
    #              package = "hydrographr"))

    #system.file("bat", "extract_ids.bat",
    #            package = "hydrographr")

    run("C:/Users/maria/Documents/Glowabio/Code/hydrographr/inst/bat/extract_ids.bat",
        args = c(wsl_cord_path, wsl_subc_path, wsl_ids_path, wsl_sh_file, wsl_basin_path),
        echo = !quiet)

    subc_ids <- fread(ids_path, col.names = "subc_id", keepLeadingZeros = TRUE)
    # Format the ids string
    subc_ids <- subc_ids %>%
      cbind.data.frame(data)


  } else {
    # Stop the function if the OS is macOS
    stop("Sorry, extract_ids() is not implimented for macOS.")
  }

  return(data_ids)

}

run("C:/Users/maria/Documents/Glowabio/Code/hydrographr/inst/bat/extract_ids.bat",
    args = c(wsl_subc_path),
    echo = !quiet)
