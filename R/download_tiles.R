#' Downloads multiple files from Nimbus
#' by calling the function download_tiles_base in a loop.
#'
#' @param variable vector of variable names (character)
#' @param filetype format of the requested file ("tif" or "gpkg")
#' @param tile_id id of the requested tile (character)
#' @param reg_unit_id id of the requested regional unit (character)
#' @param global Should the global file be downloaded or not.
#' TRUE/FALSE, FALSE by default
#' @param download_path The path where the files will be downloaded
#' @importFrom tidyr separate
#' @importFrom stringr str_split_fixed str_extract
#' @export
#'


download_tiles <- function(variable, filetype = "tif",
                           tile_id = NULL, reg_unit_id = NULL,
                           global = FALSE, download_path = ".") {

  # Introductory steps

  # Download lookup table with the size of each file
  # if it doesn't exist in the tempdir()
  file_size_file <- paste0(tempdir(), "/hydrography90m_paths_file_sizes.txt")
  if (!file.exists(file_size_file)) {
    download.file("https://drive.google.com/uc?export=download&id=1SEkcgGPutP6ZQPvYtzICh_gcGnVgH_uR&confirm=t",
                  destfile = file_size_file)

  }

  # Import lookup table with the size of each file
  file_size_table <- fread(file_size_file, sep = ";")

  # Separate the table to get the names of the hydrography90m variables
  file_size_table_sep <- separate(
    data = file_size_table,
    col = file_path,
    into = c("grass_module", "foldername", "varname_tile"),
    sep = "/",
    fill = "left",
  )

  file_size_table_sep$grass_module[
    is.na(file_size_table_sep$grass_module)] <- ""
  # Get the valid names of the hydrography variables
  # to check that the requested variable exists
  valid_varnames <- sort(unique(sub("_[^_]+$", "",
                                    file_size_table_sep$varname_tile)))
  # Get the valid filetypes of the hydrography variables
  # to check that the requested variable exists
  valid_filetypes <- sort(unique(file_size_table_sep$varname_tile))

  # Get the valid tile ids of the hydrography
  # to check that the requested tile exists
  valid_tile_ids <- unique(str_extract(
    file_size_table$file_path, "h[0-9]+v[0-9]+"))

  valid_tile_ids <- valid_tile_ids[!is.na(valid_tile_ids)]

  variable_size_sum <- 0

  for (ivar in variable) {


    if (global == TRUE) {
      tile_id <-  "_ovr"

    } else if (global == FALSE) {

      if (ivar == "regional_unit") {

        tile_id <- as.character(reg_unit_id)

      } else {
        tile_id <- tile_id
      }

    }


    tile_size_sum <- 0

    for (itile in tile_id) {

      tile_size <- check_tiles_filesize(variable = ivar,
                                        filetype = filetype,
                                        tile_id = itile,
                                        global = global,
                                        valid_varnames = valid_varnames,
                                        valid_tile_ids = valid_tile_ids,
                                        valid_filetypes = valid_filetypes,
                                        file_size_table_sep = file_size_table_sep)

      tile_size_sum <- tile_size_sum + tile_size

    }

    variable_size_sum <- tile_size_sum + variable_size_sum
  }

  variable_size_sum

  # Print warning on file size and ask for input from the user
  arg <- readline(prompt = paste0("Download size is ",
                                  round(variable_size_sum / 1000000, 2),
                                  " MB. Please type \"y\" if you are ready to smash it\n
or \"n\" if you'd rather not to, and then press Enter \n"))

  if (arg == "y") {


    # The argument 'server_path' of the download_tiles_base() function controls
    # the server from which the files will be downloaded

    # General path to the download folder in Nimbus
    nimbus_path <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"
    # General path to the download folder in GDrive
    gdrive_path <- "https://drive.google.com/uc?export=download&id="

    # Use README file as a test to check if Nimbus is up.
    server_path <- tryCatch(
      {
        download.file(paste0(nimbus_path, "README/README.txt"),
                      destfile = paste0(download_path, "/README.txt"))
        server_path <- nimbus_path
        server_path
      },
      warning = function(c) {
        # Get gdrive file id of the README.txt file
        readme_id <- file_size_table_sep[varname_tile == "README.txt", ]$file_id
        # Download README.txt file
        download.file(paste0(gdrive_path, readme_id),
                      destfile = paste0(download_path, "/README.txt"))
        server_path <- gdrive_path
        server_path
      },
      error = function(c) {
        server_path <- gdrive_path
        server_path
      }
    )

    for (ivar in variable) {
      for (itile in tile_id) {

        download_tiles_base(variable = ivar, filetype = filetype,
                            tile_id = itile, global = global,
                            download_path = download_path,
                            file_size_table_sep = file_size_table_sep,
                            server_path = server_path
        )
      }
    }
  }
}
