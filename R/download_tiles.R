#' Downloads multiple files from Nimbus by calling the function download_tiles_base in a loop.
#'
#' @param variable vector of variable names (character)
#' @param filetype format of the requested file ("tif" or "gpkg")
#' @param tile_id id of the requested tile (character)
#' @param reg_unit_id id of the requested regional unit (character)
#' @param global Should the global file be downloaded or not. TRUE/FALSE, FALSE by default
#' @param download_path The path where the files will be downloaded
#' @importFrom tidyr separate
#' @importFrom stringr str_split_fixed str_extract
#' @export
#'


download_tiles <- function(variable, filetype = "tif",
                           tile_id = NULL, reg_unit_id = NULL,
                           global = FALSE, download_path = ".") {

  # Introductory steps

  # Import lookup table with the size of each file
  file_size_table <- fread(system.file("data", "hydrography90m_file_sizes.txt", package = "hydrographr"))

  # Separate the table to get the names of the hydrography variables
  file_size_table_sep <- separate(
    data = file_size_table,
    col = path,
    into = c("grass_module", "foldername", "varname_tile"),
    sep = "/",
    fill = "left",
  )

  # Get the valid names of the hydrography variables
  # to check that the requested variable exists
  valid_varnames <- sort(unique(sub("_[^_]+$", "",
                                    file_size_table_sep$varname_tile)))
  # Get the valid filetypes of the hydrography variables
  # to check that the requested variable exists
  valid_filetypes <- sort(unique(file_size_table_sep$varname_tile))

  # Sizes of the global files
  file_size_table_global <- file_size_table[grep("global", path)]
  # Sizes of the rest of the files
  file_size_table <- file_size_table[!grep("global", path)]

  # Get the valid tile ids of the hydrography
  # to check that the requested tile exists
  valid_tile_ids <- unique(str_extract(
    file_size_table$path, "h[0-9]+v[0-9]+"))

  valid_tile_ids <- valid_tile_ids[!is.na(valid_tile_ids)]

  # General path to the download folder in Nimbus
  nimbus_path <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"

  variable_size_sum <- 0
  for (ivar in variable) {
    tile_size_sum <- 0

    for (itile in tile_id) {

      tile_size <- check_tiles_filesize(variable = ivar,
                                        filetype = filetype,
                                        tile_id = itile, reg_unit_id = reg_unit_id,
                                        global = global, valid_varnames = valid_varnames,
                                        valid_tile_ids = valid_tile_ids,
                                        valid_filetypes = valid_filetypes,
                                        file_size_table_sep = file_size_table_sep)

      tile_size_sum <- tile_size_sum + tile_size

    }

    variable_size_sum <- tile_size_sum + variable_size_sum

  }


  # Print warning on file size and ask for input from the user
  arg <- readline(prompt=paste0("Download size is ",
                                round(variable_size_sum / 1000000, 2),
                                " MB. Please type \"y\" if you are ready to smash it or \"n\" if you'd rather not to, and then press Enter \n"))

  #include trycatch to limit it to y/n and try again if other input is given!
  if (arg == "y") {

    for (ivar in variable) {
      for (itile in tile_id) {

        download_tiles_base(variable = ivar, filetype = filetype,
                            tile_id = itile, reg_unit_id = reg_unit_id,
                            global = global, download_path = download_path,
                            valid_varnames = valid_varnames,
                            valid_tile_ids = valid_tile_ids,
                            valid_filetypes = valid_filetypes,
                            file_size_table_sep = file_size_table_sep,
                            nimbus_path = nimbus_path)
      }
    }
  }
}
