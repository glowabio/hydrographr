#' @title Download files of the Environment90m dataset
#'
#' @description The function downloads data of the Environment90m
#' dataset, which is split into 20°x20° tiles. If a tile ID is specified, then
#' the selected layers (variable) will be downloaded. 
#' Multiple regular tiles, e.g. belonging to regional units, can be
#' downloaded in a single request. The tile or regional unit IDs can be
#' obtained using the functions "get_tile_id" and "get_regional_unit_id",
#' respectively.
#' The files will be stored locally in a folder architecture, similar as in the
#' data repository, available at
#' \url{https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F}.
#'
#' @param variable character vector of variable names. See Details for all the
#' variable names.
#' @param file_format character. Format of the requested file. Currently only
#' "zip" is supported. See Details.
#' @param tile_id character vector. The IDs of the requested tiles.
#' @param download_dir character. The directory where the files will be
#' downloaded. Default is the working directory.
#' @importFrom tidyr separate
#' @importFrom stringr str_split_fixed str_extract
#' @export
#'
#' @author Afroditi Grigoropoulou, Merret Buurman
#'
#' @references Amatulli G., Garcia Marquez J., Sethi T., Kiesel J.,
#' Grigoropoulou A., Üblacker M., Shen L. & Domisch S. (2022-08-09 )
#' Hydrography90m: A new high-resolution global hydrographic dataset.
#' IGB Leibniz-Institute of Freshwater Ecology and Inland Fisheries.
#' dataset. \url{https://doi.org/10.18728/igb-fred-762.1}
#'
#' @details
#' In the following table you can find all the variables included in the
#' Environment90m dataset. The column "Variable" includes the variable names
#' that should be used as an input in the parameter "variable" of the function.
#' Likewise, the column "File format" contains the input that should be given to
#' the "file_format" parameter.
#' For more details and visualisations of the spatial layers, please refer to TODO
#' \url{https://hydrography.org/hydrography90m/hydrography90m_layers/}.
#'
#''
#'  | **Variable type**  | **Variable subtype** | **Variable name** | **Variable**          | **Unit** | **File format** |
#'  |--------------------|------------------------------------------|-----------------------|----------|-----------------|
#'  | Climate            | Present              | bio1              | bio1                  | ?        | zip / csv       |
#'  | Climate            | Present              | bio2              | bio2                  | ?        | zip / csv       |
#'  | Climate            | Present              | bio3              | bio3                  | ?        | zip / csv       |
#'  | Climate            | Present              | bio<x>            | bio<x>                | ?        | zip / csv       |
#'  | Climate            | Future               | empty! TODO       | ? TODO                | ?        | zip / csv       |
#'  | Land Cover         | c10                  | Carbon ??? TODO   | c10_<year>            | ?        | zip / csv       |
#'  | Land Cover         | c20                  | Carbon ??? TODO   | c20_<year>            | ?        | zip / csv       |
#'  | Land Cover         | c30                  | ? TODO            | ? TODO                | ?        | zip / csv       |
#'  | Land Cover         | c...                 | ? TODO            | ? TODO                | ?        | zip / csv       |
#'  | Soil               | -empty- TODO         | ? TODO            | ? TODO                | ?        | zip / csv       |
#'
#'
#' @md
#' @note
#' If there is an error during the download of a file
#' (more likely in case of files bigger than 3-4GB), you can try to manually
#' download this file by pasting the link that is returned by the error
#' message in your browser.
#'
#' @examples
#' # Download data for two variables in three regular tiles
#' # to the current working directory
#' download_env(variable = c("bio1", "bio1"),
#'                file_format = "zip",
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#'


### TODO:
# Make the file with file sizes
# Examples above!
# Regional Unit Ids?


download_env <- function(variable, file_format = "zip",
                         tile_id = NULL, reg_unit_id = NULL,
                         download_dir = ".") {

  # Introductory steps

  # Set timeout option for download to 4 hours (14400 seconds)
  options(timeout=14400)

  # Download lookup table with the size of each file
  # if it doesn't exist in the tempdir()
  file_size_file <- paste0(tempdir(), "/env90m_paths_file_sizes.txt")
  if (!file.exists(file_size_file)) {
    message('Downloading env90m_paths_file_sizes.txt...')
    # TODO: Replace this URL by the URL of the env file sizes file!
    download.file("https://drive.google.com/uc?export=download&id=1SEkcgGPutP6ZQPvYtzICh_gcGnVgH_uR&confirm=t", # not yet correct!
                  destfile = file_size_file, mode = "wb")

  }

  # Import lookup table with the size of each file
  file_size_table <- fread(file_size_file, sep = ";")
  file_size_table$file_name = basename(file_size_table$file_path)

  # Extract entire lists of possible variable names, 
  # file formats and tile_ids,
  # to check that the requested variable exists

  # Get the valid names of the environment90m variables
  all_varnames <- sort(unique(sub("_[^_]+$", "",
                                    file_size_table$file_name)))

  # Get the valid file_formats of the environment90m variables
  all_file_names <- sort(unique(file_size_table$file_name))

  # Get the valid tile ids of the environment90m
  all_tile_ids <- unique(str_extract(
    file_size_table$file_path, "h[0-9]+v[0-9]+"))

  # Remove NA from list of tile ids:
  all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

  # Compute overall size of download, by iterating
  # over each variable and there over each tile:
  variable_size_sum <- 0

  for (ivar in variable) {

    tile_size_sum <- 0

    for (itile in tile_id) {

      tile_size <- check_tiles_filesize(variable = ivar,
                                        file_format = file_format,
                                        tile_id = itile,
                                        h90m_varnames = all_varnames,
                                        h90m_tile_id = all_tile_ids,
                                        h90m_file_names = all_file_names,
                                        file_size_table = file_size_table)

      tile_size_sum <- tile_size_sum + tile_size

    }

    variable_size_sum <- tile_size_sum + variable_size_sum
  }

  variable_size_sum

  # Print warning on file size and ask for input from the user
  # arg <- readline(prompt = paste0("Download size is ",
  #                                 round(variable_size_sum / 1000000, 2),
  #                                 " MB. Please type \"y\" if you are ready to smash it\n
  #                                 or \"n\" if you'd rather not to, and then press Enter \n"))
  print(paste0("Download size is ",
         round(variable_size_sum / 1000000, 2),
         " MB."))

  # General path to the download folder in Nimbus resp. GDrive
  nimbus_path <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2F"
  gdrive_path <- "https://drive.google.com/uc?export=download&id="

  # Use README file as a test to check if Nimbus is up.
  server_url <- tryCatch(
    {
      download.file(paste0(nimbus_path, "README/README.txt"),
                    destfile = paste0(download_dir, "/README.txt"), mode = "wb")
      server_url <- nimbus_path
      server_url
    },
    warning = function(c) {
      # Get gdrive file id of the README.txt file
      readme_id <- file_size_table[
        file_size_table$file_name == "README.txt", ]$file_id
      # Download README.txt file
      download.file(paste0(gdrive_path, readme_id),
                    destfile = paste0(download_dir, "/README.txt"), mode = "wb")
      server_url <- gdrive_path
      server_url
    },
    error = function(c) {
      server_url <- gdrive_path
      server_url
    }
  )

  for (ivar in variable) {
    for (itile in tile_id) {

      download_tiles_base(variable = ivar, file_format = file_format,
                          tile_id = itile, global = FALSE,
                          download_dir = download_dir,
                          file_size_table = file_size_table,
                          server_url = server_url
      )
    }
  }
  cat("Please cite the Hydrography90m publication:\n
      Amatulli, G., Garcia Marquez, J., Sethi, T., Kiesel, J., Grigoropoulou, A.,
      Üblacker, M. M., Shen, L. Q., and Domisch, S.: Hydrography90m: a new
      high-resolution global hydrographic dataset, Earth Syst. Sci. Data, 14,
      4525–4550, https://doi.org/10.5194/essd-14-4525-2022, 2022.")
  # TODO: Adapt citation!
}
