#' Downloads a single file from Nimbus. It is called by the function download_tiles.
#'
#' @param variable vector of variable names (character)
#' @param filetype format of the requested file ("tif" or "gpkg")
#' @param tile_id id of the requested tile (character)
#' @param reg_unit_id id of the requested regional unit (character)
#' @param global Should the global file be downloaded or not. TRUE/FALSE, FALSE by default
#' @param download_path The path where the files will be downloaded
#' @importFrom tidyr separate
#' @importFrom data.table fread
#' @importFrom tidyr separate
#' @importFrom stringr str_extract str_split_fixed
#' @export
#'

download_tiles_base <- function(variable, filetype = "tif",
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

  #############
  # Check if the given variable name is valid
  match.arg(variable, choices = valid_varnames)

  # General path to the download folder in Nimbus
  nimbus_path <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"

  # Check that the requested filetype is a tif or gpkg
  match.arg(filetype, choices = c("tif", "gpkg"))


  if(variable == "regional_unit") {
    tile_id <- NULL
    if(global == TRUE) {

      reg_unit_id <- NULL
      match.arg(filetype, choices = "tif")

    } else if(global == FALSE) {

      reg_unit_id <- as.character(reg_unit_id)
      match.arg(reg_unit_id, choices = as.character(
        c(seq(1:116), seq(from = 150, to = 200))))
      match.arg(filetype, choices = "tif")

    }
  } else {

    # Check tile_id argument
    match.arg(tile_id, choices = valid_tile_ids)

  }

  if(global == TRUE) {
    foldername <- "global"
    # Get the name of the file
    varname <- paste0(variable, "_ovr.", filetype)

    # Check that the requested filetype is a tif
    # because there are not global gpkg files
    match.arg(filetype, choices = "tif")

    # Grep the filesize of the requested file from the lookup table
    file_size <- file_size_table_global[path == paste0(
      foldername, "/",  varname), size]

    # Print warning on file size and ask for input from the user
    print(paste0("Download size is ", round(file_size / 1000000, 2),
                 " MB. Continue?")) # add yes/no

    # Build download link
    down_link <- paste0(nimbus_path, foldername, "%2F",  varname)

    # Create download directory
    dir.create(paste0(download_path, "/", foldername), showWarnings = FALSE)

    # Download file
    download.file(down_link,
                  destfile=paste0(download_path, "/", foldername, "/", varname))

  }

  else if(global == FALSE) {
    # Get the name of the file
    varname <- paste0(variable, "_", tile_id, ".", filetype)
    print(varname)
    # Find valid filetypes for the requested variable
    # to check that the requested filetype exists
    valid_filetype_var <- unique(
      str_split_fixed(
        valid_filetypes[grep(paste0(variable, "_"), valid_filetypes)],
        "\\.", 2)[,2])

    # Check that the requested filetype is among the valid ones
    match.arg(filetype, choices = valid_filetype_var)

    # Add an argument that moves to the next variable is the previous variable wasn't valid?

    # Find grass module to build the download link
    # and to create the download destination folder
    grass_module <- file_size_table_sep[varname_tile ==
                                          varname, ]$grass_module
    # Find folder name to build the download link
    # and to create the download destination folder
    foldername <- file_size_table_sep[varname_tile == varname, ]$foldername

    # Grep the filesize of the requested file from the lookup table
    file_size <- file_size_table_sep[varname_tile == varname, ]$size

    # Build download link
    down_link <- paste0(nimbus_path, grass_module, "%2F",  foldername,
                        "%2F",  variable, "_", tile_id, ".", filetype)

    # Create download directories
    dir.create(paste0(download_path, "/", grass_module),
               showWarnings = FALSE)
    dir.create(paste0(download_path, "/", grass_module, "/", foldername),
               showWarnings = FALSE)

    # Download file
    download.file(down_link,
                  destfile = paste0(download_path, "/", grass_module, "/",
                                    foldername, "/",  variable, "_", tile_id, ".", filetype))

  }
}

