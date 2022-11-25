#' Downloads a single file from https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F.
#' It is called and inherits arguments by the function 'download_tiles()'.
#'
#' @param variable vector of variable names (character)
#' @param filetype format of the requested file ("tif" or "gpkg")
#' @param tile_id id of the requested tile (character)
#' @param reg_unit_id id of the requested regional unit (character)
#' @param global Should the global file be downloaded or not. TRUE/FALSE, FALSE by default
#' @param download_path The path where the files will be downloaded
#' @param valid_varnames valid names of the files available for download (inherited by 'download_tiles()')
#' @param valid_tile_ids valid ids of the tiles available for download (inherited by 'download_tiles()')
#' @param valid_filetypes valid file types of the files available for download (inherited by 'download_tiles()')
#' @param file_size_table_sep lookup table of file sizes (inherited by 'download_tiles()')
#' @param nimbus_path path to the the home download folder in Nimbus (inherited by 'download_tiles()')
#' @importFrom stringr str_split_fixed
#' @export
#'

download_tiles_base <- function(variable, filetype = "tif",
                                tile_id = NULL, reg_unit_id = NULL,
                                global = FALSE, download_path = ".",
                                valid_varnames, valid_tile_ids,
                                valid_filetypes, file_size_table_sep, nimbus_path) {

  if(global == TRUE) {
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

    # Find grass module to build the download link
    # and to create the download destination folder
    grass_module <- file_size_table_sep[varname_tile ==
                                          varname, ]$grass_module
    print(grass_module)
    # Find folder name to build the download link
    # and to create the download destination folder
    foldername <- file_size_table_sep[varname_tile == varname, ]$foldername
    print(foldername)

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
