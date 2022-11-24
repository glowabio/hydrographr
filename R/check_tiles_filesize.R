#' Checks the size of single files. It is called and inherits arguments by the function 'download_tiles()'
#'
#' @param variable vector of variable names (character)
#' @param filetype format of the requested file ("tif" or "gpkg")
#' @param tile_id id of the requested tile (character)
#' @param reg_unit_id id of the requested regional unit (character)
#' @param global Should the global file be downloaded or not. TRUE/FALSE, FALSE by default
#' @param valid_varnames valid names of the files available for download (inherited by 'download_tiles()')
#' @param valid_tile_ids valid ids of the tiles available for download (inherited by 'download_tiles()')
#' @param valid_filetypes valid file types of the files available for download (inherited by 'download_tiles()')
#' @param file_size_table_sep lookup table of file sizes (inherited by 'download_tiles()')
#' @importFrom stringr str_split_fixed
#' @export
#'


check_tiles_filesize <- function(variable, filetype = "tif",
                                 tile_id = NULL, reg_unit_id = NULL,
                                 global = FALSE, valid_varnames, valid_tile_ids,
                                 valid_filetypes, file_size_table_sep
) {
  ##############
  # Check if the given variable name is valid
  match.arg(variable, choices = valid_varnames)

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
  }

  else if(global == FALSE) {
    # Get the name of the file
    varname <- paste0(variable, "_", tile_id, ".", filetype)
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

  }
  file_size
}
