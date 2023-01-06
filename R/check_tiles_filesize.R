#' Check the size of single files before downloading.
#' It is called and inherits arguments by the function 'download_tiles()'
#'
#' @param variable Vector of variable names (as character)
#' @param filetype Format of the requested file, either "tif" or "gpkg"
#' @param tile_id The ID of the requested tile or regional unit (as character)
#' @param global Logical. Should the global file be downloaded or not.
#' TRUE/FALSE, FALSE by default
#' @param valid_varnames The valid names of the files available for download
#' (inherited by 'download_tiles()')
#' @param valid_tile_ids The valid IDs of the tiles available for download
#' (inherited by 'download_tiles()')
#' @param valid_filetypes The valid file types of the files available for download
#' (inherited by 'download_tiles()')
#' @param file_size_table_sep The lookup table of file sizes
#' (inherited by 'download_tiles()')
#'
#' @importFrom stringr str_split_fixed
#' @keywords internal
#'



check_tiles_filesize <- function(variable, filetype = "tif",
                                 tile_id = NULL, reg_unit_id = NULL,
                                 global = FALSE, valid_varnames, valid_tile_ids,
                                 valid_filetypes, file_size_table_sep) {

  # Check if the given variable name is valid
  match.arg(variable, choices = valid_varnames)

  # Check that the requested filetype is a tif or gpkg
  match.arg(filetype, choices = c("tif", "gpkg"))

  # Regional unit case

  if (global == TRUE) {
    match.arg(filetype, choices = "tif")

  } else if (global == FALSE) {
    # Regional unit case, test reg_unit ids
    if (variable == "regional_unit") {

      match.arg(tile_id, choices = as.character(
        c(seq(1:116), seq(from = 150, to = 200))))

    } else if (variable != "regional_unit") {

      # Check tile_id argument for the rest of the variables
      match.arg(tile_id, choices = valid_tile_ids)

    }
  }


  # Get the name of the file
  varname <- ifelse(global == TRUE, paste0(variable, "_ovr.", filetype),
                    paste0(variable, "_", tile_id, ".", filetype))

  # Find valid filetypes for the requested variable
  # to check that the requested filetype exists
  valid_filetype_var <- unique(
    str_split_fixed(
      valid_filetypes[grep(paste0(variable, "_"), valid_filetypes)],
      "\\.", 2)[, 2])

  # Check that the requested filetype is among the valid ones
  match.arg(filetype, choices = valid_filetype_var)

  # Grep the filesize of the requested file from the lookup table
  file_size <- file_size_table_sep[varname_tile == varname, ]$file_size

  file_size
}
