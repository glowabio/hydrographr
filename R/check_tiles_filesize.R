#' Check the size of single files before downloading.
#' It is called and inherits arguments by the function 'download_tiles()'
#'
#' @param variable character vector of variable names.
#' @param file_format character. Format of the requested file ("tif" or "gpkg").
#' @param tile_id character. The ID of the requested tile or regional unit.
#' @param global logical. If TRUE, the global extent file is downloaded.
#' Default is FALSE.
#' @param h90m_varnames character vector. the valid names of the hydrography90m
#' files available for download, (inherited by 'download_tiles()').
#' @param h90m_tile_id character vector. The valid IDs of the hydrography90m.
#' regular tiles available for download (inherited by 'download_tiles()').
#' @param h90m_file_formats character vector. The valid file types of the files
#' available for download (inherited by 'download_tiles()').
#' @param file_size_table_sep data.frame. Lookup table including file names
#' and sizes (inherited by 'download_tiles()').
#'
#' @importFrom stringr str_split_fixed
#' @keywords internal
#'



check_tiles_filesize <- function(variable, file_format = "tif",
                                 tile_id = NULL, reg_unit_id = NULL,
                                 global = FALSE, h90m_varnames, h90m_tile_id,
                                 h90m_file_formats, file_size_table_sep) {

  # Check if the given variable name is valid
  match.arg(variable, choices = h90m_varnames)

  # Check that the requested file_format is a tif or gpkg
  match.arg(file_format, choices = c("tif", "gpkg"))

  # Regional unit case

  if (global == TRUE) {
    match.arg(file_format, choices = "tif")

  } else if (global == FALSE) {
    # Regional unit case, test reg_unit ids
    if (variable == "regional_unit") {

      match.arg(tile_id, choices = as.character(
        c(seq(1:116), seq(from = 150, to = 200))))

    } else if (variable != "regional_unit") {

      # Check tile_id argument for the rest of the variables
      match.arg(tile_id, choices = h90m_tile_id)

    }
  }


  # Get the name of the file
  varname <- ifelse(global == TRUE, paste0(variable, "_ovr.", file_format),
                    paste0(variable, "_", tile_id, ".", file_format))

  # Find valid file_formats for the requested variable
  # to check that the requested file_format exists
  valid_file_format_var <- unique(
    str_split_fixed(
      h90m_file_formats[grep(paste0(variable, "_"), h90m_file_formats)],
      "\\.", 2)[, 2])

  # Check that the requested file_format is among the valid ones
  match.arg(file_format, choices = valid_file_format_var)

  # Grep the filesize of the requested file from the lookup table
  file_size <- file_size_table_sep[varname_tile == varname, ]$file_size

  file_size
}
