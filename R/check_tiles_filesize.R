#' Internal function that checks the size of single files before downloading.
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
#' @param h90m_file_names character vector. The names of the files available
#' for download. Used to verify whether the requested format is valid 
#' (inherited by 'download_tiles()').
#' @param file_size_table data.frame. Lookup table including file names
#' and sizes (inherited by 'download_tiles()').
#'
#' @importFrom stringr str_split_fixed
#' @keywords internal
#'



check_tiles_filesize <- function(variable, file_format = "tif",
                                 tile_id = NULL, reg_unit_id = NULL,
                                 global = FALSE, h90m_varnames, h90m_tile_id,
                                 h90m_file_names, file_size_table) {

  # Check if the given variable name is valid
  match.arg(variable, choices = h90m_varnames)

  # Check that the requested file_format is a tif or gpkg
  # Added "zip" for env data!
  match.arg(file_format, choices = c("tif", "gpkg", "zip"))

  # Regional unit case

  if (global == TRUE) {
    match.arg(file_format, choices = "tif")

  } else if (global == FALSE) {
    # TODO: Can we use elif (variable == "regional_unit") here?
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
  filename <- ifelse(global == TRUE, paste0(variable, "_ovr.", file_format),
                    paste0(variable, "_", tile_id, ".", file_format))

  # Find valid file_formats for the requested variable
  # to check that the requested file_format exists.
  # Important: TODO: Do not just split at the dot, as in the future climate
  # variables, we have files like this:
  #   bio18_2071-2100_mpi-esm1-2-hr_ssp585_V.2.1_h04v00.zip
  #   bio18_2071-2100_ukesm1-0-ll_ssp370_V.2.1_h20v00.zip
  #   bio18_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1_h16v10.zip
  # Works, but inelegant:
  valid_file_format_var = c()
  for (filename in h90m_file_names) {
    pieces = str_split(filename, "\\.")[[1]]
    format = pieces[length(pieces)]
    valid_file_format_var <- unique(c(valid_file_format_var, format))
  }

  # Check that the requested file_format is among the valid ones
  match.arg(file_format, choices = valid_file_format_var)

  # Grep the filesize of the requested file from the lookup table
  file_size <- file_size_table[file_size_table$file_name == filename, ]$file_size

  file_size
}
