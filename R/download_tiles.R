#' Downloads multiple files from Nimbus by calling the function download_tiles_base in a loop.
#'
#' @param variable vector of variable names (character)
#' @param filetype format of the requested file ("tif" or "gpkg")
#' @param tile_id id of the requested tile (character)
#' @param reg_unit_id id of the requested regional unit (character)
#' @param global Should the global file be downloaded or not. TRUE/FALSE, FALSE by default
#' #' @param download_path The path where the files will be downloaded
#' @export
#'


download_tiles <- function(variable, filetype = "tif",
                           tile_id = NULL, reg_unit_id = NULL,
                           global = FALSE, download_path = ".") {

  for (ivar in variable) {
    for (itile in tile_id) {
      download_tiles_base(variable = ivar, filetype = filetype,
                          tile_id = itile, reg_unit_id = reg_unit_id,
                          global = global, download_path = download_path)
    }
  }
}


