#' Get tile id when the input is a point dataframe

#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
#' @importFrom data.table fread
#' @importFrom processx run


#' @export
#'
#'
# Get tile id when the input is a point data frame
get_tile_id <- function(data, lon, lat) {


  # If the required global regional unit file does not already exist,
  # download it in the package folder
  if (!file.exists(system.file("data", "regional_unit_ovr.tif",
                               package = "hydrographr"))) {
    download_tiles(variable = "regional_unit", global=TRUE,
                   download_path = system.file("data", "regional_unit_ovr.tif",
                                   package = "hydrographr"))

    }

  reg_un <- get_regional_unit_id(data, lon, lat)

  lookup <- fread(system.file("data", "lookup_tile_regunit.txt",
                               package = "hydrographr"))

  # Find the tile ID in the lookup table based on the regional unit id
  tile_id <- sort(unique(lookup[lookup$reg_unit %in% reg_un, tile]))
  tile_id

}
