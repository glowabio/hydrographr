#' Get tile id when the input is a point dataframe

#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
# ' @param xmin
# ' @param ymin
# ' @param xmax
# ' @param ymax
#' @importFrom data.table fread
#' @importFrom processx run
# ' @importFrom rlang is_missing
# ' @importFrom terra vect ext

#' @export
#'
#'
# Get tile id when the input is a point dataframe
get_tile_id <- function(data, lon, lat) {

  reg_un <- get_regional_unit_id(data, lon, lat)

  #replace this path with system.file
  lookup <- fread(system.file("data", "lookup_tile_regunit.txt",
                               package = "hydrographr"))

  # Find the tile ID in the lookup table based on the regional unit id
  tile_id <- sort(unique(lookup[lookup$reg_unit %in% reg_un, tile]))
  tile_id

}