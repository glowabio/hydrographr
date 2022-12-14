
#' Identifies the ids of the tiles in which the given points are located.
#' Input is a point data frame.
#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
#' @importFrom data.table fread
#' @author Afroditi Grigoropoulou
#' @export
#'
#'
# Get tile id when the input is a point data frame

get_tile_id <- function(data, lon, lat) {

  reg_un <- get_regional_unit_id(data, lon, lat)


  lookup_file <- paste0(tempdir(), "/lookup_tile_regunit.txt")

  if (!file.exists(lookup_file)) {
    download.file("https://drive.google.com/uc?export=download&id=1deKhOEjGgvUXPwivYyH99hgHlJV7OgUv&confirm=t",
                  destfile = lookup_file,
                  quiet = FALSE)

  }

  lookup <- fread(lookup_file)

  # Find the tile ID in the lookup table based on the regional unit id
  tile_id <- sort(unique(lookup[lookup$reg_unit %in% reg_un, tile]))
  tile_id


}
