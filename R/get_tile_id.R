<<<<<<< HEAD
#' Identifies the ids of the tiles in which the given points are located. Input is a point data frame.
=======
#' Get tile id when the input is a point dataframe
>>>>>>> dev_get_tile_id

#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
<<<<<<< HEAD
# ' @param xmin
# ' @param ymin
# ' @param xmax
# ' @param ymax
#' @importFrom data.table fread
#' @importFrom processx run
# ' @importFrom terra vect ext
#' @export
#'
#'
=======
#' @importFrom data.table fread
#' @importFrom processx run
#' @author Afroditi Grigoropoulou
#' @export
#'
#'
# Get tile id when the input is a point data frame
>>>>>>> dev_get_tile_id
get_tile_id <- function(data, lon, lat) {

  reg_un <- get_regional_unit_id(data, lon, lat)

<<<<<<< HEAD
  #replace this path with system.file
  lookup <- fread(system.file("data", "lookup_tile_regunit.txt",
                               package = "hydrographr"))

  # Find the tile ID in the lookup table based on the regional unit id
  sort(unique(lookup[lookup$reg_unit %in% reg_un, tile]))

}
=======
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


>>>>>>> dev_get_tile_id
