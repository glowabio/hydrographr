#' @title Get the Hydrography90m 20°x20° tile ID
#'
#' @description Identifies the 20°x20° tile IDs of the Hydrography90m
#' data in which the input points are located. The IDs can then be used to
#' download the data using \code{\link{download_tiles()}}. The input is a data
#' frame with point coordinates.
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the longitude / latitude coordinates in WGS84.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @importFrom data.table fread
#' @export
#'
#' @author Afroditi Grigoropoulou
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load species occurrence data
#' species_occurrence <- read.table(paste0(my_directory,
#'                                        "/hydrography90m_test_data",
#'                                        "/spdata_1264942.txt"),
#'                                  header = TRUE)
#'
#' # Get the tile ID
#' get_tile_id(data = species_occurrence,
#'             lon = "longitude", lat = "latitude")


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
