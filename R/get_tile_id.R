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


  # If the required files do not already exist,
  # download them in the inst/data/ package folder
  dir.create("tmp", showWarnings = FALSE)
  if (!file.exists("tmp/regional_unit_ovr.tif")) {
    print("Downloading required file")
    download.file("https://drive.google.com/uc?export=download&id=1ykV0jRCglz-_fdc4CJDMZC87VMsxzXE4&confirm=t",
                  destfile = "tmp/regional_unit_ovr.tif")

}


  if (!file.exists("tmp/lookup_tile_regunit.txt")) {
    download.file("https://drive.google.com/uc?export=download&id=1deKhOEjGgvUXPwivYyH99hgHlJV7OgUv&confirm=t",
                  destfile = "tmp/lookup_tile_regunit.txt",
                  quiet = TRUE)

  }

  reg_un <- get_regional_unit_id(data, lon, lat)

  lookup <- fread(system.file("data", "lookup_tile_regunit.txt",
                               package = "hydrographr"))

  # Find the tile ID in the lookup table based on the regional unit id
  tile_id <- sort(unique(lookup[lookup$reg_unit %in% reg_un, tile]))
  tile_id

}
