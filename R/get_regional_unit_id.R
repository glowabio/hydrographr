#' Identifies the ids of the regional units
#' in which the given points are located.
#' Input is a point data frame.
#'
#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column as character string
#' @param lat Name of the latitude column as character string
#' @importFrom stringi stri_rand_strings
#' @importFrom dplyr select
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#'


# provide points an an input and get the regional units
# where the points belong (without the full extent)

get_regional_unit_id <- function(data, lon, lat) {

  # global file of regional units ids
  reg_unit_file <- paste0(tempdir(), "/regional_unit_ovr.tif")

  # If the required file does not already exist,
  # download it in the tempdir()
  if (!file.exists(reg_unit_file)) {
    print("Downloading required file")
    download.file("https://drive.google.com/uc?export=download&id=1ykV0jRCglz-_fdc4CJDMZC87VMsxzXE4&confirm=t",
                  destfile = reg_unit_file)

  }


  # Export taxon occurrence points
  dataset_tmp_path <- paste0(tempdir(), "/points_dataset.txt")
  fwrite(data, dataset_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  reg_unit_ids <- processx::run(system.file("sh", "get_regional_unit_id.sh",
                  package = "hydrographr"),
      args = c(dataset_tmp_path, lon, lat,
               reg_unit_file,  tempdir()),
      echo = FALSE)


  # Read in the file containing the ids
  data_reg_unit_ids <- fread(paste0(tempdir(), "/reg_unit_ids.txt"),
                             keepLeadingZeros = TRUE, header = TRUE, sep = " ")


  # Return vector of regional unit ids
  data_reg_unit_ids$reg_unit_id

}
