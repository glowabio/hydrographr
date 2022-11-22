#' Adds sub-catchment and/or basin IDs to a dataframe of points
#'
#' @param data data.frame with the lat lon columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
#' @param subc_path Path to the sub-catchment ID .tif layer
#' @param basin_path Path to the basin ID .tif layer
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
#'
extract_ids <- function(data, lon, lat, subcatchment_path = NULL, basin_path = NULL) {
  # Remove all files in the tmp folder
  unlink(paste0(tempdir(), "/*"))

  # Export taxon occurrence points
  dataset_tmp_path <- paste0(tempdir(), "/points_dataset.txt")
  fwrite(data, dataset_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # Convert null arguments to 0 so that bash can evaluate the variables
  subc_path <- ifelse(is.null(subcatchment_path), 0, subcatchment_path)
  bas_path <- ifelse(is.null(basin_path), 0, basin_path)

  # Call the external .sh script extract_ids() containing the gdal function
  subc_ids <- run(system.file("sh", "extract_ids.sh",
                              package = "hydrographr"),
                  args = c(dataset_tmp_path, lon, lat, subc_path,
                            bas_path, tempdir()),
                  echo = FALSE)

  # Read in the file containing the ids
  data_ids <- fread(paste0(tempdir(), "/ids.txt"), keepLeadingZeros = TRUE,
                    header = T, sep = " ")

  # Remove all files in the tmp folder
  unlink(paste0(tempdir(), "/*"))

  # Return data frame
  return(data_ids)


}

