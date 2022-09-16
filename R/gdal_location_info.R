#' Join sub-catchment IDs with occurrence points
#'
#' Add the column 'subcID' to the dataset in 'dataset_path' and export the
#' data frame in a csv.
#' In the current version the long-lat values must be in the 2nd and 3rd
#' column of the data set text file.
#'
#' @param subc_path Path to the sub-catchment ID .tif layer
#' @param dataset_path Path to data set text file with the lat long columns
#' @importFrom data.table fread
#' @importFrom processx run
#' @export
#'
get_subcID <- function(subc_path, dataset_path) {
  # Call external gdal command of gdal_location_info()
  IDs <- run(system.file("sh", "gdal_location_info.sh",
                         package = "hydrographr"),
                    args = c(dataset_path, subc_path),
                    echo = F)$stdout
  # Format the IDs string
  IDs <- data.frame(subcID = as.integer(strsplit(IDs, "\n", "", fixed = TRUE)[[1]]))

  # Import taxon occurrence points
  dataset <- fread(dataset_path, header = T, sep=" ")

  # Join the IDs with the observations
  dataIDs <- cbind(dataset, IDs)

  return(dataIDs)
}


