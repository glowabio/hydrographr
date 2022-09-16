#' Join sub-catchment IDs with occurrence points
#'
#' Add the column 'subcID' to the dataset in 'dataset_path' and export the
#' dataframe in a csv.
#' The longitude-latitude values should be in the 3rd-4th column respectively!!
#'
#' @param subc_path Description of parameter subc_path
#' @param dataset_path Description of parameter dataset_path
#' @importFrom datatable fread
#' @importFrom processx run
#' @export
#'
get_subcID <- function(taxon, subc_path, dataset_path) {

  dataIDs <- NULL
  IDs <- NULL

  # Call external gdal command of gdal_location_info()
  IDs <- run(system.file("extdata", "gdal_location_info",
                         package = "hydrographr"),
                    args = c(dataset_path, subc_path),

                    echo = F)$stdout
  # Format the IDs string
  IDs <- data.frame(subcID = strsplit(IDs, "\n", "", fixed = TRUE)[[1]])

  # Import taxon occurrence points
  dataset <- fread(dataset_path, header = T, sep=" ")

  # Join the IDs with the observations
  dataIDs <- cbind.data.frame(dataset, IDs)

  return(dataIDs)
}


