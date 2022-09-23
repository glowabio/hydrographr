#' Join sub-catchment IDs with occurrence points
#'
#' Add the column 'subcID' to the dataset in 'dataset_path' and export the
#' data frame in a csv.
#' In the current version the long-lat values must be in the 2nd and 3rd
#' column of the data set text file.
#'
#' @param dataset_path Path to data set text file with the lat long columns
#' @param lon Name of the longitude column
#' @param lat Name of the latitude column
#' @param subc_path Path to the sub-catchment ID .tif layer
#' @param basin_path Path to the basin ID .tif layer
#' @importFrom data.table fread
#' @importFrom processx run
#' @importFrom rlang is_missing
#' @export
#'
extract_ids <- function(dataset_path, lon, lat, subc_path, basin_path) {

  # Import taxon occurrence points
  dataset <- fread(dataset_path, header = TRUE, keepLeadingZeros = TRUE)

  # To extract only subcatchment ids
  if (missing(basin_path) || is.na(basin_path)) {

  # Call the external .sh script extract_ids() containing the gdal function
  subc_ids <- run(system.file("sh", "extract_ids.sh",
                              package = "hydrographr"),
                           args = c(dataset_path, lon, lat, subc_path),
                           echo = FALSE)$stdout
  # Format the ids string
  subc_ids <- data.frame(subc_id = as.integer(
    strsplit(subc_ids, "\n", "", fixed = TRUE)[[1]]
    ))

  # Join the IDs with the observations
  data_ids <- cbind.data.frame(dataset, subc_ids)

  # To extract only basin ids
} else if (missing(subc_path) || is.na(subc_path)) {

  basin_ids <- run(system.file("sh", "extract_ids.sh",
                               package = "hydrographr"),
                       args = c(dataset_path, lon, lat, basin_path),
                       echo = FALSE)$stdout
  # Format the ids string
  basin_ids <- data.frame(basin_ids = as.integer(
    strsplit(basin_ids, "\n", "", fixed = TRUE)[[1]]
  ))

  # Join the IDs with the observations
  data_ids <- cbind.data.frame(dataset, basin_ids)

   # To extract both subcatchment and basin ids
} else {

  subc_ids <- run(system.file("sh", "extract_ids.sh",
                              package = "hydrographr"),
                            args = c(dataset_path, lon, lat, subc_path),
                            echo = FALSE)$stdout

  # Format the ids string
  subc_ids <- data.frame(subc_id = as.integer(
    strsplit(subc_ids, "\n", "", fixed = TRUE)[[1]]
  ))

  basin_ids <- run(system.file("sh", "extract_ids.sh",
                               package = "hydrographr"),
                             args = c(dataset_path, lon, lat, basin_path),
                             echo = FALSE)$stdout
  # Format the ids string
  basin_ids <- data.frame(basin_ids = as.integer(
    strsplit(basin_ids, "\n", "", fixed = TRUE)[[1]]
  ))

  # Join the IDs with the observations
  data_ids <- cbind.data.frame(dataset, subc_ids, basin_ids)
}
    ids <- run(system.file("sh", "extract_ids.sh",
                          package = "hydrographr"),
                      args = c(dataset_path, subc_path, lon, lat),
                      echo = FALSE)$stdout

  return(data_ids)


  }

