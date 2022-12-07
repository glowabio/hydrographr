#' Downloads the test data for the Hydrography90m dataset
#'
#' The test data is available at from https://drive.google.com/drive/u/1/folders/18RpqmywL7yThitsOklWXAuYIfMvfyPfg and can b downloaded with this function.
#'
#' @param download_path The path where the files will be downloaded
#' @author Afroditi Grigoropoulou
#' @export
#'
#' @examples
#' # Download the test data to the current working directory
#' download_test_data()
#'

download_test_data <- function(download_path = "./hydrography90m_test_data") {

  # General path to the test data folder in GDrive
  gdrive_path <- "https://docs.google.com/uc?export=download&id="

  # File id of the zipped folder retrieved from gdrive
  file_id <- "1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD"

  download.file(paste0(gdrive_path, file_id, "&confirm=t"),
                destfile = paste0(download_path, ".zip"), mode = "wb")

  # Unzip the data
  unzip(paste0(download_path, ".zip"),  overwrite = T,
        exdir = download_path,
        unzip=getOption("unzip"))

  # remove the zip file
  unlink(paste0(download_path, ".zip"))

  # Report
  cat("Data downloaded and unzipped to", download_path, "\n")

}
