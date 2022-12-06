#' Downloads all the test data from https://drive.google.com/drive/u/1/folders/18RpqmywL7yThitsOklWXAuYIfMvfyPfg.
#'
#' @param download_path The path where the files will be downloaded
#' @author Afroditi Grigoropoulou
#' @export
#'

download_test_data <- function(download_path = "./hydrography90m_test_data.zip") {

  # General path to the test data folder in GDrive
  gdrive_path <- "https://docs.google.com/uc?export=download&id="

  # File id of the zipped folder retrieved from gdrive
  file_id <- "17qxc0t8PqnzuUzOtfXytwX8_qwNrfN6s"

  download.file(paste0(gdrive_path, file_id, "&confirm=t"),
                destfile = download_path)

}
