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
  file_id <- "1tMpFJQI83OfuBut0QYoLxIN2VMEpdO0N"

  download.file("https://doc-0o-a0-docs.googleusercontent.com/docs/securesc/2sptjdpfbfglp1a0rhhe26hsbej2u8bk/n3rp2utc9t5tk0sbvddibive9o2pm1t7/1670413575000/00772125120773016308/13507423287198476060Z/1xejqoPC7ydT4W6lVED9GNVxYLwWzhv-r?e=download&uuid=9e30b611-f345-467e-9b6a-2191105c6986&nonce=rhhjml742t9ge&user=13507423287198476060Z&hash=ofcj9svbssgge2bncd13d246arng6a4q", destfile = paste0(download_path, "/hydrographr_data.zip"), quiet=T)

  # Unzip the data
  unzip(paste0(download_path, "/hydrographr_data.zip"),  overwrite = T,
        exdir = paste0(download_path, "/hydrographr_data"),
        unzip=getOption("unzip"))

  # remove the zip file
  unlink(paste0(download_path, "/hydrographr_data.zip"))

  # Report
  cat("Data downloaded and unzipped to", download_path, "\n")

}

