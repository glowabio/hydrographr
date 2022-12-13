<<<<<<< HEAD


=======
>>>>>>> dev_download_tiles
#' Downloads the test data for the Hydrography90m dataset
#'
#' The test data contains all Hydrography90m and species point observation data for a small geographic extent to test the functions.
#'
#' The test data is available at https://drive.google.com/file/d/1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD/view?usp=share_link and can be automatically downloaded and unzipped with this function to a desired path.
<<<<<<< HEAD
=======

>>>>>>> dev_download_tiles
#'
#' @param download_path The path where the files will be downloaded
#' @author Afroditi Grigoropoulou
#' @export
#'
#' @examples
#' # Download the test data to the current working directory
#' download_test_data()
#'
<<<<<<< HEAD
#' # Download the data to a specific directory
=======
#' # Download the data to a specific (existing) directory
>>>>>>> dev_download_tiles
#' download_test_data("path/to/your/directory")

download_test_data <- function(download_path = ".") {


  # General path to the test data folder in GDrive
  gdrive_path <- "https://docs.google.com/uc?export=download&id="

  # File id of the zipped folder retrieved from gdrive
  file_id <- "1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD"

  # Create the folder where the files will be downloaded if it doesn't exist
  ifelse(!dir.exists(paste0(download_path, "/hydrography90m_test_data")),
<<<<<<< HEAD
          dir.create(paste0(download_path, "/hydrography90m_test_data")), FALSE)

=======
         dir.create(paste0(download_path, "/hydrography90m_test_data")), FALSE)
>>>>>>> dev_download_tiles

  download.file(paste0(gdrive_path, file_id, "&confirm=t"),
                destfile = paste0(download_path, "/hydrography90m_test_data.zip"), mode = "wb")

<<<<<<< HEAD

=======
>>>>>>> dev_download_tiles
  # Unzip the data
  unzip(paste0(download_path, "/hydrography90m_test_data.zip"),  overwrite = T,
        exdir = paste0(download_path, "/hydrography90m_test_data"),
        unzip=getOption("unzip"))

  # remove the zip file
  unlink(paste0(download_path, "/hydrography90m_test_data.zip"))

  # Report
  cat("Data downloaded and unzipped to ", download_path,"/hydrography90m_test_data\n", sep = "")
<<<<<<< HEAD
=======

>>>>>>> dev_download_tiles

}

