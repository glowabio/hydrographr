#' Downloads the test data for the Hydrography90m dataset
#'
#' The test data contains all Hydrography90m and species point observation data
#' for a small geographic extent to test the functions.
#'
#' The test data is available at
#' https://drive.google.com/file/d/1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD/view?usp=share_link
#' and can be automatically downloaded and unzipped
#' with this function to a desired path.
#'
#' @param download_dir character. The directory where the files will be
#' downloaded. Default location is the working directory.
#' @author Afroditi Grigoropoulou
#' @export
#'
#' @examples
#' # Download the test data to the current working directory
#' download_test_data()
#'
#' # Download the data to a specific (existing) directory
#' download_test_data("path/to/your/directory")
#'
#' @references
#'
#' Amatulli, G., Garcia Marquez, J., Sethi, T., Kiesel, J., Grigoropoulou, A.,
#' Üblacker, M. M., Shen, L. Q., and Domisch, S.: Hydrography90m: a new
#' high-resolution global hydrographic dataset, Earth Syst. Sci. Data, 14,
#' 4525–4550, https://doi.org/10.5194/essd-14-4525-2022, 2022.")
#'
#' Amatulli G., Garcia Marquez J., Sethi T., Kiesel J., Grigoropoulou A.,
#' Üblacker M., Shen L. & Domisch S. (2022-08-09 ). Hydrography90m: A new
#' high-resolution global hydrographic dataset. IGB Leibniz-Institute of
#' Freshwater Ecology and Inland Fisheries. dataset.
#' https://doi.org/10.18728/igb-fred-762.1

download_test_data <- function(download_dir = ".") {


  # General path to the test data folder in GDrive
  gdrive_path <- "https://docs.google.com/uc?export=download&id="

  # File id of the zipped folder retrieved from gdrive
  file_id <- "1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD"

  # Create the folder where the files will be downloaded if it doesn't exist
  ifelse(!dir.exists(paste0(download_dir, "/hydrography90m_test_data")),

         dir.create(paste0(download_dir, "/hydrography90m_test_data")), FALSE)

  download.file(paste0(gdrive_path, file_id, "&confirm=t"),
                destfile = paste0(
                  download_dir, "/hydrography90m_test_data.zip"),
                  mode = "wb")

  # Unzip the data
  unzip(paste0(download_dir, "/hydrography90m_test_data.zip"),
  overwrite = TRUE,
        exdir = paste0(download_dir, "/hydrography90m_test_data"),
        unzip = getOption("unzip"))

  # remove the zip file
  unlink(paste0(download_dir, "/hydrography90m_test_data.zip"))

  # Report
  cat("Data downloaded and unzipped to ",
  download_dir, "/hydrography90m_test_data\n", sep = "")

}
