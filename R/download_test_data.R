#' Downloads the test data of the Hydrography90m dataset
#'
#' @title Download test data
#'
#' @description Download the test data of the package, which includes all
#' Hydrography90m and species point observation data
#' for a small geographic extent, to test the functions.
#'
#' The test data will be automatically downloaded and unzipped
#' with this function to a desired path, or can be alternatively downloaded at
#'
#' \url{https://drive.google.com/file/d/1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD/view?usp=share_link}.
#'
#' @param download_dir character. The directory where the files will be
#' downloaded. Default location is the working directory.
#'
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
#' Amatulli, G., Garcia Marquez, J., Sethi, T., Kiesel, J., Grigoropoulou, A.,
#' Üblacker, M. M., Shen, L. Q., and Domisch, S.: Hydrography90m: a new
#' high-resolution global hydrographic dataset, Earth Syst. Sci. Data, 14,
#' 4525–4550, \url{https://doi.org/10.5194/essd-14-4525-2022, 2022.}
#'
#' Amatulli G., Garcia Marquez J., Sethi T., Kiesel J., Grigoropoulou A.,
#' Üblacker M., Shen L. & Domisch S. (2022-08-09 ). Hydrography90m: A new
#' high-resolution global hydrographic dataset. IGB Leibniz-Institute of
#' Freshwater Ecology and Inland Fisheries. dataset.
#' \url{https://doi.org/10.18728/igb-fred-762.1}

download_test_data <- function(download_dir = ".") {


  # General path to the test data folder in GDrive
  gdrive_path <- "https://docs.google.com/uc?export=download&id="

  # File id of the zipped folder retrieved from gdrive
  file_id <- "1kYNWXmtVm6X7MZLISOePGpvxB1pk1scD"

  # Entire URL to download the zipped test data:
  gdrive_url <- paste0(gdrive_path, file_id, "&confirm=t")
  igb_url <- "https://public.igb-berlin.de/index.php/s/9XGDD3EmHTs69g8/download"

  # Local directory where to store it:
  where_to_store <- paste0(download_dir, "/hydrography90m_test_data")
  full_path_local_zip <- paste0(where_to_store, "/hydrography90m_test_data.zip")

  # Create the folder where the files will be downloaded if it doesn't exist
  ifelse(!dir.exists(where_to_store),
         dir.create(where_to_store), FALSE)

  # First try downloading at IGB, then at GDrive:
  server_url <- tryCatch(
    {
      options(timeout=10) # seconds
      utils::download.file(igb_url, destfile = full_path_local_zip, mode = "wb")
      # This may run into a timeout of 60 seconds, if IGB servers are not reachable
    },
    warning = function(c) {
      #warning("Could not download test data from ", igb_url, ", trying now at ", gdrive_url)
      #download.file(gdrive_url, destfile = full_path_local_zip, mode = "wb")
      # This may run into a problem that you need to manually skip the virus check
      stop("Could not download test data from ", igb_url,
              ".\nPlease, download manually at", gdrive_url, "or",
              igb_url, ", store to", full_path_local_zip, "and unzip!")
    },
    error = function(c) {
      #warning("Could not download test data from ", igb_url, ", trying now at ", gdrive_url)
      #download.file(gdrive_url, destfile = full_path_local_zip, mode = "wb")
      # This may run into a problem that you need to manually skip the virus check
      stop("Could not download test data from ", igb_url,
              ".\nPlease, download manually at ", gdrive_url, " or ",
              igb_url, ", store to ", full_path_local_zip, " and unzip!")
    }
  )

  # Checking file size, if too small it is probably a HTML page with
  # a virus check warning...
  if (file.size(full_path_local_zip) < 30000000) { # bytes (real size is > 36 MB)

    gdrive_url_base <- "https://drive.google.com/uc?export=download&id="
    gdrive_url_full <- paste0(gdrive_url_base, "1ykV0jRCglz-_fdc4CJDMZC87VMsxzXE4&confirm=t")

    # Checking the actual text content (only first 10 lines):
    first_lines <- readLines(full_path_local_zip, warn = FALSE)[1:10]
    if (any(grepl("still like to download", first_lines, fixed = TRUE))) {
      msg <- paste("Downloading the zipped data from GDrive went wrong,",
                   "as you manually need to confirm skipping the virus check.",
                   "\nPlease, download manually at", gdrive_url, "or",
                   igb_url, "and store to", full_path_local_zip,
                   ". Stopping.")
      stop(msg)

    } else {
      # In case the text is in a different locale and does not contain those
      # exact words, still warn the user:
      msg <- paste0("Downloading the zipped data probably",
                    " went wrong, it is only ", file.size(full_path_local_zip),
                    " bytes.\nIf this function fails, please download",
                    " manually at ", gdrive_url, " or ",
                    igb_url, " and store to ", full_path_local_zip, ".")
      stop(msg)
    }
  }

  # Unzip the data
  unzip(full_path_local_zip,
        overwrite = TRUE,
        exdir = where_to_store,
        unzip = getOption("unzip"))

  # remove the zip file
  unlink(full_path_local_zip)

  # Report
  if (length(list.files(where_to_store)) > 40) {
    cat("Data downloaded and unzipped to ",
        download_dir, "/hydrography90m_test_data\n", sep = ""
    )
  }
}
