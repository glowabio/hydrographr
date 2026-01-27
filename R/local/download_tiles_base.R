#' Internal function that downloads a single file from
#' https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F or the
#' GDrive folder.
#' It is called and inherits arguments by the function 'download_tiles()'.
#'
#' @param variable character vector of variable names.
#' @param file_format character. Format of the requested file ("tif" or "gpkg").
#' @param tile_id character. The ID of the requested tile or regional unit.
#' @param global logical. If TRUE, the global extent file is downloaded.
#' Default is FALSE.
#' @param download_dir character. The directory where the files will be
#' downloaded. Default is the working directory.
#' @param file_size_table data.frame. Lookup table including file names
#' and sizes (inherited by 'download_tiles()').
#' @param server_url character. url to the the home download folder
#' in either Nimbus or GDrive (inherited by 'download_tiles()').
#' @keywords internal
#'

download_tiles_base <- function(variable, file_format = "tif",
                                tile_id = NULL,
                                global = FALSE, download_dir = ".",
                                file_size_table = NULL,
                                server_url = NULL) {

  if (!(dir.exists(download_dir))) {
    stop(paste0("Directory ", download_dir, " does not exist, cannot download to there."))
  }

  # Get the file_name, e.g. "stream_dist_up_farth_h00v02.tif"
  file_name <- ifelse(global == TRUE, paste0(variable, "_ovr.", file_format),
                    paste0(variable, "_", tile_id, ".", file_format))

  # Get file path with parent folder structure, incl. file name
  # e.g. "r.stream.distance/stream_dist_up_farth_tiles20d/stream_dist_up_farth_h00v02.tif"
  row_selector <- file_size_table$file_name == file_name
  file_path <- file_size_table[row_selector,]$file_path
  if (length(file_path)>1) stop(paste0("Found several file paths (expected one): ", paste(file_path, collapse=", ")))

  if (!(any(row_selector))){
    message('Skipping file "', file_name, '" (not found)...')
    warning('Problem: Did not find any file "', file_name, '" in the list of files - are you sure it is a valid file?')
    return()
  }

  # Get parent folder structure to reproduce it in the download path
  # e.g. "r.stream.distance/stream_dist_up_farth_tiles20d"
  folder_structure <- dirname(file_path)

  # Create download directories
  dir.create(paste0(download_dir, "/", folder_structure),
             showWarnings = FALSE, recursive = TRUE)

  destfile <- paste0(download_dir, "/", file_path)

  # Always download cti_ovr.tif from GDrive:
  if (file_name == "cti_ovr.tif") {

    # Inform user why Nimbus is not used:
    if (grepl("public.igb-berlin.de", server_url, fixed=TRUE)) {
      message(paste('Downloading', file_name, 'from GDrive instead of IGB because it does not exist on IGB servers.')) # TODO
    }

    # Get GDrive file id from the lookup table
    file_id <- file_size_table[row_selector, ]$file_id

    # The addition of &confirm=t in the download link
    # skips the virus scan of the gdrive
    gdrive_path <- "https://drive.google.com/uc?export=download&id="
    download.file(paste0(gdrive_path, file_id, "&confirm=t"),
                  destfile = destfile, mode = "wb")


  # Download from Nimbus
  } else if (grepl("public.igb-berlin.de", server_url, fixed=TRUE)) {

    download.file(paste0(server_url, gsub("/", "%2F", file_path)),
                  destfile = destfile, mode = "wb")


  # Download from GDrive
  } else if (grepl("drive.google.com", server_url, fixed=TRUE)) {

    # Get GDrive file id from the lookup table
    file_id <- file_size_table[row_selector, ]$file_id

    # The addition of &confirm=t in the download link
    # skips the virus scan of the gdrive
    download.file(paste0(server_url, file_id, "&confirm=t"),
                  destfile = destfile, mode = "wb")

  } else {
    warning("Unrecognized download URL, cannot download:", server_url)
  }

  return(destfile)
}
