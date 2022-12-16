#' Downloads a single file from
#' https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F.
#' It is called and inherits arguments by the function 'download_tiles()'.
#'
#' @param variable vector of variable names (character)
#' @param filetype format of the requested file ("tif" or "gpkg")
#' @param tile_id id of the requested tile (character)
#' @param global Should the global file be downloaded or not.
#' TRUE/FALSE, FALSE by default
#' @param download_path The path where the files will be downloaded
#' @param file_size_table_sep lookup table of file sizes
#' (inherited by 'download_tiles()')
#' @param server_path path to the the home download folder
#' in either Nimbus or GDrive (inherited by 'download_tiles()')
#' @keywords internal
#' @export
#'

download_tiles_base <- function(variable, filetype = "tif",
                                tile_id = NULL,
                                global = FALSE, download_path = ".",
                                file_size_table_sep = NULL,
                                server_path = NULL) {

  # Get the name of the file
  varname <- ifelse(global == TRUE, paste0(variable, "_ovr.", filetype),
                    paste0(variable, "_", tile_id, ".", filetype))

  # Find grass module to build the download link
  # and to create the download destination folder
  grass_module <- file_size_table_sep[varname_tile ==
                                        varname, ]$grass_module

  # Find folder name to build the download link
  # and to create the download destination folder
  foldername <- file_size_table_sep[varname_tile == varname, ]$foldername

  # Get file path with parent folder structure
  file_path <- str_c(file_size_table_sep[varname_tile == varname,
                                         c(grass_module, foldername, varname)],
                     collapse = "/")

  # Get parent folder structure to reproduce it in the download path
  folder_structure <- str_c(file_size_table_sep[varname_tile == varname,
                                                c(grass_module, foldername)],
                            collapse = "/")

  # Create download directories
  dir.create(paste0(download_path, "/", folder_structure),
             showWarnings = FALSE, recursive = TRUE)

  # General path to the download folder in Nimbus
  nimbus_path <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"
  # General path to the download folder in GDrive
  gdrive_path <- "https://drive.google.com/uc?export=download&id="

  # Download from Nimbus
  if (server_path == nimbus_path && varname != "cti_ovr.tif") {

    print(varname)
    download.file(paste0(nimbus_path, gsub("/", "%2F", file_path)),
                  destfile = paste0(download_path, "/", file_path))

  }

  # Download from GDrive
  if (server_path == gdrive_path || varname == "cti_ovr.tif") {

    # Get GDrive file id from the lookup table
    file_id <- file_size_table_sep[varname_tile == varname, ]$file_id
    print(varname)

    # The addition of &confirm=t in the download link
    # skips the virus scan of the gdrive
    download.file(paste0(gdrive_path, file_id, "&confirm=t"),
                  destfile = paste0(download_path, "/", file_path))

  }


}
