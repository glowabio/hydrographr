#' @title Download files of the Hydrography90m dataset
#'
#' @description The function downloads data of the Hydrography90m
#' dataset, which is split into 20°x20° tiles. If a tile ID is specified, then
#' the selected layers (variable) will be downloaded. In addition,
#' the Hydrography90m is organized in non-interrupted drainage basins called
#' regional units. If a regional unit ID (reg_unit_id) is specified, then only
#' the raster mask of the drainage basin is downloaded (useful for later
#' processing). Multiple regular tiles, e.g. belonging to regional units, can be
#' downloaded in a single request. The tile or regional unit IDs can be
#' obtained using the functions "get_tile_id" and "get_regional_unit_id",
#' respectively.
#' The files will be stored locally in a folder architecture, similar as in the
#' data repository, available at
#' \url{https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F}.
#'
#' @param variable character vector of variable names. See Details for all the
#' variable names.
#' @param file_format character. Format of the requested file ("tif" or "gpkg").
#' See Details.
#' @param tile_id character vector. The IDs of the requested tiles.
#' @param reg_unit_id character vector. The IDs of the requested regional units.
#' @param global logical. If TRUE, the global extent file is downloaded.
#' Default is FALSE.
#' @param download_dir character. The directory where the files will be
#' downloaded. Default is the working directory.
#' @importFrom tidyr separate
#' @importFrom stringr str_split_fixed str_extract
#' @export
#'
#' @author Afroditi Grigoropoulou
#'
#' @references Amatulli G., Garcia Marquez J., Sethi T., Kiesel J.,
#' Grigoropoulou A., Üblacker M., Shen L. & Domisch S. (2022-08-09 )
#' Hydrography90m: A new high-resolution global hydrographic dataset.
#' IGB Leibniz-Institute of Freshwater Ecology and Inland Fisheries.
#' dataset. \url{https://doi.org/10.18728/igb-fred-762.1}
#'
#' @details
#' In the following table you can find all the variables included in the
#' Hydrography90m dataset. The column "Variable" includes the variable names
#' that should be used as an input in the parameter "variable" of the function.
#' Likewise, the column "File format" contains the input that should be given to
#' the "file_format" parameter.
#' For more details and visualisations of the spatial layers, please refer to
#' \url{https://hydrography.org/hydrography90m/hydrography90m_layers/}.
#'
#'
#'| **Variable type**    | **Variable name**                             | **Variable**          | **Unit** | **File format** |
#' |----------------------|------------------------------------------|---------------------------|----------|------------------|
#'   | Network              | Drainage basin                           | basin                     |          | tif              |
#'   | Network              | Drainage basin                           | basin                     |          | gpkg             |
#'   | Network              | Sub-catchment                           | sub_catchment            |          | tif              |
#'   | Network              | Sub-catchment                           | sub_catchment            |          | gpkg             |
#'   | Network              | Stream segment                           | segment                   |          | tif              |
#'   | Network              | Outlet                                   | outlet                    |          | tif              |
#'   | Network              | Outlet                                   | outlet                    |          | gpkg             |
#'   | Network              | Regional unit                            | regional_unit            |          | tif              |
#'   | Network              | Flow direction                            | direction            |          | tif              |
#'   | Flow                 | Flow accumulation                        | accumulation                      | km^2     | tif              |
#'   | Stream slope         | Cell maximum curvature                   | slope_curv_max_dw_cel | 1/m      | tif              |
#'   | Stream slope         | Cell minimum curvature                   | slope_curv_min_dw_cel | 1/m      | tif              |
#'   | Stream slope         | Cell elevation difference                | slope_elv_dw_cel       | m        | tif              |
#'   | Stream slope         | Cell gradient                            | slope_grad_dw_cel      |          | tif              |
#'   | Stream distance      | Shortest distance to drainage divide     | stream_dist_up_near    | m        | tif              |
#'   | Stream distance      | Longest distance to drainage divide      | stream_dist_up_farth   | m        | tif              |
#'   | Stream distance      | Nearest down stream stream grid cell     | stream_dist_dw_near    | m        | tif              |
#'   | Stream distance      | Outlet grid cell in the network          | outlet_dist_dw_basin   | m        | tif              |
#'   | Stream distance      | Down stream stream node grid cell        | outlet_dist_dw_scatch  | m        | tif              |
#'   | Stream distance      | Euclidean distance                       | stream_dist_proximity   | m        | tif              |
#'   | Elevation difference | Shortest path                            | stream_diff_up_near    | m        | tif              |
#'   | Elevation difference | Longest path                             | stream_diff_up_farth   | m        | tif              |
#'   | Elevation difference | Nearest downstream stream pixel          | stream_diff_dw_near    | m        | tif              |
#'   | Elevation difference | Outlet grid cell in the network          | outlet_diff_dw_basin   | m        | tif              |
#'   | Elevation difference | Downstream stream node grid cell         | outlet_diff_dw_scatch  | m        | tif              |
#'   | Segment properties   | Segment downstream mean gradient         | channel_grad_dw_seg    |          | tif              |
#'   | Segment properties   | Segment upstream mean gradient           | channel_grad_up_seg    |          | tif              |
#'   | Segment properties   | Cell upstream gradient                   | channel_grad_up_cel    |          | tif              |
#'   | Segment properties   | Cell stream course curvature             | channel curv_cel         |          | tif              |
#'   | Segment properties   | Segment downstream elevation difference  | channel_elv_dw_seg     |          | tif              |
#'   | Segment properties   | Segment upstream elevation difference    | channel_elv_up_seg     |          | tif              |
#'   | Segment properties   | Cell upstream elevation difference       | channel_elv_up_cel     |          | tif              |
#'   | Segment properties   | Cell downstream elevation difference     | channel_elv_dw_cel     |          | tif              |
#'   | Segment properties   | Segment downstream distance              | channel_dist_dw_seg    |          | tif              |
#'   | Segment properties   | Segment upstream distance                | channel_dist_up_seg    |          | tif              |
#'   | Segment properties   | Cell upstream distance                   | channel_dist_up_cel    |          | tif              |
#'   | Stream order         | Strahler’s stream order                  | order_strahler           |          | tif              |
#'   | Stream order         | Shreve’s stream magnitude                | order_shreve             |          | tif              |
#'   | Stream order         | Horton’s stream order                    | order_horton             |          | tif              |
#'   | Stream order         | Hack’s stream order                      | order_hack               |          | tif              |
#'   | Stream order         | Topological dimension of streams         | order_topo               |          | tif              |
#'   | Stream order         | Strahler’s stream order                  | order_vect_segment      |          | gpkg             |
#'   | Stream order         | Shreve’s stream magnitude                | order_vect_segment      |          | gpkg             |
#'   | Stream order         | Horton’s stream order                    | order_vect_segment      |          | gpkg             |
#'   | Stream order         | Hack’s stream order                      | order_vect_segment      |          | gpkg             |
#'   | Stream order         | Topological dimension of streams         | order_vect_segment      |          | gpkg             |
#'   | Stream reach         | Length of the stream reach               | order_vect_segment      | m        | gpkg             |
#'   | Stream reach         | Straight length                          | order_vect_segment      | m        | gpkg             |
#'   | Stream reach         | Sinusoid of the stream reach             | order_vect_segment      |          | gpkg             |
#'   | Stream reach         | Accumulated length                       | order_vect_segment      | m        | gpkg             |
#'   | Stream reach         | Flow accumulation                        | order_vect_segment      | km^2     | gpkg             |
#'   | Stream reach         | Distance to outlet                       | order_vect_segment      | m        | gpkg             |
#'   | Stream reach         | Source elevation                         | order_vect_segment      | m        | gpkg             |
#'   | Stream reach         | Outlet elevation                         | order_vect_segment      | m        | gpkg             |
#'   | Stream reach         | Elevation drop                           | order_vect_segment      |          | gpkg             |
#'   | Stream reach         | Outlet drop                              | order_vect_segment      |          | gpkg             |
#'   | Stream reach         | Gradient                                 | order_vect_segment      |          | gpkg             |
#'   | Flow index           | Stream power index                       | spi                       |          | tif              |
#'   | Flow index           | Sediment transportation index            | sti                       |          | tif              |
#'   | Flow index           | Compound topographic index               | cti                       |          | tif              |
#'
#' @md
#' @note
#' If there is an error during the download of a file
#' (more likely in case of files bigger than 3-4GB), you can try to manually
#' download this file by pasting the link that is returned by the error
#' message in your browser.
#'
#' @examples
#' # Download data for two variables in three regular tiles
#' # to the current working directory
#' download_tiles(variable = c("sti", "stream_dist_up_farth"),
#'                file_format = "tif",
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#'
#' # Download the global .tif layer for the variable "direction"
#' # into the temporary R folder or define a different directory
#' # Define directory
#' my_directory <- tempdir()
#' # Download layer
#' download_tiles(variable = "direction",
#'                file_format = "tif",
#'                global = TRUE,
#'                download_dir = my_directory)
#'
#' # Download the raster mask of two regional units
#' # to the current working directory.
#' download_tiles(variable = "regional_unit",
#'                file_format = "tif",
#'                reg_unit_id = c("33","34"))
#'
#' # Download the raster mask of all regional units
#' # to the current working directory.
#' download_tiles(variable = "regional_unit",
#'                file_format = "tif",
#'                global = TRUE)

download_tiles <- function(variable, file_format = "tif",
                           tile_id = NULL, reg_unit_id = NULL,
                           global = FALSE, download_dir = ".") {

  # Introductory steps

  # Set timeout option for download to 4 hours (14400 seconds)
  options(timeout=14400)

  # Download lookup table with the size of each file
  # if it doesn't exist in the tempdir()
  file_size_file <- paste0(tempdir(), "/hydrography90m_paths_file_sizes.txt")
  if (!file.exists(file_size_file)) {
    download.file("https://drive.google.com/uc?export=download&id=1SEkcgGPutP6ZQPvYtzICh_gcGnVgH_uR&confirm=t",
                  destfile = file_size_file, mode = "wb")

  }

  # Import lookup table with the size of each file
  file_size_table <- fread(file_size_file, sep = ";")

  # Separate the table to get the names of the hydrography90m variables
  file_size_table$file_name = basename(file_size_table$file_path)

  # Get the valid names of the hydrography variables
  # to check that the requested variable exists
  all_varnames <- sort(unique(sub("_[^_]+$", "",
                                    file_size_table$file_name)))

  # Get all file names. This is used in check_tiles_filesize() to
  # get the valid file_formats of the hydrography variables
  # to check that the requested variable exists
  all_file_names <- sort(unique(file_size_table$file_name))

  # Get the valid tile ids of the hydrography
  # to check that the requested tile exists
  all_tile_ids <- unique(str_extract(
    file_size_table$file_path, "h[0-9]+v[0-9]+"))

  all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]


  # Collect the file sizes of all requested variables and tiles together:
  variable_size_sum <- 0

  for (ivar in variable) {

    if (global == TRUE) {
      tile_id <-  "_ovr" # TODO: Tile_id is not always a tile_id! That is confusing. Spatial_unit_id?

    } else if (global == FALSE) {

      if (ivar == "regional_unit") {
        tile_id <- as.character(reg_unit_id)

      } else {
        tile_id <- tile_id
      }
    }

    # Collect the file sizes of all requested tiles for one variable:
    tile_size_sum <- 0

    for (itile in tile_id) {

      tile_size <- check_tiles_filesize(variable = ivar,
                                        file_format = file_format,
                                        tile_id = itile,
                                        global = global,
                                        h90m_varnames = all_varnames,
                                        h90m_tile_id = all_tile_ids,
                                        h90m_file_names = all_file_names,
                                        file_size_table = file_size_table)

      tile_size_sum <- tile_size_sum + tile_size

    }

    variable_size_sum <- tile_size_sum + variable_size_sum
  }

  # This is the added file sizes of all files to be downloaded:
  variable_size_sum

  # Print warning on file size and ask for input from the user
  # arg <- readline(prompt = paste0("Download size is ",
  #                                 round(variable_size_sum / 1000000, 2),
  #                                 " MB. Please type \"y\" if you are ready to smash it\n
  #                                 or \"n\" if you'd rather not to, and then press Enter \n"))
  print(paste0("Download size is ",
         round(variable_size_sum / 1000000, 2),
         " MB."))
  # if (arg == "y") {


    # The argument 'server_url' of the download_tiles_base() function controls
    # the server from which the files will be downloaded

    # General path to the download folder in Nimbus
    nimbus_path <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"
    # General path to the download folder in GDrive
    gdrive_path <- "https://drive.google.com/uc?export=download&id="

    # Download README file as a test to check whether Nimbus is up.
    # If Nimbus is not available, download README from GDrive to check
    # whether GDrive is up.

    server_url <- tryCatch(
      {
        download.file(paste0(nimbus_path, "README/README.txt"),
                      destfile = paste0(download_dir, "/README.txt"), mode = "wb")
        server_url <- nimbus_path
        server_url
      },
      warning = function(c) {

        # Get gdrive file id of the README.txt file
        readme_id <- file_size_table[
          file_size_table$file_name == "README.txt", ]$file_id
        # Download README.txt file
        download.file(paste0(gdrive_path, readme_id),
                      destfile = paste0(download_dir, "/README.txt"), mode = "wb")
        server_url <- gdrive_path
        server_url
      },
      error = function(c) {
        server_url <- gdrive_path
        server_url
      }
    )

    # Now perform the download:

    for (ivar in variable) {
      for (itile in tile_id) {

        download_tiles_base(variable = ivar, file_format = file_format,
                            tile_id = itile, global = global,
                            download_dir = download_dir,
                            file_size_table = file_size_table,
                            server_url = server_url
        )
      }
    }
    cat("Please cite the Hydrography90m publication:\n
        Amatulli, G., Garcia Marquez, J., Sethi, T., Kiesel, J., Grigoropoulou, A.,
        Üblacker, M. M., Shen, L. Q., and Domisch, S.: Hydrography90m: a new
        high-resolution global hydrographic dataset, Earth Syst. Sci. Data, 14,
        4525–4550, https://doi.org/10.5194/essd-14-4525-2022, 2022.")
  # }
}
