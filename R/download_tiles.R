#' @title Download files of the Hydrography90m dataset
#'
#' @description The function downloads files of the Hydrography90m
#' dataset, available at https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F .
#' The files will be stored in the folder architecture of the above domain.
#' Multiple regular tile or regional unit files can be requested in a single
#' call of the function. The tile or regional unit IDs can be obtained
#' using the functions "get_tile_id" and "get_regional_unit_id" respectively.
#'
#' @param variable character vector of variable names.
#' @param file_format character. Format of the requested file ("tif" or "gpkg").
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
#' dataset. https://doi.org/10.18728/igb-fred-762.1
#'
#' @details
#' | **Variable type**    | **Variable**                             | **Abbreviation**      | **Unit** | **Description**                                                                                     |
#'   |----------------------|------------------------------------------|-----------------------|----------|-----------------------------------------------------------------------------------------------------|
#'   | Flow                 | Flow accumulation                        | flow                  | km²      | Flow accumulation                                                                                   |
#'   | Flow                 | Flow accumulation (positive values)      | flowpos               | km²      | Flow accumulation with positive values                                                              |
#'   | Stream slope         | Cell maximum curvature                   | slope_curv_max_dw_cel | m⁻¹      | Cell maximum curvature (between highest upstream cell,focal cell and downstream cell)               |
#'   | Stream slope         | Cell minimum curvature                   | slope_curv_min_dw_cel | m⁻¹      | Cell minimum curvature(between lowest upstream cell,focal cell and downstream cell)                 |
#'   | Stream slope         | Cell elevation difference                | slope_elv_dw_cel      | m        | Cell elevation difference(between focal cell and downstream cell)                                   |
#'   | Stream slope         | Cell gradient                            | slope_grad_dw_cel     |          | Cell gradient                                                                                       |
#'   | Stream distance      | Shortest distance to drainage divide     | stream_dist_up_near   | m        | Shortest upstream distance between focal grid cell and the nearest sub-catchment drainage divide    |
#'   | Stream distance      | Longest distance to drainage divide      | stream_dist_up_farth  | m        | Longest upstream distance between focal grid cell and the nearest sub-catchment drainage divide     |
#'   | Stream distance      | Nearest down stream stream grid cell     | stream_dist_dw_near   | m        | Distance between focal grid cell and its nearest down stream stream grid cell                       |
#'   | Stream distance      | Outlet grid cell in the network          | outlet_dist_dw_basin  | m        | Distance between focal grid cell and the outlet grid cell in the network                            |
#'   | Stream distance      | Down stream stream node grid cell        | outlet_dist_dw_scatch | m        | Distance between focal grid cell and the down stream stream node grid cell                          |
#'   | Stream distance      | Euclidean distance                       | stream_dist_proximity | m        | Euclidean distance between focal grid cell and the stream network (in meters)                       |
#'   | Elevation difference | Shortest path                            | stream_diff_up_near   | m        | Elevation difference of the shortest path from focal grid cell to the sub-catchment drainage divide |
#'   | Elevation difference | Longest path                             | stream_diff_up_farth  | m        | Elevation difference of the longest path from focal grid cell to the sub-catchment drainage divide  |
#'   | Elevation difference | Nearest downstream stream pixel          | stream_diff_dw_near   | m        | Elevation difference between focal grid cell and its nearest downstream stream pixel                |
#'   | Elevation difference | Outlet grid cell in the network          | outlet_diff_dw_basin  | m        | Elevation difference between focal grid cell and the outlet grid cell in the network                |
#'   | Elevation difference | Downstream stream node grid cell         | outlet_diff_dw_scatch | m        | Elevation difference between focal grid cell and the downstream stream node grid cell               |
#'   | Segment properties   | Segment downstream mean gradient         | channel_grad_dw_seg   |          | Segment downstream mean gradient (between focal cell and the node/outlet)                           |
#'   | Segment properties   | Segment upstream mean gradient           | channel_grad_up_seg   |          | Segment upstream mean gradient (between focal cell and the init/node)                               |
#'   | Segment properties   | Cell upstream gradient                   | channel_grad_up_cel   |          | Cell upstream gradient (between focal cell and next cell                                            |
#'                                                                                                                                    | Segment properties   | Cell stream course curvature             | channel curv_cel      |          | Cell stream course curvature (focal cell)                                                           |
#'                                                                                                                                    | Segment properties   | Segment downstream elevation difference  | channel_elv_dw_seg    |          | Segment downstream elevation difference (between focal cell and the node/outlet)                    |
#'                                                                                                                                    | Segment properties   | Segment upstream elevation difference    | channel_elv_up_seg    |          | Segment upstream elevation difference (between focal cell and the init/node)                        |
#'                                                                                                                                    | Segment properties   | Cell upstream elevation difference       | channel_elv_up_cel    |          | Cell upstream elevation difference (between focal cell and next cell)                               |
#'                                                                                                                                    | Segment properties   | Cell downstream elevation difference     | channel_elv_dw_cel    |          | Cell downstream elevation difference (between focal cell and next cell)                             |
#'                                                                                                                                    | Segment properties   | Segment downstream distance              | channel_dist_dw_seg   |          | Segment downstream distance (between focal cell and the node/outlet                                 |
#'                                                                                                                                                                                                                                                                          | Segment properties   | Segment upstream distance                | channel_dist_up_seg   |          | Segment upstream distance (between focal cell and the init/node)                                    |
#'                                                                                                                                                                                                                                                                          | Segment properties   | Cell upstream distance                   | channel_dist_up_cel   |          | Cell upstream distance (between focal cell and next cell)                                           |
#'                                                                                                                                                                                                                                                                          | Stream order         | Strahler’s stream order                  | stream_strahler       |          | Strahler’s stream order                                                                             |
#'                                                                                                                                                                                                                                                                          | Stream order         | Shreve’s stream magnitude                | stream_shreve         |          | Shreve’s stream magnitude                                                                           |
#'                                                                                                                                                                                                                                                                          | Stream order         | Horton’s stream order                    | stream_horton         |          | Horton’s stream order                                                                               |
#'                                                                                                                                                                                                                                                                          | Stream order         | Hack’s stream order                      | stream_hack           |          | Hack’s stream order                                                                                 |
#'                                                                                                                                                                                                                                                                          | Stream order         | Topological dimension of streams         | stream_topo           |          | Topological dimension of streams                                                                    |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Length of the stream reach               | length                |          | Length of the stream reach                                                                          |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Straight length                          | stright               |          | Length of stream as straight line                                                                   |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Sinusoid of the stream reach             | sinusoid              |          | Fractal dimension: stream length/straight stream length                                             |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Accumulated length                       | cum_length            |          | Length of stream from source                                                                        |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Flow accumulation (?)                    | flow_accum            |          | Flow accumulation (what is the difference to the one above???)                                      |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Distance to outlet                       | out_dist              |          | Distance of current stream init from outlet                                                         |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Source elevation                         | source_elev           |          | Elevation of stream init                                                                            |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Outlet elevation                         | outlet_elev           |          | Elevation of stream outlet                                                                          |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Elevation drop                           | elev_drop             |          | Difference between source_elev and outlet_elev + drop outlet                                        |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Outlet drop                              | out_drop              |          | Drop at the outlet of the stream                                                                    |
#'                                                                                                                                                                                                                                                                          | Stream reach         | Gradient                                 | gradient              |          | Mean gradient of the sub-catchment (downstream elevation difference divided by distance)            |
#'                                                                                                                                                                                                                                                                          | Flow index           | Stream power index                       | spi                   |          | A measure of the erosive power of flowing water (Moore et al., 1991)                                |
#'                                                                                                                                                                                                                                                                          | Flow index           | Sediment transportation index            | sti                   |          | A metric for describing the erosion and deposition of sediments (Mojaddadi et al., 2017)            |
#'                                                                                                                                                                                                                                                                          | Flow index           | Compound topographic index               | cti                   |          | "A steady state wetness index, also known as topographic wetness index (TWI) (Beven & Kirkby,       |
#' | 1979)"               |                                          |                       |          |                                                                                                     |

#' @md
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



download_tiles <- function(variable, file_format = "tif",
                           tile_id = NULL, reg_unit_id = NULL,
                           global = FALSE, download_dir = ".") {

  # Introductory steps

  # Set timeout option for download to 1 hour (3600 seconds)
  options(timeout=3600)

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
  file_size_table_sep <- separate(
    data = file_size_table,
    col = file_path,
    into = c("grass_module", "foldername", "varname_tile"),
    sep = "/",
    fill = "left",
  )

  file_size_table_sep$grass_module[
    is.na(file_size_table_sep$grass_module)] <- ""
  # Get the valid names of the hydrography variables
  # to check that the requested variable exists
  h90m_varnames <- sort(unique(sub("_[^_]+$", "",
                                    file_size_table_sep$varname_tile)))
  # Get the valid file_formats of the hydrography variables
  # to check that the requested variable exists
  h90m_file_formats <- sort(unique(file_size_table_sep$varname_tile))

  # Get the valid tile ids of the hydrography
  # to check that the requested tile exists
  h90m_tile_id <- unique(str_extract(
    file_size_table$file_path, "h[0-9]+v[0-9]+"))

  h90m_tile_id <- h90m_tile_id[!is.na(h90m_tile_id)]

  variable_size_sum <- 0

  for (ivar in variable) {


    if (global == TRUE) {
      tile_id <-  "_ovr"

    } else if (global == FALSE) {

      if (ivar == "regional_unit") {

        tile_id <- as.character(reg_unit_id)

      } else {
        tile_id <- tile_id
      }

    }


    tile_size_sum <- 0

    for (itile in tile_id) {

      tile_size <- check_tiles_filesize(variable = ivar,
                                        file_format = file_format,
                                        tile_id = itile,
                                        global = global,
                                        h90m_varnames = h90m_varnames,
                                        h90m_tile_id = h90m_tile_id,
                                        h90m_file_formats = h90m_file_formats,
                                        file_size_table_sep = file_size_table_sep)

      tile_size_sum <- tile_size_sum + tile_size

    }

    variable_size_sum <- tile_size_sum + variable_size_sum
  }

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

    # Use README file as a test to check if Nimbus is up.
    server_url <- tryCatch(
      {
        download.file(paste0(nimbus_path, "README/README.txt"),
                      destfile = paste0(download_dir, "/README.txt"), mode = "wb")
        server_url <- nimbus_path
        server_url
      },
      warning = function(c) {
        # Get gdrive file id of the README.txt file
        readme_id <- file_size_table_sep[
          file_size_table_sep$varname_tile == "README.txt", ]$file_id
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

    for (ivar in variable) {
      for (itile in tile_id) {

        download_tiles_base(variable = ivar, file_format = file_format,
                            tile_id = itile, global = global,
                            download_dir = download_dir,
                            file_size_table_sep = file_size_table_sep,
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
