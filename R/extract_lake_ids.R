#' @title Extract lake ids
#'
#' @description Extract lake ids either at the location
#' of point occurrences or falling within a bounding box
#'
#' @param data a data.frame or data.table that contains the columns
#' the longitude / latitude coordinates in WGS84, i.e. of species point
#' occurrences
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param bbox logical. if TRUE, then all lake ids within the bounding box are
#' returned, note that either bounding box values can be supplied thorugh xmin,
#' ymin, xmax and ymax values or when left empty (NULL) bounding box is
#' calculated from min max values of the longitude and latitude values provided
#' in the occurrence table; if FALSE lake ids in which point occurrences are
#' located are returned, default is FALSE
#' @param xmin integer. bounding box coordinate check what is lower left corner in WGS84;
#' if NULL see parameter bbox
#' @param ymin integer. bounding box coordinate check what is lower right corner in WGS84;
#' if NULL see parameter bbox
#' @param xmax integer. bounding box coordinate upper right corner in WGS84;
#' if NULL see parameter bbox
#' @param ymax integer. bounding box coordinate upper left corner in WGS84;
#' if NULL see parameter bbox
#' @param var_name character. Name of the shape file ID column, i.e. "Hylak_id"
#' for HydroLAKES; default is "Hylak_id"
#' @param lake_shape Full path to lake shape files; i.e. HydroLAKES shapefiles;
#' when NULL a lake raster file needs to be provided
#' @param lake_id_table character. Full path of the output lake id table
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @export
#'
#' #' @details
#' For the extraction of a value at a given point location from the basin
#' and/or sub-catchment raster layer of the Hydrography90m dataset, the GDAL
#' function 'gdallocationinfo' is used. The point locations have to be defined
#' by coordinates in the WGS84 reference system.
#'
#' @author Jaime Garcia Marquez, Thomas Tomiczek
#'
#' @references
#' https://grass.osgeo.org/grass82/manuals/
#' \url{https://gdal.org/programs/gdallocationinfo.html}
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Example 1: Extracts lake IDs for points located within a lake
#'
#' # Load occurrence data
#'
#' species_occurrence <- read.table(paste0(my_directory,
#'                                      "/hydrography90m_test_data",
#'                                      "/spdata_1264942.txt"),
#'                                      header = TRUE)
#'
#'
#' extract_lake_ids(data = species_occurrence,
#'                  lon = "longitude",
#'                  lat = "latitude",
#'                  lake_shape = paste0(my_directory, "lakes_corsica.shp"),
#'                  lake_id_table = output_folder,
#'                  quiet = FALSE)
#'
# extract_lake_ids(data = species_occurrence,
#'                 lon = "longitude",
#'                 lat = "latitude",
#'                 var_name = "lake_id",
#'                 lake_shape = paste0(my_directory, "lakes_corsica.shp"),
#'                 lake_id_table = output_folder,
#'                 quiet = FALSE)
#'
#' extract_lake_ids(data = species_occurrence,
#'                  lon = "longitude",
#'                  lat = "latitude",
#'                  xmin = 9.498263,
#'                  ymin = 44.21651,
#'                  xmax = 29.71806,
#'                  ymax = 50.25584,
#'                  var_name = "lake_id",
#'                  lake_shape = paste0(my_directory, "lakes_corsica.shp"),
#'                  bbox = TRUE,
#'                  lake_id_table = output_folder,
#'                  quiet = FALSE)

extract_lake_ids <- function(data, lon, lat, lake_shape,
                             var_name = "Hylak_id", xmin = NULL,
                             ymin = NULL, xmax = NULL, ymax = NULL,
                             bbox = FALSE, lake_id_table, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if lon, lat in occurrence point table exists
  # are character vectors
  for (name in  c(lon, lat)) {
    if (!is.null(name))
      if (!is.character(name))
        stop(paste0("Column name ", name, " is not a character vector."))
  }

  # Check if paths exists
  for (path in c(lake_id_table)) {
    if (!file.exists(path))
      stop(paste0("File path: ", path, " does not exist."))
  }

  # Check if bbox is logical
  if (!is.logical(bbox))
    stop("bbox: Has to be TRUE or FALSE.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")



  # if provided use coordinates to extract lake IDs
  if (!is.null(xmax) && !is.null(xmin) && !is.null(ymax) && !is.null(ymin)
      && isTRUE(bbox)) {
    xmax <- xmax
    xmin <- xmin
    ymax <- ymax
    ymin <- ymin

    # else extract coordinates from the extent of species occurrence points
  } else {

    longi <- data$lon
    lati <- data$lat

    xmax <- max(longi)
    xmin <- min(longi)
    ymax <- max(lati)
    ymin <- min(lati)

  }

  longi <- data$lon
  lati <- data$lat

  # convert table format to text files for the shell to run
  # Create random string to attach to the file name of the temporary
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  # Select columns with lon/lat coordinates
  # Remove duplicated rows across entire data frame
  coord <- data.table(longitude = longi, latitude = lati)
  coord <- coord[!duplicated(coord), ]

  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".txt")

  ## Note:Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # write the extent of the bounding box into one variable for the shell script
  bbox_coord <- data.frame(
    extent = c("xmin", "ymin", "xmax", "ymax"),
    value = c(xmin, ymin, xmax, ymax)
  )

  bbox_coord_tmp_path <- paste0(tempdir(), "/bbox_coordinates_", rand_string, ".txt")
  fwrite(bbox_coord, bbox_coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {


    # Call the external .sh script extract_ids() containing the gdal function
    processx::run(system.file("sh", "extract_lake_ids.sh", package = "hydrographr"),
                  args = c(coord_tmp_path, lon, lat, lake_shape, bbox_coord_tmp_path,
                           var_name, bbox, tempdir()), lake_id_table,
                  echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_lake_shape <- ifelse(is.null(lake_shape), 0, fix_path(lake_shape))
    wsl_bbox_coord_tmp_path <- fix_path(bbox_coord_tmp_path)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_lake_id_table <- fix_path(lake_id_table)
    wsl_sh_file <- fix_path(
                            system.file("sh", "extract_lake_ids.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "extract_lake_ids.bat",
                              package = "hydrographr"),
                  args = c(wsl_coord_tmp_path, lon, lat, wsl_lake_shape,
                           wsl_bbox_coord_tmp_path,
                           var_name, bbox, wsl_tmp_path, wsl_lake_id_table,
                           wsl_sh_file, echo = !quiet))

  }


  # Remove all files in the tmp folder
  file.remove(coord_tmp_path, bbox_coord_tmp_path)

  # Return data frame
  # Read in file containing lake ids setting fill=TRUE, in case that
  # some coordinates are located in null cells and did not get an ID
  lake_ids <- fread(paste0(lake_id_table, "/lake_id.txt"),
                    keepLeadingZeros = TRUE, header = TRUE, sep = " ",
                    fill = TRUE)
  return(lake_ids)

}
