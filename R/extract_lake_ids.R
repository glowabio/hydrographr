#' @title Extract lake ids
#'
#' @description Extract lake ids either at the location
#' of point occurrences or falling within a bounding box
#'
#'  Any thing the user needs to notice? add Note
#'
#' @param data a data.frame or data.table that contains the columns
#' the longitude / latitude coordinates in WGS84, i.e. of species point
#' occurrences
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param xmin integer. bounding box coordinate check what is lower left corner in WGS84; if NULL see parameter bbox
#' @param ymin integer. bounding box coordinate check what is lower right corner in WGS84; if NULL see parameter bbox
#' @param xmax integer. bounding box coordinate upper right corner in WGS84; if NULL see parameter bbox
#' @param ymax integer. bounding box coordinate upper left corner in WGS84; if NULL see parameter bbox
#' for HydroLAKES; default is "Hylak_id"
#' @param lake_shape Full path to lake shape files; i.e. HydroLAKES shapefiles; when NULL a lake raster file needs to be provided
#' @param var_name character. Name of the shapefile ID column, i.e. "Hylak_id"
#' for HydroLAKES; default is "Hylak_id"
#' @param bbox logical. if TRUE, then all lake ids within the bounding box (xmin, ymin, xmax and ymax) are returned,
#' if TRUE and no xmin, ymin, xmax and ymax values are provided then the bounding box is the spatial extent min max values of the longitude and latitude values provided in the species occurrence table;
#' if FALSE lake ids in which point occurrences are located are returned, default is FALSE
#' @param lake_id_table character. Full path of the output lake id table
#' @param read logical. If TRUE, then the model .csv table
#' gets read into R as data.table and data.frame.
#' if FALSE, the table is only stored on disk. Default is FALSE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @export
#'
#' @author Jaime Garcia Marquez, Thomas Tomiczek
#'
#' @references
#' add reference manual html here
#' https://grass.osgeo.org/grass82/manuals/
#'
#'
#' @examples
#' # Download hydrolakes shape files from their website and test with hydrolakes.shp instead I always transformed it before to lake.gpkg

extract_lake_ids <- function(data, lon, lat, xmin = NULL, ymin = NULL,
                             xmax = NULL, ymax = NULL,
                             lake_shape, var_name = "Hylak_id", bbox = FALSE,
                             lake_id_table, quiet = TRUE) {

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

  if (!is.null(xmax) && !is.null(xmin) && !is.null(ymax) && !is.null(ymin) && isTRUE(bbox)) {

  bbox <- 1

  } else if (isTRUE(bbox)) {

  bbox <- 1

  longi <- data$lon
  lati <- data$lat

  xmax <- max(longi)
  xmin <- min(longi)
  ymax <- max(lati)
  ymin <- min(lati)

  } else {

  bbox <- 0

  longi <- data$lon
  lati <- data$lat

  xmax <- max(longi)
  xmin <- min(longi)
  ymax <- max(lati)
  ymin <- min(lati)
  }

  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  # columns <- c(lon, lat)
  coord <- as.data.table(data)
  # Remove duplicated rows across entire data frame
  coord <- coord[!duplicated(coord), ]

  # coord <- data.table(longitude = longi, latitude = lati)
  # coord <- coord[!duplicated(coord), ]
  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".txt")
  ## Note:Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    # Call the external .sh script extract_ids() containing the gdal function
    processx::run(system.file("sh", "extract_lake_ids.sh", package = "hydrographr"),
                  args = c(lake_shape, xmin, ymin, xmax, ymax, tempdir(), var_name, lon, lat,
                           coord_tmp_path, bbox, lake_id_table),
                           echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_lake_shape <- ifelse(is.null(lake_shape), 0, fix_path(lake_shape))
    wsl_tmp_path <- fix_path(tempdir())
    wsl_sh_file <- fix_path(
      system.file("sh", "extract_lake_ids.sh",
                  package = "hydrographr"))

    processx::run(system.file("bat", "extract_lake_ids.bat",
                              package = "hydrographr"),
                  args = c(wsl_lake_shape, xmin,
                           ymin, xmax, ymax, wsl_tmp_path,
                           wsl_sh_file, echo = !quiet))

  }

  file_path <- paste0(lake_id_table, "/lake_ids", ".txt")

  print(file_path)

  lake_ids <- fread(file_path,
                   keepLeadingZeros = TRUE, header = TRUE, sep = " ",
                   fill = TRUE)

  # Return data frame
  ## check fread data_table e.g. lake_ids <- fread(OUTDIR/lake_rand_string)
  return(lake_ids)
}
