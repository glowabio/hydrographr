#' @title Get intersection points between Hydrography90m stream network and lake shapefiles
#'
#' @description The function identifies, for each lake ID, the intersection points
#' between the stream network (i.e., Hydrography90m) and the corresponding
#' lake's geospatial file (e.g., HydroLAKES shapefile).
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the lake ids, i.e. HydroLAKES ids. (i.e., output of extract_lake_ids);
#' see also help(extract_lake_ids)
#' @param lake_id character. The name of the column containing lake ids.
#' @param lakes character. Full path to lake geo-spatial files; i.e. HydroLAKES shapefiles.
#' @param lake_name character. The name of the attribute table column in
#' the geo-spatial files containing the lake ids; i.e. "HydroLAKES_polys_v10"
#' for HydroLAKES shapefiles.
#' @param buffer character. users can either set buffer to TRUE, FALSE or provide
#' buffer size in meters;
#' if TRUE a predefined buffer is used where the size of the buffer depends on
#' the lake size; if FALSE no buffer is applied; Default is TRUE
#' @param edge character. Full path to GuidosToolbox Workbench MSPA tool
#' see also (https://forest.jrc.ec.europa.eu/en/activities/lpa/gtb/)
#' @param stream character. Full path to Hydrography90m stream network tif file
#' add a reference were to check the necessary tif files for lake analysis
#' @param flow character. Full path to Hydrography90m flow accumulation tif file
#' @param basins character. Full path to Hydrography90m basin tif file
#' @param lake_dat character. Full path of the output.csv table,
#' i.e., the lake intersection table.
#' @param n_cores interger. Number of cores used in parallelization, Default is one.
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
#' https://grass.osgeo.org/grass82/manuals/
#' Soille P. and Vogt P. (2009). Morphological segmentation of binary patterns.
#' Pattern Recognition Letters 30, 4:456-459, doi: 1.1016/j.patrec.2008.10.015
#' \url{https://ies-ows.jrc.ec.europa.eu/gtb/GTB/MSPA_Guide.pdf}
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Note: For this we need the lake IDs (i.e. HydroLake ID), extracted from the
#' # extract_lake_ids.R function; see also help(extract_lake_ids)
#'
#' data <- fread(paste0(my_directory,
#'                          "/hydrography90m_test_data",
#'                          "/lake_id.txt"),
#'                          header = TRUE)
#' lake_id <- "lake_id"
#'
#' lakes <- (paste0(my_directory,
#'                     "/hydrography90m_test_data",
#'                     "/hydrography90m_test_lakes.gpkg"))
#'
#' # To run the function we need to have installed GuidosToolbox Workbench MSPA tool
#' # see also (https://forest.jrc.ec.europa.eu/en/activities/lpa/gtb/) and
#' # (https://glowabio.github.io/hydrographr/articles/case_study_lake_workflow.html)
#' # give full path virtual machine linux path
#' edge <- (paste0(/home/USER/GWB_version/,
#'                     "/GWB"))
#'
#' stream <- (paste0(my_directory,
#'                     "/hydrography90m_test_data",
#'                     "/stream_1264942.tif"))
#'
#' flow <- (paste0(my_directory,
#'                     "/hydrography90m_test_data",
#'                     "/flow_1264942.tif"))
#'
#' basins <- (paste0(my_directory,
#'                     "/hydrography90m_test_data",
#'                     "/basin_1264942.tif"))
#'
#' lake_dat <- (paste0(my_directory,
#'                     "/hydrography90m_test_data"))
#'
#' lake_intersect_table <- get_lake_intersection(data, lake_id = "lake_id",
#'                             lakes, lake_name = "lake_id", buffer = TRUE, edge,
#'                             stream, flow, basins, lake_dat, n_cores = 1,
#'                             quiet = FALSE)


get_lake_intersection <- function(data, lake_id = "HydroLAKES_polys_v10", lakes,
                                  lake_name, buffer = TRUE, edge, stream, flow,
                                  basins, lake_dat,
                                  n_cores = 1, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if paths exists
  if (!file.exists(lakes))
    stop(paste0("Please provide the path to lake geo-spatial files"))

  # Check if paths exists
  if (is.null(edge))
    stop(paste0("Please provide the path to GuidosToolbox Workbench MSPA tool"))

  # Check if paths exists
  if (!file.exists(stream))
    stop(paste0("Please provide the path to the stream raster file"))

  # Check if paths exists
  if (!file.exists(flow))
    stop(paste0("Please provide the path to the flow raster file"))

  # Check if paths exists
  if (!file.exists(basins))
    stop(paste0("Please provide the path to the basin raster file"))

  # Check if paths exists
  if (!file.exists(lake_dat))
    stop(paste0("Please provide the output path to store intersection tables"))

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # create file required by GuidosToolbox Workbench MSPA tool
mspa_content <- ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GWB_MSPA parameter file:
;; NOTE: do NOT delete or add any lines in this parameter file!
;;
;; MSPA: Morphological Spatial Pattern Analysis (up to 25 classes)
;; Input image requirements: 1b-background, 2b-foreground, optional: 0b-missing
;;
;; MSPA will provide an image and summary statistics.
;; (see tools/docs/MSPA_Guide.pdf for details)
;; Please specify entries at lines 27-32 ONLY using the following options:
;;
;; line 27: MSPA parameter 1: Foreground connectivity: 8 (default) or 4
;; line 28: MSPA parameter 2: EdgeWidth: 1 (default) or larger integer values
;; line 29: MSPA parameter 3: Transition: 1 (default) or 0
;; line 30: MSPA parameter 4: IntExt: 1 (default) or 0
;; line 31: disk: 0 (default) or 1 (requires 20% less RAM but +40% processing time)
;; line 32: statistics: 0 (default) or 1 (add summary statistics)
;;
;; a parameter file with the default settings would look like this:
;; 8
;; 1
;; 1
;; 1
;; 0
;; 0
****************************************************************************
8
1
0
1
0
0
****************************************************************************"

  writeLines(mspa_content, paste0(tempdir(), "/mspa-parameters.txt"))

  values <- rep(0, 256) # Create a vector of 256 zeros

  # Set the specific indices to 1
  indices <- c(3, 33, 67, 103) # These are the positions you want to set to 1
  values[indices + 1] <- 1 # +1 because R indexing starts at 1

  # Create the data.frame
  result <- data.frame(
    index = 0:255,   # Create an index column from 0 to 255
    value = values   # Assign the values vector
  )

  fwrite(result, paste0(tempdir(), "/mspa_reclass_code.txt"), col.names = FALSE,
         row.names = FALSE, quote = FALSE, sep = " ")


  # write a line of code to transform meters to units in degree
  if (is.numeric(buffer) && length(buffer) > 0) {
    buffer <- (buffer * 0.000009)
  }

  # Create random string
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Select columns with lake ids
  columns <- c(lake_id)
  id_dat <- as.data.table(data)[, ..columns]
  # Remove duplicated rows across entire data frame
  id_dat <- id_dat[!duplicated(id_dat), ]

  # Path for tmp ids.txt file
  ids_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".txt")

  # write to tempory file to convert to txt file
  fwrite(id_dat, ids_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    # Call the external .sh script extract_ids() containing the gdal function
    processx::run(system.file("sh", "get_lake_intersection.sh", package = "hydrographr"),
                  args = c(ids_tmp_path, lake_id, lakes, lake_name, buffer,
                           edge, stream, flow, basins,
                           tempdir(), lake_dat, n_cores),
                  echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_ids_tmp_path <- fix_path(ids_tmp_path)
    wsl_lakes_path <- fix_path(lakes)
    wsl_stream_path <- fix_path(stream)
    wsl_flow_path <- fix_path(flow)
    wsl_basins_path <- fix_path(basins)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_lake_dat_path <- fix_path(lake_dat)
    wsl_sh_file <- fix_path(
                            system.file("sh", "get_lake_intersection.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "get_lake_intersection.bat",
                              package = "hydrographr"),
                  args = c(wsl_ids_tmp_path, lake_id, wsl_lakes_path, lake_name,
                           buffer, edge, wsl_stream_path, wsl_flow_path,
                           wsl_basins_path, wsl_tmp_path, wsl_lake_dat_path,
                           n_cores, wsl_sh_file, echo = !quiet))

  }
  # Return message
  print(paste0("Lake intersection tables are stored under", lake_dat))


}
