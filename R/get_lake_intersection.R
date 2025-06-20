#' @title Get intersection points between Hydrography90m stream network and lake shapefiles
#'
#' @description Add describtion text here
#'
#'  Any thing the user needs to notice? add Note
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the lake ids, i.e. HydroLAKES ids.
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
#' (i.e., output of get_predict_table); see also help(get_predict_table)
#' @param stream character. Full path to Hydrography 90m stream network tif file
#' add a reference were to check the necessary tif files for lake analysis
#' @param flow character. Full path to Hydrography 90m flow accumulation tif file
#' @param glbasins character. Full path to Hydrography 90m basin tif file
#' @param lake_dat character. Full path of the output.csv table,
#' i.e., the lake intersection table.
#' @param n_cores interger. Number of cores used in parallelzation
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
#' add manual of the MSPA analysis tool here
#'
#'
#' @examples
#' # add example here
#' # Download HydroLAKES shapefiles from their website and test with
#' hydrolakes.sh instead I always transformed it before to lake.gpkg
#' # write the script to run in paralell using ncores

get_lake_intersection <- function(data, lake_id = "HydroLAKES_polys_v10", lakes,
                                  lake_name, buffer = TRUE, edge, stream, flow,
                                  glbasins, glcompunits, cubasins, lake_dat,
                                  n_cores, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

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

# writeLines(mspa_content, paste0(tempdir(), "/mspa-parameters.txt"))
writeLines(mspa_content, paste0(edge,"input/", "mspa-parameters.txt"))

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

  # Print the resulting data.frame
  # print(result)

  # write a line of code to transform number in meters to units in degree
  if (is.numeric(buffer) && length(buffer) > 0) {
    buffer <- (buffer * 0.000009)
  }

  ### be sure to convert csv files into txt files for the sh script to work! ###

  # Create random string
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Select columns with lake ids
  columns <- c(lake_id)
  id_dat <- as.data.table(data)[, ..columns]
  # Remove duplicated rows across entire data frame
  id_dat <- id_dat[!duplicated(id_dat), ]

  # Path for tmp ids.txt file
  ids_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".txt")
  ## write to tempory file to convert to txt file
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
                           edge, stream, flow, glbasins,
                           tempdir(), lake_dat, n_cores),
                  echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_subc_layer <- ifelse(is.null(subc_layer), 0,
                             fix_path(subc_layer))
    wsl_bas_path <- ifelse(is.null(basin_layer), 0, fix_path(basin_layer))
    wsl_tmp_path <- fix_path(tempdir())
    wsl_ids_tmp_path <- fix_path(ids_tmp_path)
    wsl_sh_file <- fix_path(
      system.file("sh", "extract_ids.sh",
                  package = "hydrographr"))

    processx::run(system.file("bat", "extract_ids.bat",
                              package = "hydrographr"),
                  args = c(wsl_coord_tmp_path, lon, lat, wsl_subc_layer,
                           wsl_bas_path, wsl_tmp_path, wsl_ids_tmp_path,
                           wsl_sh_file, echo = !quiet))

  }
  # Read in the file containing the ids setting fill=TRUE, for the case that
  # some coordinates were in null cells so they did not get an ID
  data_ids <- fread(paste0(tempdir(),  "/ids_", rand_string, ".txt"),
                    keepLeadingZeros = TRUE, header = TRUE, sep = " ",
                    fill = TRUE)

  # Remove all files in the tmp folder
  file.remove(ids_tmp_path)

  # Return data frame
  return(data_ids)

}
