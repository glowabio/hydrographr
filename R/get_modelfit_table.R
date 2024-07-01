#' @title Get environmental variables at each occurrence
#' and pseudo-absence point location
#'
#' @description Get the environmental variables for each
#' species occurrences and pseudo-absences at given point locations
#' by extracting the environmental information from
#' the prediction table produced from the get_predict_table function
#' see also help(get_predict_table).
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the species name and the longitude / latitude coordinates in WGS84.
#' @param spec character. The name of the column with the species names.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param pseudoabs integer. number of pseudo-absences
#' @param subc character. Full path to the sub-catchment .tif file with the
#' sub-catchment ID.
#' @param predict_table character. Full path of the predict.csv table
#' (i.e., output of get_predict_table); see also help(get_predict_table)
#' @param mod_fit_table character. Full path of the output.csv table,
#' i.e., the model fit table file.
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
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load occurrence data
#' species_occurrence <- read.table(paste0(my_directory,
#'                                         "/hydrography90m_test_data",
#'                                         "/spp.csv"),
#'                                  header = TRUE)
#'
#' # Define full path to the sub-catchments raster layer
#' subc_raster <- paste0(my_directory,
#'                      "/hydrography90m_test_data/sub_catchment_h10v06.tif")
#'
#' # Define full path to the prediction table
#' predict_tbl <- paste0(my_directory,
#'                      "/hydrography90m_test_data/projectionTB.csv")
#'
#' # Define full path to the output model fit table
#' model_fit <- paste0(my_directory,
#'                      "/hydrography90m_test_data/model_table.csv")
#'
#' # Get table with environmental variables at each occurrence
#' # and pseudo-absence point location
#' modelfit_table <- get_modelfit_table(data = species_occurrence,
#'                                   spec = "species",
#'                                   lon = "long",
#'                                   lat = "lat",
#'                                   pseudoabs = 10000,
#'                                   subc = subc_raster,
#'                                   predict_table =  predict_tbl,
#'                                   mod_fit_table = model_fit)

get_modelfit_table <- function(data, spec, lon, lat,
                               pseudoabs = NULL, subc, predict_table,
                               mod_fit_table, read = TRUE, quiet = TRUE) {

  # Check if data.frame is defined
  if (missing(data))
    stop("data: Input data.frame is missing.")

  # Check if rast_val and new_val is defined
  if (missing(spec))
    stop("spec: Column name of species table is missing.")

  # Check if rast_val/new_val column names exist
  if (is.null(data[[spec]]))
    stop(paste0("spec: Column name '", spec,
                "' does not exist."))
  # Check if paths exists
  for (path in predict_table) {
    if (!file.exists(path))
      stop(paste0("File path: ", path, " does not exist."))
  }

  # Check if raster_layer is defined
  if (missing(predict_table))
    stop("predict_table: Path of the input predict table is missing.")

  # Check if add missing object is defined
  if (missing(mod_fit_table))
    stop("mod_fit_table: Path for the output model fit table is missing.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Create random string to attach to the file name of the temporary
  # points_dataset.txt and ids.txt file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  columns <- c(spec, lon, lat)
  coord <- as.data.table(data)[, ..columns]

  # Remove duplicated rows across entire data frame
  coord <- coord[!duplicated(coord), ]

  # Export taxon occurrence points
  coord_tmp_path <- paste0(tempdir(), "/coordinates_", rand_string, ".csv")

  ## Note:Only export lon/lat column
  fwrite(coord, coord_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = ",")

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    # Call external GRASS GIS command r.reclass
    processx::run(system.file("sh", "get_modelfit_table.sh",
                              package = "hydrographr"),
                  args = c(coord_tmp_path, predict_table, subc,
                           pseudoabs, tempdir(), mod_fit_table),
                  echo = !quiet)
  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_subc <- fix_path(subc)
    wsl_predict_table <- fix_path(predict_table)
    wsl_pseudoabs <- fix_path(pseudoabs)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_mod_fit_table <- fix_path(mod_fit_table)
    wsl_sh_file <- fix_path(
                            system.file("sh", "get_modelfit_table.sh",
                                        package = "hydrographr"))

    processx::run(system.file("bat", "get_modelfit_table.bat",
                              package = "hydrographr"),
                  args = c(wsl_coord_tmp_path, wsl_predict_table,
                           wsl_subc, wsl_pseudoabs, wsl_tmp_path,
                           wsl_mod_fit_table, wsl_sh_file, echo = !quiet))
  }

  # Remove all files in the tmp folder
  file.remove(coord_tmp_path)

  if (read == TRUE) {
    # Read model_fit_table
    model_fit <- fread(mod_fit_table, header = TRUE, fill = TRUE)
    return(model_fit)
  }


}
