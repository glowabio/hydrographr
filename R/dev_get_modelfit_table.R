#' @title Get table with environmental variables at each occurrence
#' and pseudo absence point location
#'
#' @description Add describtion text here
#'
#'  Any thing the user needs to notice? add Note
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the species name and the longitude / latitude coordinates in WGS84.
#' @param spec character. The name of the column with the species names.
#' @param lon character. The name of the column with the longitude coordinates.
#' @param lat character. The name of the column with the latitude coordinates.
#' @param id character. The name of a column containing unique IDs for each row
#' of "data" (e.g., occurrence or site IDs).
#' @param pseudoabs
#' @param subc_id character. Full path to the .csv table with the
#' sub-catchment ID.
#' @param predict_table character. Full path of the predict.csv table
#' (i.e., output of get_predict_table); see also help(get_predict_table)
#' @param mod_fit_table character. Full path of the output.csv table,
#' i.e., the model fit table file.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom import needed R packages here
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
#' # add example here

get_modelfit_table <- function(data, spec, lon, lat, # id = NULL,
                               pseudoabs = NULL, subc_id, predict_table,
                               mod_fit_table, read = FALSE, quiet = TRUE) {

# Check operating system
sys_os <- get_os()
# Check if data.frame is defined
if (missing(data))
  stop("data: Input data.frame is missing.")

# Check if input data is of type data.frame,
# data.table or tibble
if (!is(data, "data.frame"))
  stop("data: Has to be of class 'data.frame'.")

# Check if rast_val and new_val is defined
if (missing(spec))
  stop("spec: Column name of species table is missing.")

# Check if lon, lat, side_id, and subc_id column names
# are character vectors
for (name in  c(lon, lat, id)) {
  if (!is.null(name))
    if (!is.character(name))
      stop(paste0("Column name ", name, " is not a character vector."))
}

# Check if lon, lat, id, and subc_id column names exist
for (name in c(lon, lat, id)) {
  if (!is.null(name))
    if (is.null(data[[name]]))
      stop(paste0("Column name '", name, "' does not exist."))
}

# Check if rast_val/new_val column names exist
if (is.null(data[[spec]]))
  stop(paste0("spec: Column name '", spec,
              "' does not exist."))
# Check if paths exists
for (path in c(subc_layer, predict_table)) {
  if (!file.exists(path))
    stop(paste0("File path: ", path, " does not exist."))
}

# Check if values of the rast_val/new_val columns are numeric
if (!is.integer(pseudoabs)
  stop(paste0("Pseudo absences have to be integers."))

# Check if raster_layer is defined
if (missing(predict_table))
  stop("predict_table: Path of the input predict table is missing.")

# Check if add object ends with add specific ending
if (!endsWith(raster_layer, ""))
  stop("add object: Input object is not a add ending file.")

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

  f (sys_os == "linux" || sys_os == "osx") {

# Call external GRASS GIS command r.reclass
processx::run(system.file("sh", "get_modelfit_table.sh",
                          package = "hydrographr"),
              args = c(coord_tmp_path, pseudoabs, subc_id,
                       predict_table , tempdir(), mod_fit_table),
              echo = !quiet)
  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_coord_tmp_path <- fix_path(coord_tmp_path)
    wsl_subc_id <- fix_path(subc_id)
    wsl_predict_table <- fix_path(predict_table)
    wsl_tmp_path <- fix_path(tempdir())
    wsl_mod_fit_table <- fix_path(mod_fit_table)
    wsl_sh_file <- fix_path(
      system.file("sh", "get_modelfit_table.sh",
                  package = "hydrographr"))

    processx::run(system.file("bat", "get_modelfit_table.bat",
                              package = "hydrographr"),
                  args = c(wsl_coord_tmp_path, wsl_subc_id,
                           wsl_predict_table, wsl_tmp_path, wsl_mod_fit_table,
                           wsl_sh_file, echo = !quiet))
  }


  # Remove all files in the tmp folder
  file.remove(coord_tmp_path)

  if (read == TRUE) {
    # Read reclassified .tif layer
    model_fit <- fread(mod_fit_table, header = TRUE)
    return(model_fit)
  }


}
