#' Reclassification of an integer raster layer
#'
#' Reclassifies an integer raster .tif layer using the r.reclass function of
#' GRASS GIS. To reclassify the raster layer the present raster values
#' and the new raster values have to be defined.
#'
#' @param data data.frame with the present and new raster values
#' @param rast_val Column name of present integer raster values as character
#' string.
#' @param new_val Column name of new raster integer values as character string.
#' @param rast_path Full path to the integer input raster .tif layer.
#' If the input raster layer has floating point values,
#' you should multiply the input data
#' by some factor to achieve integer values, otherwise the GRASS GIS r.reclass
#' will round the raster values down to the next integer.
#' @param recl_path Full path of the output .tif layer
#' of the reclassified raster file.
#' @param read If TRUE the reclassified raster .tif layer gets read into R.
#' TRUE is set by default.
#' @param nodata Nodata value of the new .tif layer. The default value is -9999.
#' @param type Data type; Options are Byte, Int16, UInt16, Int32, UInt32,
#' CInt16, CInt32; Int32 is set by default.
#' @param compress Compression type: DEFLATE or LZW; DEFLATE is set by default.
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fwrite
#' @importFrom processx run
#' @importFrom terra rast
#' @export
#'
#' @examples
#' reclassified_raster <- reclass_raster(df, rast_val = "present_value",
#' new_val = "new_value", rast_path =  "/home/user/project_folder/input.tif",
#' recl_path = "home/user/project_folder/output.tif")
#'
#' @references
#' https://grass.osgeo.org/grass82/manuals/r.reclass.html
#'
#' @author Maria Üblacker

reclass_raster <- function(data, rast_val, new_val, rast_path,
                           recl_path, read = TRUE,
                           nodata = -9999, type = "Int32",
                           compress = "DEFLATE", quiet = TRUE) {
  # Check operating system
  system <- get_os()
  # Check if data.frame is defined
  if (missing(data))
    stop("data: Input data.frame is missing.")

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if rast_val and new_val is defined
  if (missing(rast_val))
    stop("rast_val: Column name of current raster value is missing.")
  if (missing(new_val))
    stop("new_val: Column name of new raster value is missing.")

  # Check if rast_val/new_val column names exist
  if (is.null(data[[rast_val]]))
    stop(paste0("rast_val: Column name '", rast_val,
    "' does not exist."))
  if (is.null(data[[new_val]]))
    stop(paste0("new_val: Column name '", new_val,
    "' does not exist."))

  # Check if values of the rast_val/new_val columns are numeric
  if (!is.integer(data[[rast_val]]))
    stop(
      paste0("rast_val: Values of column ", rast_val,
      " have to be integers."))
  if (!is.integer(data[[new_val]]))
    stop(paste0("new_val: Values of column ", new_val,
    " have to be integers."))

  # Check if rast_path is defined
  if (missing(rast_path))
    stop("rast_path: Path of the input raster layer is missing.")

  # Check if rast_path and exists
  if (!file.exists(rast_path))
    stop(paste0("rast_path: ", rast_path, " does not exist."))

  # Check if rast_path ends and recl_path with .tif
  if (!endsWith(rast_path, ".tif"))
    stop("rast_path: Input raster is not a .tif file.")
    if (!endsWith(recl_path, ".tif"))
      stop("recl_path: Output raster file path needs to end with .tif.")

  # Check if rast_path is defined
  if (missing(recl_path))
    stop("recl_path: Path for the output raster layer is missing.")

  if (!is.logical(read))
   stop("read: Has to be TRUE or FALSE.")

  # Check if type is one of the listed types
  if (!(type == "Int16" || type == "UInt16" || type == "CInt16" ||
        type == "Int32" || type == "UInt32" || type == "CInt32" ||
        type == "Byte"))
    stop("type: Has to be 'Byte', 'Int16', 'UInt16', 'Int32', 'UInt32',
    'CInt16', or 'CInt32' ")

  # Check if compress is "DEFLATE" or "LZW"
  if (!(compress == "DEFLATE" || compress == "LZW"))
    stop("compress: Has to be 'DEFLATE or 'LZW'.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # The r.reclass function of GRASS GIS requires a text file
  # including the old and the new value with an = between
  # (e.g. 1 = 20)
  rules <- data.table(old = data[[rast_val]],
                      equal = "=",
                      new = data[[new_val]])
  # Create random string to attach to the file name of the temporary
  # rules .txt file
  rand_string <- stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
  # Path to the text file with the reclassification rules
  rules_path <- paste0(tempdir(), "/reclass_rules_", rand_string, ".txt")
  # Write rules as a .txt file to the temporary folder
  fwrite(rules, rules_path, sep = " ", col.names = FALSE)

  if (system == "linux" || system == "osx") {
  # Open GRASS GIS session
  # Call external GRASS GIS command r.reclass
  run(system.file("sh", "reclass_raster.sh",
                           package = "hydrographr"),
      args = c(rast_path, rules_path, recl_path,
                             nodata, type, compress),
      echo = !quiet)

  } else {
    # Check if WSL and Ubuntu are installed
    check_wsl()
    # Change paths for WSL
    wsl_rast_path <- fix_path(rast_path)
    wsl_recl_path <- fix_path(recl_path)
    wsl_rules_path <- fix_path(rules_path)
    wsl_sh_file <- fix_path(
      system.file("sh", "reclass_raster.sh",
                            package = "hydrographr"))

    # Open GRASS GIS session on WSL
    # Call external GRASS GIS command r.reclass
    run(system.file("bat", "reclass_raster.bat",
                    package = "hydrographr"),
        args = c(wsl_rast_path, wsl_rules_path, wsl_recl_path,
                 nodata, type, compress, wsl_sh_file),
        echo = !quiet)

  }
  # Remove temporary rules file
  file.remove(rules_path)

  if (read == TRUE) {
    # Read reclassified .tif layer
    recl_rast <- rast(recl_path)

  } else {
   # Print message
   print(paste0("Reclassified raster saved under: ", recl_path))
  }

}
