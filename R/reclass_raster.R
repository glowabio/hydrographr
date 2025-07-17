#' @title Reclassify a raster layer
#'
#' @description Reclassifies a raster .tif layer based on a look-up table, such
#' that the output raster contains the new values. The function uses the r.reclass
#' function of GRASS GIS.
#'
#' Note that the input raster needs to be of type integer. If the input raster
#' layer has floating point values, you can multiply it by some factor
#' (e.g. 1000) to achieve integer values, otherwise the GRASS GIS r.reclass will
#' round the raster values down to the next integer which is not always desired.
#'
#' @param data a data.frame or data.table with the original and new values to be
#' written to the raster.
#' @param rast_val character. The name of the column with the original raster
#' values.
#' @param new_val character. The name of the column with the new raster values,
#' which need to be integer values. In case of floating point values, consider
#' multiplying the values e.g. by 1000 to keep three decimals.
#' @param remaining character. How to treat raster values not listed in the reclassification table:
#'  `"same"` retains their original values (equivalent to `* = *` in GRASS),
#'  `"value"` assigns a fixed value (`remaining_value`), and
#'  `NULL` (default) does nothing.
#' @param remaining_value numeric. Value to assign if `remaining = "value"`. Default is -9999.
#' @param raster_layer Full path to the input raster .tif layer.
#' @param recl_layer character. Full path of the output .tif layer, i.e., the
#' reclassified raster file.
#' @param no_data numeric. The no_data value of the new .tif layer.
#' Default is -9999.
#' @param type character. Data type; Options are Byte, Int16, UInt16, Int32,
#' UInt32,CInt16, CInt32. Default is Int32. Note that only integer values are
#' allowed.
#' @param compression character. Compression of the written output file.
#' Compression levels can be defined as "none", "low", or "high". Default is
#' "low", referring to compression type "DEFLATE" and compression level 2.
#' "high" refers to compression level 9.
#' @param bigtiff logical. Define whether the output file is expected to be a
#' BIGTIFF (file size larger than 4 GB). If FALSE and size > 4GB no file will be
#' written. Default is TRUE.
#' @param read logical. If TRUE, then the reclassified raster .tif layer
#' gets read into R as a SpatRaster (terra object).
#' If FALSE, the layer is only stored on disk. Default is FALSE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table data.table fwrite
#' @importFrom processx run
#' @importFrom terra rast
#' @export
#'
#' @author Marlene Schürz, Afroditi Grigoropoulou
#'
#' @references
#' https://grass.osgeo.org/grass82/manuals/r.reclass.html
#'
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Read the stream order for each sub-catchment as a data.table
#' my_dt <- read_geopackage(gpkg= paste0(my_directory,
#'                                          "/hydrography90m_test_data",
#'                                          "/order_vect_59.gpkg"),
#'                          import_as = "data.table")
#'
#'
#' # Select the stream segment ID and and the Strahler stream order
#' str_ord <- my_dt[,c("stream", "strahler")]
#'
#' # Define input and output raster layer
#' stream_raster <- paste0(my_directory,
#'                         "/hydrography90m_test_data/stream_1264942.tif")
#'
#' recl_raster <- paste0(my_directory,
#'                       "/hydrography90m_test_data/reclassified_raster.tif")
#'
#' # Reclassify the stream network to obtain the Strahler stream order raster
#' str_ord_rast <- reclass_raster(data = str_ord,
#'                                rast_val = "stream",
#'                                new_val = "strahler",
#'                                raster_layer = stream_raster,
#'                                recl_layer = recl_raster)
#'


reclass_raster <- function(data, rast_val, new_val, remaining = NULL,
                           remaining_value = -9999,
                           raster_layer,
                           recl_layer,
                           no_data = -9999, type = "Int32",
                           compression = "low", bigtiff = TRUE,
                           read = FALSE, quiet = TRUE) {
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
  if (missing(rast_val))
    stop("rast_val: Column name of current raster value is missing.")
  if (missing(new_val))
    stop("new_val: Column name of new raster value is missing.")

  # Check if rast_val/new_val column names exist
  if (is.null(data[[rast_val]]))
    stop(paste0("rast_val: Column name '", rast_val,
    "' does not exist."))
  if(!is.null(new_val))
    if (is.null(data[[new_val]]))
      stop(paste0("new_val: Column name '", new_val,
      "' does not exist."))

  # Check if values of the rast_val/new_val columns are numeric
  if (!is.integer(data[[rast_val]]))
    stop(
      paste0("rast_val: Values of column ", rast_val,
      " have to be integers."))
  if(!is.null(new_val))
    if (!is.integer(data[[new_val]]))
      stop(paste0("new_val: Values of column ", new_val,
      " have to be integers."))

  # Check if raster_layer is defined
  if (missing(raster_layer))
    stop("raster_layer: Path of the input raster layer is missing.")

  # Check if raster_layer and exists
  if (!file.exists(raster_layer))
    stop(paste0("raster_layer: ", raster_layer, " does not exist."))

  # Check if raster_layer ends and recl_layer with .tif
  if (!endsWith(raster_layer, ".tif"))
    stop("raster_layer: Input raster is not a .tif file.")
    if (!endsWith(recl_layer, ".tif"))
      stop("recl_layer: Output raster file path needs to end with .tif.")

  # Check if recl_layer is defined
  if (missing(recl_layer))
    stop("recl_layer: Path for the output raster layer is missing.")

  # Check if type is one of the listed types
  if (!(type == "Int16" || type == "UInt16" || type == "CInt16" ||
        type == "Int32" || type == "UInt32" || type == "CInt32" ||
        type == "Byte"))
    stop("type: Has to be 'Byte', 'Int16', 'UInt16', 'Int32', 'UInt32',
    'CInt16', or 'CInt32' ")


  # Check and translate compression into the compression type and the
  # compression level which is applied to the tiff file when writing it.
  if(compression == "none") {
    compression_type  <- "NONE"
    compression_level <- 0
  } else if (compression == "low") {
    compression_type  <- "DEFLATE"
    compression_level <- 2
  } else if (compression == "high") {
    compression_type  <- "DEFLATE"
    compression_level <- 9
  } else {
    stop("'compression' must be one of 'none', 'low', or 'high'.")
  }

  # Define whether BIGTIFF is used or not. BIGTIFF is required for
  # tiff output files > 4 GB.
  if(bigtiff) {
    bigtiff <- "YES"
  } else {
    bigtiff <- "NO"
  }

  if (!is.logical(read))
   stop("read: Has to be TRUE or FALSE.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Make bash scripts executable
  make_sh_exec()

  # The r.reclass function of GRASS GIS requires a text file
  # including the old and the new value with an = between
  # (e.g. 1 = 20)
  rules <- data.table(old = data[[rast_val]],
                      equal = "=",
                      new = data[[new_val]])

  # Add the remaining values to the rules file
  if (!is.null(remaining)) {
    if (remaining == "same") {
      rules <- rbind(rules, data.table(old = "*", equal = "=", new = "*"))
    } else if (remaining == "value") {
      rules <- rbind(rules, data.table(old = "*", equal = "=", new = remaining_value))
    } else {
      stop("remaining: must be one of 'same', 'value', or NULL.")
    }
  }
  # Create random string to attach to the file name of the temporary
  # rules .txt file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Path to the text file with the reclassification rules
  rules_path <- paste0(tempdir(), "/reclass_rules_", rand_string, ".txt")
  # Write rules as a .txt file to the temporary folder
  fwrite(rules, rules_path, sep = " ", col.names = FALSE)

  if (sys_os == "linux" || sys_os == "osx") {
  # Open GRASS GIS session
  # Call external GRASS GIS command r.reclass
  processx::run(system.file("sh", "reclass_raster.sh",
                           package = "hydrographr"),
      args = c(raster_layer, rules_path, recl_layer,
               no_data, type, compression_type, compression_level, bigtiff),
      echo = !quiet)

  } else {
    # Check if WSL and Ubuntu are installed
    check_wsl()
    # Change paths for WSL
    wsl_raster_layer <- fix_path(raster_layer)
    wsl_recl_layer <- fix_path(recl_layer)
    wsl_rules_path <- fix_path(rules_path)
    wsl_sh_file <- fix_path(
      system.file("sh", "reclass_raster.sh",
                            package = "hydrographr"))

    # Open GRASS GIS session on WSL
    # Call external GRASS GIS command r.reclass
    processx::run(system.file("bat", "reclass_raster.bat",
                    package = "hydrographr"),
        args = c(wsl_raster_layer, wsl_rules_path, wsl_recl_layer,
                 no_data, type, compression_type, compression_level, bigtiff,
                 wsl_sh_file),
        echo = !quiet)

  }
  # Remove temporary rules file
  file.remove(rules_path)

  if (file.exists(recl_layer)) {
    # Print message
    cat("Reclassified raster saved under: ", recl_layer, "\n")
  } else {
    stop("Output file was not written. File size may have been larger than 4GB",
         "\nSet bigtiff = TRUE, for writing large output files.")
  }

  if (read == TRUE) {
    # Read reclassified .tif layer
    recl_rast <- rast(recl_layer)
    return(recl_rast)
  }

}
