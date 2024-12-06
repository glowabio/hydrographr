#' @title Reclassify an integer raster layer
#'
#' @description Reclassifies an integer raster .tif layer using the r.reclass
#' function of GRASS GIS. To reclassify the raster layer the present raster
#' values and the new raster values have to be defined.
#'
#' If the input raster layer has floating point values, you should multiply
#' the input data by some factor (e.g. 1000) to achieve integer values,
#' otherwise the GRASS GIS r.reclass will round the raster values down to
#' the next integer which is not always desired.
#'
#' @param data a data.frame or data.table with the present and new raster
#' values.
#' @param rast_val character. The name of the column with the present
#' raster values.
#' @param new_val character. The name of the column with the new raster values.
#' @param reclass_value integer. Value to reclassify the entire raster.
#' Default is FALSE. Note in case reclass_value is provided, new_val parameter
#' needs to be empty, otherwise the raster is reclassified based on the new_val column.
#' @param raster_layer Full path to the input raster .tif layer.
#' @param recl_layer character. Full path of the output .tif layer
#' of the reclassified raster file.
#' @param read logical. If TRUE, then the reclassified raster .tif layer
#' gets read into R as a SpatRaster (terra object).
#' If FALSE, the layer is only stored on disk. Default is TRUE.
#' @param no_data numeric. The no_data value of the new .tif layer.
#' Default is -9999.
#' @param type character. Data type; Options are Byte, Int16, UInt16, Int32,
#' UInt32,CInt16, CInt32. Default is Int32.
#' @param compress Compression type: DEFLATE or LZW. Default is DEFLATE.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fwrite
#' @importFrom processx run
#' @importFrom terra rast
#' @export
#'
#' @author Maria M. Üblacker, Thomas Tomiczek
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
#' # my_dt <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#' type = "net", as_dt = T)
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
# Reclassify the stream network with the value 1 across the network
#' str_ord_rast <- reclass_raster(data = str_ord$stream,
#'                                reclass_value = 1,
#'                                rast_val = "stream",
#'                                raster_layer = stream_raster,
#'                                recl_layer = recl_raster)

reclass_raster <- function(data, rast_val, new_val = FALSE, raster_layer,
                           recl_layer, reclass_value = FALSE,
                           no_data = -9999, type = "Int32",
                           compression = "low", bigtiff = TRUE,
                           read = FALSE, quiet = TRUE) {
  # Check operating system
  system <- get_os()
  # Check if data.frame is defined
  if (missing(data))
    stop("data: Input data.frame is missing.")

  # Check if input data is of type data.frame, data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if rast_val is defined
  if (missing(rast_val))
    stop("rast_val: Column name of current raster value is missing.")

  # Check if rast_val column names exist
  if (is.null(data[[rast_val]]))
    stop(paste0("rast_val: Column name '", rast_val,
    "' does not exist."))

  # Check if values of the rast_val columns are numeric
  if (!is.integer(data[[rast_val]]))
    stop(
      paste0("rast_val: Values of column ", rast_val,
      " have to be integers."))

  # Check if new_val column names exist when no reclass_values is given
  if (isFALSE(new_val) && isFALSE(reclass_value))
    stop(paste0("new_val: Column name '", new_val,
                "' does not exist."))

  # Check if values of the new_val columns are numeric when no reclass_values is given
  if (isFALSE(reclass_value)) {
    if (!is.integer(data[[new_val]])) {
      stop(paste0("reclass_value:", reclass_value, " must be integers."))
    }

  }

  # Check if reclass_value is an numeric or integer value
    if (!isFALSE(reclass_value)) {
      if (!(is.numeric(reclass_value) || is.integer(reclass_value))) {
        stop(paste0("reclass_value:", reclass_value, " must be integers."))
      }
    }

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

  if (!is.logical(read))
   stop("read: Has to be TRUE or FALSE.")

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


  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Make bash scripts executable
  make_sh_exec()

  # To check the raster values and data values
  # load raster and convert to data.frame
  rast_dat <- rast(raster_layer)
  rast_dat <- as.data.frame(rast_dat)
  rast_dat <- as.data.frame(unique(rast_dat[[1]]))
  colnames(rast_dat) <- "val"

    # Check if rast_val is missing raster values
    if (length(data[[rast_val]]) < length(rast_dat[[1]])) {
      print("Reclassification is missing raster values: Warning NA's are introduced!")
    }
    # Check and handle if raster values are provided in rast_val that are not in the raster tif file.
    if (length(data[[rast_val]]) != length(rast_dat[[1]])) {
    # Index all raster values which are not in the input data table
    indx_miss_raster <- which(rast_dat[[1]] %in% data[[rast_val]])
    # Get missing raster values
    miss_raster <- rast_dat[-c(indx_miss_raster),]
    print(paste0("These values of the raster were not found in the data table:", paste(miss_raster, collapse = ", ")))
    # Write all values found in raster tif file and input data table as data frame
    same_val1 <- as.data.frame(rast_dat[c(indx_miss_raster),])
    # Set name for raster values
    colnames(same_val1) <- "val"
    # Index all input data table values which are not in the raster tif file
    indx_miss_rast_val <- which(data[[rast_val]] %in% rast_dat[[1]])
    # Get missing input data values
    miss_rast_val <- data[-c(indx_miss_rast_val),]
    # Get only missing raster input data values to throw out message
    missing_rast_values <- data[-c(indx_miss_rast_val),1]
    print(paste0("These raster values of the data table were not found in the raster:", paste(missing_rast_values, collapse = ", ")))
    # Write all values found in input data table and raster file as data frame
    same_val2 <- data[c(indx_miss_raster),]
    # Combine all values found in raster and input data table
    same_val <- as.data.frame(cbind(same_val1, same_val2))
    # Write missing raster values as data frame
    miss_raster <- as.data.frame(miss_raster)
    # Set name for raster values
    colnames(miss_raster) <- "val"
    # Give missing values NA
    miss_raster$rast_val <- NA
    miss_raster$new_val <- NA
    # Set to the same names to combine with all values table
    colnames(miss_raster) <- c("val", rast_val, new_val)
    # Combine all values table with missing values table and use as input data
    data <- rbind(same_val, miss_raster)

    # List both rast_val and new_val columns to check if they are of equal length
    dat <- list(data[[rast_val]], list(data[[new_val]]))

    # In case new_val is bigger than rast_val length of rast_val will be used to reclassify
    data <- setNames(do.call(cbind.data.frame,
                            lapply(lapply(dat, unlist),
                                   `length<-`, max(lengths(dat)))), paste0(c(rast_val, new_val)))
    if (isFALSE(reclass_value)) {
    # The r.reclass function of GRASS GIS requires a text file
    # including the old and the new value with an = between
    # (e.g. 1 = 20)
    rules <- data.table::data.table(old = data[[rast_val]],
                                    equal = "=",
                                    new = data[[new_val]])
    }
  }

  if (!isFALSE(reclass_value)) {

  # use reclass_value for reclassification
    data$reclass <- reclass_value
    data$reclass <- as.integer(data$reclass)
    #
    #
    #
    #
    #
    # rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
    # rules_path <- paste0(tempdir(), "/reclass_rules_", rand_string, ".txt")
    # fwrite(data, rules_path, sep = " ", col.names = TRUE)

    # The r.reclass function of GRASS GIS requires a text file
    # including the old and the new value with an = between
    # (e.g. 1 = 20)
    rules <- data.table::data.table(old = data[[rast_val]],
                                    equal = "=",
                                    new = data[["reclass"]])
   }
  # else {
  #
  # # The r.reclass function of GRASS GIS requires a text file
  # # including the old and the new value with an = between
  # # (e.g. 1 = 20)
  # rules <- data.table::data.table(old = data[[rast_val]],
  #                     equal = "=",
  #                     new = data[[new_val]])
  # }
  # Create random string to attach to the file name of the temporary
  # rules .txt file
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  # Path to the text file with the reclassification rules
  rules_path <- paste0(tempdir(), "/reclass_rules_", rand_string, ".txt")
  # Write rules as a .txt file to the temporary folder
  fwrite(rules, rules_path, sep = " ", col.names = FALSE)

  # Remove all temporary data files
  rm(indx_miss_raster, miss_raster, same_val1, indx_miss_rast_val,
     miss_rast_val, missing_rast_values, same_val2, same_val)

  if (system == "linux" || system == "osx") {
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
