#' Reclassify raster values of a raster file
#'
#' Reclassifies a raster file based on integer values according
#' to the reclassification rules.
#' @param rast_val Numeric vector of current raster values.
#' @param recl_val Numeric vector of reclassification values .
#' @param rast_path Full path to raster .tif layer.
#' @param recl_path Full path to reclassified .tif layer = output file.
#' @param recl_read If TRUE the reclassified raster .tif layer gets read into R.
#' TRUE is set by default.
#' @param nodata Nodata value of the new .tif layer. The default value is -9999.
#' @param type Data type; Options are Byte, Int16, UInt16, Int32, UInt32,
#' CInt16, CInt32; Int32 is set by default.
#' @param compress Compression type: DEFLATE or LZW; DEFLATE is set by default.
#' @import magrittr
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fwrite
#' @importFrom processx run
#' @importFrom terra rast
#' @export

reclass_raster <- function(rast_val, recl_val, rast_path,
                           recl_path, recl_read = TRUE,
                           nodata = -9999, type = "Int32",
                           compress = "DEFLATE", quiet = TRUE) {
  # Check operating system
  system <- get_os()
  # The r.reclass function of GRASS GIS requires a text file
  # including the old and the new value with an = between
  # (e.g. 1 = 20)
  rules <- data.table(old = rast_val,
                      equal = "=",
                      new = recl_val)
  # Create random string to attach to the file name of the temporary
  # rules .txt file
  rand_string <- stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
  # Path to the text file with the reclassification rules
  rules_path <- paste0(tempdir(), "/reclass_rules_", rand_string, ".txt")
  # Write rules as a .txt file to the temporary folder
  fwrite(rules, rules_path, sep = " ", col.names = FALSE)

  if (system == "linux") {
  # Open GRASS GIS session
  # Call external GRASS GIS command r.reclass
  run(system.file("sh", "reclass_raster.sh",
                           package = "hydrographr"),
      args = c(rast_path, rules_path, recl_path,
                             nodata, type, compress),
      echo = !quiet)

  } else if (system == "windows") {
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

  } else {
    # Stop the function if the OS is macOS
    stop("Sorry, reclass_raster() is not implimented for macOS.")
  }

  # Remove temporary rules file
  file.remove(rules_path)

  if (recl_read == TRUE) {
    # Read reclassified .tif layer
    recl_rast <- rast(recl_path)

  } else {
   # Print message
   print(paste0("Reclassified raster saved under: ", recl_path))
  }

}
