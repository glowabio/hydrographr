#' @title Get lake catchments based on intersection points between stream network and lake
#'
#' @description Add describtion text here
#'
#'  Any thing the user needs to notice? add Note
#'
#' @param data a data.frame or data.table that contains the columns regarding
#' the intersection ids; (i.e., output of get_lake_intersection)
#' @param flow character. The name of the flow accumulation column used for sorting the intersection table;
#' i.e. either flow_accu (flow accumulation value at intersection point pixel),
#' flow_accu_max (maximum flow accumulation value of 3 x 3 neighboring pixels),
#' @param lake_id character. The name of the column containing lake ids; (i.e., output "lake_ID" of get_lake_intersection)
#' flow_accu_mean (mean flow accumulation value of 3 x 3 neighboring pixels). Default is flow_accu_mean
#' @param top integer. Number of intersection points used for lake catchment calculations;
#' e.g. top=1 equals first row of intersection table and lake outlet;
#' top=20 equals first 20 rows of intersection table; Default is all.
#' @param direction character. Full path to Hydrography 90m flow direction tif file
#' @param catch character. Full path to output catchment tif files
#' @param n_cores integer. Number of cores used in parallelsation
#' @param read logical. If TRUE, then the model .csv table
#' gets read into R as data.table and data.frame.
#' if FALSE, the table is only stored on disk. Default is FALSE.
#' @param quiet logical. If FALSE, the standard output will be printed.;
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
#' # Download hydrolakes shape files from their website and test with hydrolakes.sh instead I always transformed it before to lake.gpkg
#' # write the script to run in paralell using ncores

get_lake_catchment <- function(data, flow = "flow_accu_mean",
                                  lake_id = "lake_ID", top = "all", direction, catch,
                                  n_cores, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  ### be sure to convert csv files into txt files for the sh script to work! ###
  lake_dat <- as.data.table(data)
  # Remove duplicated rows across entire data frame
  lake_dat <- lake_dat[!duplicated(lake_dat), ]
  #  sort intersection table
  lake_dat <- lake_dat[order(-get(flow))]

  if (top == "all") {
  lake_dat <- lake_dat
  } else {
  # select the top 15 % or all intersection points
  lake_dat <- lake_dat[1:top, ]
  }

  # Create random string
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  lak_tmp_path <- paste0(tempdir(), "/ids_", rand_string, ".txt")
  ## write to tempory file to convert to txt file
  fwrite(lake_dat, lak_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = " ")

  # rename outlet_ids to go in sequence
  # lake_dat$outlet_ID <- 1:nrow(lake_dat)
  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    # Call the external .sh script extract_ids() containing the gdal function
    processx::run(system.file("sh", "get_lake_catchment.sh", package = "hydrographr"),
                  args = c(lak_tmp_path, lake_id, direction, tempdir(), catch, n_cores),
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

  # Return data frame
  return(data_ids)

}
