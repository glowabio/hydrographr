#' @title Get predict table
#'
#' @description This function creates a table with environmental variables from
#' an specific subset of subcatchments.
#'
#' @param variable character vector of variable names. Possible values are:
#' c("bio1", "bio10", "bio11", "bio12", "bio13","bio14", "bio15", "bio16",
#'  "bio17", "bio18","bio19", "bio2", "bio3", "bio4", "bio5","bio6", "bio7",
#'  "bio8", "bio9", "c100", "c10", "c20", "c30", "c40", "c50","c60", "c70",
#'  "c80", "c90", "chancurv","chandistdwseg", "chandistupcel", "chandistupseg",
#'  "chanelvdwcel", "chanelvdwseg", "chanelvupcel","chanelvupseg",
#'  "changraddwseg", "changradupcel", "changradupseg", "elev", "flow",
#'  "flowpos", "gradient", "length", "out", "outdiffdwbasin", "outdiffdwscatch",
#'  "outdistdwbasin", "outdistdwscatch", "outlet", "slopdiff", "slopgrad",
#'  "soil", "source", "strdiffdwnear", "strdiffupfarth", "strdiffupnear",
#'  "strdistdwnear", "strdistprox", "strdistupfarth", "strdistupnear",
#'  "stright").
#' @param statistics character vector of statistics names. Possible values are
#' "sd", "mean", "range" or "ALL". Default "ALL".
#' @param tile_id character. The IDs of the tiles of interest.
#' @param input_var_path path to table with environmental variables for entire
#' tiles.
#' @param subcatch_id path to a text file with subcatchments ids.
#' @param out_file_path character. The path to the output file.
#' @param n_cores numeric. Number of cores used for parallelization.
#' @param read logical. If TRUE, the table with environmental variables gets
#' read into R.
#' If FALSE, the table is only stored on disk. Default is TRUE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @importFrom processx run
#' @importFrom stringi stri_rand_strings
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fread
#'
#' @author Jaime García Márquez, Yusdiel Torres-Cambas
#'
#' @return The function returns a table with
#' * sub-catchment ID (subcID)
#' * a column for each descriptive statistic of each variable (eg. bio1_mean:
#' mean of the variable bio1)
#' @md
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory) # TODO make test data available for download!
#'
#' # Define variable and tile:
#' var <- c("bio1")
#' tile_id <- c("h18v02")
#'
#' # Point to input data
#' in_path <- paste0(my_directory, '/hydrography90m_test_data')
#' subc_ids <- paste0(my_directory, '/hydrography90m_test_data/subc_IDs.txt')
#' output <- paste0(my_directory, '/hydrography90m_test_data/predictTB.csv')
#'
#' # Run the function with 2 cores and calculate all statistics:
#' get_predict_table(variable = var,
#'                   statistics = c("ALL"),
#'                   tile_id = tile_id,
#'                   input_var_path = in_path,
#'                   subcatch_id = subc_ids,
#'                   out_file_path = output,
#'                   read = FALSE, quiet = FALSE,
#'                   n_cores = 2)
#'
#' # Now you can see the result in /tmp/.../hydrography90m_test_data/predictTB.csv
#'


get_predict_table <- function(variable,
                              statistics = "ALL",
                              tile_id,
                              input_var_path,
                              subcatch_id,
                              out_file_path,
                              n_cores = NULL,
                              read = TRUE,
                              quiet = TRUE) {

  #Check if one of the arguments is missing
  if (missing(variable))
    stop("variable is missing.
    Please provide at least the name of one variable. Possible names are: ")

  if (missing(tile_id))
    stop("Please provide at least one tile ID")

  if (missing(input_var_path))
    stop("Please provide a path to the table with environmental variables for
    the entire tiles")

  if (missing(subcatch_id))
    stop("Please provide at least one subcatchment ID")

  if (missing(out_file_path))
    stop("Please provide a path to the output file")

  # Check if paths exists
  if (!file.exists(input_var_path))
    stop(paste0("Path: ", input_var_path, " does not exist."))

  # Check variable name is one of the accepted values
  accepted_vars <- c("bio1", "bio10", "bio11", "bio12", "bio13",
                     "bio14", "bio15", "bio16", "bio17", "bio18",
                     "bio19", "bio2", "bio3", "bio4", "bio5",
                     "bio6", "bio7", "bio8", "bio9", "c100",
                     "c10", "c20", "c30", "c40", "c50",
                     "c60", "c70", "c80", "c90", "chancurv",
                     "chandistdwseg", "chandistupcel", "chandistupseg",
                     "chanelvdwcel", "chanelvdwseg", "chanelvupcel",
                     "chanelvupseg", "changraddwseg", "changradupcel",
                     "changradupseg", "elev", "flow", "flowpos",
                     "gradient", "length", "out", "outdiffdwbasin",
                     "outdiffdwscatch", "outdistdwbasin", "outdistdwscatch",
                     "outlet", "slopdiff", "slopgrad", "soil", "source",
                     "strdiffdwnear", "strdiffupfarth", "strdiffupnear",
                     "strdistdwnear", "strdistprox", "strdistupfarth",
                     "strdistupnear", "stright")
  if (any(!variable %in% accepted_vars))
    stop("Please provide a valid variable name")

  # Check if statistics name provided is one of the accepted values
  if (any(!(statistics %in% "ALL"))) {
    if (any(!(statistics %in% c("sd", "mean", "range"))))
      stop("Please provide a valid statistics name. Possible values are
             sd, mean, range or ALL")
  }


  # Check if n_cores is numeric
  if (!is.numeric(n_cores))
    stop(paste0("n_cores: Has to be numeric."))

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  # Check if read is logical
  if (!is.logical(read))
    stop("read: Has to be TRUE or FALSE.")

  # Check operating system
  sys_os <- get_os()

  # Create random string to attach to the tmp folder
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")
  tmp <- paste0("/tmp_", rand_string)

  # Path to the tmp directory
  tmp_dir <- paste0(getwd(), tmp)

  # Create temporary output directory
  dir.create(tmp_dir, showWarnings = FALSE)

  # Format variable, statistics and tile_id vectors so they can be read in bash
  variable <- paste(variable, collapse = "/")
  statistics <- paste(statistics, collapse = "/")
  tile_id <- paste(tile_id, collapse = "/")

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {
    # Call external bash script
    processx::run(system.file("sh", "get_predict_table.sh",
                              package = "hydrographr"),
                  args = c(variable,
                           statistics,
                           tile_id,
                           input_var_path,
                           subcatch_id,
                           out_file_path,
                           tmp_dir,
                           n_cores),
                  echo = FALSE)
  } else {
    # Check if WSL and Ubuntu are installed
    check_wsl()

    # Change path for WSL
    wsl_input_var_path <- fix_path(input_var_path)
    wsl_out_file_path <- fix_path(out_file_path)
    wsl_subcatch_id <- fix_path(subcatch_id)
    wsl_tmp_dir <- fix_path(tmp_dir)
    wsl_sh_file <- fix_path(system.file("sh",
                                        "get_predict_table.sh",
                                        package = "hydrographr")
    )
    processx::run(system.file("bat", "get_predict_table.bat",
                              package = "hydrographr"),
                  args = c(variable,
                           statistics,
                           tile_id,
                           wsl_input_var_path,
                           wsl_subcatch_id,
                           wsl_out_file_path,
                           wsl_tmp_dir,
                           n_cores,
                           wsl_sh_file))

  }

  # Delete temporary output directory
  unlink(tmp_dir, recursive = TRUE)

  if (read == TRUE) {
    # Read predict table
    predict_table <- fread(out_file_path)
    return(predict_table)
  }

}
