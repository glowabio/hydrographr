#' @title Get predict table
#'
#' @description This function creates a table with environmental variables from
#' an specific subset of subcatchments.
#'
#' @param variable character vector of variable names. Possible values are:
#' c("bio1", "bio10", "bio11", "bio12", "bio13",
#' "bio14", "bio15", "bio16", "bio17", "bio18",
#' "bio19", "bio2", "bio3", "bio4", "bio5",
#' "bio6", "bio7", "bio8", "bio9", "c100",
#' "c10", "c20", "c30", "c40", "c50",
#' "c60", "c70", "c80", "c90", "chancurv",
#' "chandistdwseg", "chandistupcel", "chandistupseg",
#' "chanelvdwcel", "chanelvdwseg","chanelvupcel", "chanelvupseg",
#' "changraddwseg", "changradupcel", "changradupseg",
#' "elev", "flow", "flowpos", "gradient", "length",
#' "out", "outdiffdwbasin", "outdiffdwscatch", "outdistdwbasin",
#' "outdistdwscatch", "outlet", "slopdiff", "slopgrad", "soil",
#' "source", "strdiffdwnear", "strdiffupfarth", "strdiffupnear",
#' "strdistdwnear", "strdistprox","strdistupfarth",
#' "strdistupnear", "stright").
#'
#' @param statistics character vector of statics names. Possible values are
#' "sd", "mean", "range" or "ALL". Default "ALL"
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
#' @return The function returns...
#'
#' @author Jaime García Márquez, Yusdiel Torres-Cambas
#'s
#' @examples bla

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
    Please provide at least the name of one variable")

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

  if (!file.exists(out_file_path))
    stop(paste0("Path: ", out_file_path, " does not exist."))

  # Check if the variable name provided is one of the accepted values
  var_list <- c("bio1", "bio10", "bio11", "bio12", "bio13",
                "bio14", "bio15", "bio16", "bio17", "bio18",
                "bio19", "bio2", "bio3", "bio4", "bio5",
                "bio6", "bio7", "bio8", "bio9", "c100",
                "c10", "c20", "c30", "c40", "c50",
                "c60", "c70", "c80", "c90", "chancurv",
                "chandistdwseg", "chandistupcel", "chandistupseg",
                "chanelvdwcel", "chanelvdwseg","chanelvupcel", "chanelvupseg",
                "changraddwseg", "changradupcel", "changradupseg",
                "elev", "flow", "flowpos", "gradient", "length",
                "out", "outdiffdwbasin", "outdiffdwscatch", "outdistdwbasin",
                "outdistdwscatch", "outlet", "slopdiff", "slopgrad", "soil",
                "source", "strdiffdwnear", "strdiffupfarth", "strdiffupnear",
                "strdistdwnear", "strdistprox","strdistupfarth",
                "strdistupnear", "stright")
  if (!variable %in% var_list) {
    stop("Please provide a valid variable name")
  }

  # Check if statistics name provided is one of the accepted values
  if (statistics != "ALL") {
    if (!statistics %in% c("sd", "mean", "range"))
      stop("Please provide a valid statistics name. Possible values are
             sd, mean, range")
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
  #unlink(tmp_dir, recursive = TRUE)

  if (read == TRUE) {
    # Read predict table
    predict_table <- fread(out_file_path)
    return(predict_table)
  }

}