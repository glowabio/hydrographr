#' @title Get predict table
#'
#' @description This function creates a table with environmental variables from
#' an specific subset of subcatchments.
#'
#' @param variable character vector of variable names. Possible values are:
#'  all variables in the Env90m dataset, which can bew viewed by calling
#'  'download_<datasetname>_tables()'. For more details, see
#'  '?download_env90m_tables'.
#' @param statistics character vector of statistics names. Possible values are
#' "sd", "mean", "range" or "ALL". Default "ALL".
#' @param tile_id character. The IDs of the tiles of interest.
#' @param input_var_path path to directory that contains table with
#'  environmental variables for entire tiles. Tables may be in subdirectories
#'  of the provided directory.
#' @param subcatch_id path to a text file with subcatchment ids, or numeric
#'  vector containing subcatchment ids.
#' @param out_file_path character. The path to the output file to be created.
#' @param n_cores numeric. Number of cores used for parallelization.
#' @param read logical. If TRUE, the table with environmental variables gets
#' read into R.
#' If FALSE, the table is only stored on disk. Default is TRUE.
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#' @param tempdir String. Path to the directory where to store/look for the
#'  file size table. If not passed, defaults to the output of [base::tempdir()].
#' @param overwrite logical. If TRUE, the output file will be overwritten if it.
#' already exists. Useful for repeated testing. Default is FALSE.
#' @importFrom processx run
#' @importFrom stringi stri_rand_strings
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fread
#' @export

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
                              quiet = TRUE,
                              tempdir = NULL,
                              overwrite = FALSE) {

  # Define tempdir:
  if (is.null(tempdir)) {
    tempdir <- tempdir()
  }

  #Check if one of the arguments is missing
  if (missing(variable))
    stop('Variable is missing. Please provide at least the name of one variable.',
      '\n  You may use any of the >1000 variables of the Environment90m dataset,',
      ' which you can view using e.g. download_soil_tables(). Please check',
      ' ?download_env90m_tables for more details.')

  if (missing(tile_id))
    stop("Please provide at least one tile ID (parameter \"tile_id\").")

  if (missing(input_var_path))
    stop("Please provide a path to the directories containing the tables",
         " with environmental variables for the entire tiles",
         " (parameter \"input_var_path\").")

  if (missing(subcatch_id))
    stop("Please provide at least one subcatchment ID or a path to a file",
     " containing subcatchment IDs (parameter \"subcatch_id\").")

  if(is.numeric(subcatch_id)) {
    if (!quiet)
      message("INFO: You provided ", length(subcatch_id), " subcatchment IDs: ",
              paste(subcatch_id[1:3], collapse=", "), ", etc.")
  } else {
    if (!quiet) message("INFO: You provided a path to a table containing subcatchment IDs.")
    if (!file.exists(subcatch_id))
      stop("File: ", subcatch_id, " does not exist (should contain subcatchment ids).")
  }

  if (missing(out_file_path))
    stop("Please provide a path to the output file (parameter \"out_file_path\").")

  # If the result file already exists, stop or warn user:
  if (file.exists(out_file_path)) {

    # It is a not a file, but a directory!
    if (dir.exists(out_file_path)) {
      stop("Please provide a path to an output file. You passed a path to a directory: ",
        out_file_path)
    }

    # It is a file, and it exists already!
    else {
      if (overwrite) {
        message("INFO: Result will overwrite existing file: ", out_file_path)
      } else {
        stop("Output file already exists (cannot overwrite): ", out_file_path,
             "\n  (if you want to overwrite, set \"overwrite=TRUE\").")
      }
    }
  }

  # If the result file does not exist, check whether it can be created
  # (to prevent errors later):
  if (!file.exists(out_file_path)) {

    # Trying to write to a file that ends in a slash causes an error in the bash script:
    # "cp: cannot create regular file './testfile/': Not a directory"
    if (endsWith(out_file_path, "/")) {
      stop("Please provide a path to an output file. You passed a path to a directory: ",
        out_file_path)
    }

    # The directory in which we want to write the output has to exist:
    if (dir.exists(dirname(out_file_path))) {
      if (!quiet) message("INFO: Output will be written to ", out_file_path)
    } else {
      stop("Cannot create output file \"", basename(out_file_path), "\".",
           " We can only write output to an existing directory.",
           " Directory does not exist: ", dirname(out_file_path)
      )
    }
  }

  # Check if paths exists
  if (!file.exists(input_var_path))
    stop("Path: ", input_var_path, " does not exist (should contain the input tables).")

  # Check if input tables exist somewhere in a subdir of input_var_path:
  # This only works if the files have this name pattern: bio1_h18v04.txt
  # Example: Providing "./my_inputs" also find the files here:
  # ./my_inputs/chelsa_bioclim_v2_1/1981-2010_observed/bio1/bio1_h18v04.txt
  # ./my_inputs/soilgrids250m_v2_0/clyppt/clyppt_h18v04.txt
  # etc.
  for (ivar in variable) {
    for (itile in tile_id) {
      filename <- paste0(ivar, "_", itile, ".txt")
      if (filename %in% basename(list.files(input_var_path, recursive=TRUE))) {
        #if (!quiet) message("INFO: Input table exists: ", filename)
      } else {
        # Check for zips?
        if (paste0(ivar, "_", itile, ".zip") %in%
            basename(list.files(input_var_path, recursive=TRUE))) {
          stop("Input table does not exist: ", filename,
             " (in subdirectory of: ", input_var_path, ")\n  But we found ",
             paste0(ivar, "_", itile, ".zip"), ", maybe you forgot to unzip?")
        } else {
          stop("Input table does not exist: ", filename,
             " (in subdirectory of: ", input_var_path, ")")
        }
      }
      # The following loop is not necessary. It finds the path of each file,
      # which is not required but just nice, for the user's convenience:
      for (somepath in list.files(input_var_path, recursive=TRUE)) {
        somename <- basename(somepath)
        if (somename==filename) {
          if (!quiet) message("INFO: Input table exists: ", filename,
                              " (here: ", file.path(input_var_path, somepath), ")")
        }
      }
    }
  }

  # Check if statistics name provided is one of the accepted values
  if (any(!(statistics %in% "ALL"))) {
    if (any(!(statistics %in% c("sd", "mean", "range")))) {
      stop("Please provide a valid statistics name. Possible values are:",
             " sd, mean, range or ALL")
    }
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

  # Now do the longer check, which needs to download some files first (unless they are
  # already present in temp):
  # Check variable name is one of the accepted values
  #if (!quiet) message("Checking the variable names against the list(s) of allowed variable names...")
  if (!quiet) message("INFO: Downloading the list(s) of allowed variable names,",
                      "unless they were already downloaded to your temp directory...")
  # TODO: This download is potentially noisy. Make it (possibly) quiet? Really quiet?
  accepted_vars <- c(
    download_observed_climate_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names,
    download_projected_climate_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names,
    download_hydrography90m_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names,
    download_soil_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names,
    download_landcover_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names,
    download_flo1k_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names,
    download_cgiar_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names,
    download_merit_dem_tables(download=FALSE, quiet=TRUE, tempdir=tempdir)$variable_names
  )
  if (any(!variable %in% accepted_vars)) {
    which_invalid <- variable[!variable %in% accepted_vars]
    stop(paste0("These variables are not valid: ",
      paste0(which_invalid, collapse=", "),
      ". Please provide a valid variable name.",
      "\nYou may use any of the >1000 variables of the Environment90m dataset,",
      " which you can view using e.g. download_soil_tables(). Please check",
      " ?download_env90m_tables for more details."))
  }

  # Check operating system
  sys_os <- get_os()

  # Create random string to attach to the tmp folder
  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  # Path to the tmp directory
  # TODO: Should we do this in the tempdir?
  tmp_dir <- file.path(getwd(), paste0("tmp_", rand_string))
  if (!quiet) message(paste0("INFO: Temp dir for this task: ", tmp_dir))

  # Create temporary output directory
  #if (!quiet) message("Creating temp directory for GRASS: ", tmp_dir)
  dir.create(tmp_dir, showWarnings = FALSE)

  # If user provided subcatchmend ids, write them to a file that the bash file can use:
  if(is.numeric(subcatch_id)) {
    file_subcids <- paste0("subc_ids_", rand_string, ".txt")
    full_path_subcids <- file.path(tmp_dir, file_subcids)
    if (!quiet) message("INFO: Writing subcatchment IDs provided by user",
                        " to text file: ", file_subcids)
    fileConn<-file(full_path_subcids)
    writeLines(as.character(subcatch_id), fileConn)
    close(fileConn)
    subcatch_id = full_path_subcids
  }

  # Format variable, statistics and tile_id vectors so they can be read in bash
  variable <- paste(variable, collapse = "/")
  statistics <- paste(statistics, collapse = "/")
  tile_id <- paste(tile_id, collapse = "/")

  # Make bash scripts executable
  make_sh_exec()
  if (!quiet) message("INFO: Running bash script...")
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
                  echo = !quiet)
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
  if (!quiet) message("INFO: Running bash script: Done.")


  # Delete temporary output directory
  unlink(tmp_dir, recursive = TRUE)

  if (read == TRUE) {
    if (!quiet) message("INFO: Reading result table from ", out_file_path)
    predict_table <- fread(out_file_path)
    return(predict_table)
  }

}
