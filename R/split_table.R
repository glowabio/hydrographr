#' @title Split table
#'
#' @description Split the table along a set number of rows into multiple parts
#' of equal length.
#'
#' Note that duplicated rows will be removed.
#'
#' @param data a data.frame or data.table
#' @param split_tbl_path character. Full path to store the split tables
#' @param split numeric. number of rows selected to split the table
#' @param read logical. If TRUE, then the split data tables
#' get read into R as a data tables.
#' If FALSE, the tables are only stored on disk. Default is FALSE.
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
#' # Create data table
#'
#' df <- data.frame(matrix(ncol = 2, nrow = 10000))
#' colnames(df) <- c('var1', 'var2')
#' # or with real data
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#' df <- fread(paste0(my_directory, '/projectionTB.csv'), fill=TRUE)
#'
#' # Define full path to store split data tables
#' split_tbl_path <- paste0(my_directory,
#'                      "/hydrography90m_test_data/")
#' # Split data table
#' hydrography90m_ids <- split_table(df, split = 20000, split_tbl_path)
#'
#' # Show the output table
#' hydrography90m_ids


split_table <- function(data, split = NULL, split_tbl_path,
                        read = FALSE, quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  # if (!is(data, "data.frame"))
  #   stop("data: Has to be of class 'data.frame'.")

  # Check if value of split is numeric
  if (!is.numeric(split))
    stop("split: Value has to be numeric.")

  # Check if paths exists
  if (!dir.exists(split_tbl_path))
    stop(paste0("Path: ", split_tbl_path, " does not exist."))

  if (!is.logical(read))
    stop("read: Has to be TRUE or FALSE.")

  # Check if quiet is logical
  if (!is.logical(quiet))
    stop("quiet: Has to be TRUE or FALSE.")

  if (is.character(data) && file.exists(data)) {
    data <- fread(data)
    }

  rand_string <- stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z0-9]")

  table_tmp_path <- paste0(tempdir(), "/split_table_", rand_string, ".csv")

  fwrite(data, table_tmp_path, col.names = TRUE,
         row.names = FALSE, quote = FALSE, sep = ",")

  # Check operating system
  sys_os <- get_os()

  # Make bash scripts executable
  make_sh_exec()

  if (sys_os == "linux" || sys_os == "osx") {

    # Call the external .sh script extract_ids() containing the gdal function
    processx::run(system.file("sh", "split_table.sh", package = "hydrographr"),
                  args = c(table_tmp_path, split_tbl_path, split),
                  echo = !quiet)

  } else {
    # Check if WSL and Ubuntu is installed
    check_wsl()
    # Change path for WSL
    wsl_table_tmp_path <- fix_path(table_tmp_path)
    wsl_split_tbl_path <- fix_path(split_tbl_path)
    wsl_split <- fix_path(split)
    wsl_sh_file <- fix_path(
      system.file("sh", "split_table.sh",
                  package = "hydrographr"))

    processx::run(system.file("bat", "split_table.bat",
                              package = "hydrographr"),
                  args = c(wsl_table_tmp_path, wsl_split_tbl_path,
                           wsl_split,
                           wsl_sh_file, echo = !quiet))
  }

  # Read stored data frames
  if (read == TRUE) {
    split_files <- list.files(split_tbl_path)

    for (i in seq_along(split_files)) {
      assign(paste0("data", i),
             fread(paste0(split_tbl_path,
                          split_files[i])))
      return(split_files[i])
    }
  } else {
    cat(paste0("Split tables are stored under ", split_tbl_path))
  }

  # Remove all files in the tmp folder
    file.remove(table_tmp_path)

}
