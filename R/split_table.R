#' @title Split table
#'
#' @description Split the table along a set number of rows into multiple parts
#' of equal length.
#'
#' @param data a data.frame or data.table
#' @param split_tbl_path character. Full path to store the split tables
#' @param split number of rows selected to split the table
#' @param quiet logical. If FALSE, the standard output will be printed.
#' Default is TRUE.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom data.table fread fwrite
#' @importFrom processx run
#' @export
#'
#' @details
#' add details here
#'
#' @note
#' Duplicated rows will be removed.
#'
#' @author Jaime Garcia Marquez, Thomas Tomiczek
#'
#' @references
#' add references here
#'
#' @examples
#' # Create data table
#'
#' df <- data.frame(matrix(ncol = 2, nrow = 10000))
#' colnames(df) <- c('var1', 'var2')
#'
#' # Define full path to store split data tables
#' my_directory <- tempdir()
#' split_tbl_path <- paste0(my_directory,
#'                      "/hydrography90m_test_data/")
#' Split data table
#' split_table <- (df, split = 2000, split_tbl_path)
#'
#' # Show the output table
#' hydrography90m_ids


split_table <- function(data, split = NULL, split_tbl_path,
                        quiet = TRUE) {

  # Check if input data is of type data.frame,
  # data.table or tibble
  if (!is(data, "data.frame"))
    stop("data: Has to be of class 'data.frame'.")

  for (name in split) {
    if (!is.null(name))
        stop(paste0("Please specify a number to split the table"))
  }

  # Check if paths exists
  if (!dir.exists(split_tbl_path))
    stop(paste0("Path: ", split_tbl_path, " does not exist."))

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

  # data_ids <- fread(paste0(split_tbl_path, ".csv"),
                  #  keepLeadingZeros = TRUE, header = TRUE, sep = ",",
                  # fill = TRUE)

  split_files <- list.files(split_tbl_path)
  for(i in 1:length(split_files)) {                              # Head of for-loop
    assign(paste0("data", i),                                   # Read and store data frames
           fread(paste0(split_tbl_path,
                            data_files[i])))
  }
  # Remove all files in the tmp folder
  file.remove(table_tmp_path)

  # Return data frame
  return(split_files)

}
