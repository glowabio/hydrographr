# utils.R
# Utility functions which are used in different R functions

#' Identify the operating system.
#' The function was written by Will Lowe and was copied from here:
#' https://conjugateprior.org/2015/06/identifying-the-os-from-r/
#'
#' @keywords internal
#'
get_os <- function(){
  sysinf <- Sys.info()

  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }

  # If rare case occurs that Sys.info() is NULL
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  tolower(os)
}


#' Check if WSL and Ubuntu is installed on Windows
#'
#' #' @keywords internal
#'
check_wsl <- function(){
  # Check if lxss folder exists under C:\Windows\System32
  lxss <- file.exists(paste0(Sys.getenv("windir"),"/System32/lxss"))
  # Check if Ubuntu exists under ~\Appdata\Local\...
  ubuntu <- list.files(paste0(Sys.getenv("localappdata"), "/Packages"),
                       pattern ="Ubuntu")

  if (lxss == TRUE & length(ubuntu) == 0) {
    stop("Ubuntu is not installed!")
  }

  if (lxss == FALSE & length(ubuntu) == 1) {
    stop("WSL is not installed!")
  }

  if (lxss == FALSE & length(ubuntu) == 0)  {
    stop("WSL and Ubuntu are not installed!")
  }

}
