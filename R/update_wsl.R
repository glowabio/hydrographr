#' @title Update the Windows Subsystem for Linux
#'
#' @description The "sudo apt-get update" command updates the package lists from
#' the repositories to ensure that the system has the latest information about
#' available software packages.
#' The "sudo apt-get upgrade" command upgrades the installed packages to their
#' latest available versions by downloading and installing any updates that are
#' available in the repositories.
#'
#'
#' @param data a data.frame or data.table with lat/lon coordinates in WGS84.
#' @param quiet logical. If FALSE, the standard output will be printed.
#'
#' @importFrom processx run
#' @export
#'
#' @note
#' Sometimes VPN connections block the downloading of necessary updates.
#' We thus recommend to run this function outside any VPN connection.
#'
#' @author Afroditi Grigoropoulou
#'
#' @references
#' \url{https://manpages.debian.org/stretch/apt/apt.8.en.html}
#'

update_wsl <- function(password) {

  # Make bash scripts executable
  make_sh_exec()

  processx::run(system.file("sh", "update_wsl.sh",
                            package = "hydrographr"),
                args = password,
                echo = TRUE)
}

