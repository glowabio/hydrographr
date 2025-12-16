# utils.R
# Utility functions which are used in different R functions

#' Identify the operating system.
#' The function was written by Will Lowe and was copied from here:
#' https://conjugateprior.org/2015/06/identifying-the-os-from-r/
#'
#' @keywords internal
#'
get_os <- function() {
  sysinf <- Sys.info()

  if (!is.null(sysinf)) {
    os <- sysinf[["sysname"]]
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


#' Make bash scripts executable
#'
#' @keywords internal
#

make_sh_exec <- function() {
  sh_files <- list.files(system.file("sh", package = "hydrographr"),
                      pattern = "\\.sh", full.names = TRUE)
  lapply(sh_files, function(x) system(command = paste0("chmod u+x ", "'", x, "'")))

}


#' Check if WSL and Ubuntu is installed on Windows
#'
#' @keywords internal
#'
check_wsl <- function() {
  # Check if lxss folder exists under C:\Windows\System32
  lxss <- file.exists(paste0(Sys.getenv("windir"), "/System32/lxss"))
  # Check if Ubuntu exists under ~\Appdata\Local\...
  ubuntu <- list.files(paste0(Sys.getenv("localappdata"), "/Packages"),
                       pattern = "Ubuntu")

  if (lxss == TRUE && length(ubuntu) == 0) {
    stop("Ubuntu is not installed!")
  }

  if (lxss == FALSE && length(ubuntu) == 1) {
    stop("WSL is not installed!")
  }

  if (lxss == FALSE && length(ubuntu) == 0)  {
    stop("WSL and Ubuntu are not installed!")
  }

}


#' Fix path for WSL on Windows
#'
#' @param path Full Windows path.
#' @import magrittr
#' @importFrom stringi stri_replace_all_fixed stri_replace_first_fixed
#' @keywords internal
#'
fix_path <- function(path) {

  drive <- substr(path, 1, 2)
  mnt <- paste0("/mnt/", tolower(substr(drive, 1, 1)))

  path %>%
    stri_replace_all_fixed(., "\\", "/") %>%
    stri_replace_first_fixed(., drive, mnt) %>%
    stri_replace_first_fixed(., "Program Files (x86)", "PROGRA~2") %>%
    stri_replace_first_fixed(., "Program Files", "PROGRA~1")

}

#' Traverse downstream along a stream network
#'
#' @description
#' Starting from a given segment and position along that segment, this function
#' traverses downstream along the network for a specified distance, collecting
#' all segments (or partial segments) encountered.
#'
#' @param lines_sf An `sf` object representing the stream network with columns
#'   `subc_id`, `target`, and `length`.
#' @param segment_id Character or numeric. The starting segment ID.
#' @param start_fraction Numeric between 0 and 1. The fractional position along
#'   the starting segment (0 = start of segment, 1 = end of segment).
#' @param max_distance Numeric. Maximum distance to traverse downstream (in the
#'   same units as `length` column in `lines_sf`).
#'
#' @return A list of segment information, where each element contains:
#'   \item{subc_id}{Segment ID}
#'   \item{fraction_start}{Starting fraction along the segment}
#'   \item{fraction_end}{Ending fraction along the segment}
#'   \item{length_used}{Length of segment used in the traversal}
#'
#' @details
#' The function follows the network downstream (using the `target` column)
#' until either the maximum distance is reached or there are no more downstream
#' segments. Partial segments are included if the distance budget is exhausted
#' partway through a segment.
#'
#' @keywords internal
traverse_downstream <- function(lines_sf, segment_id, start_fraction, max_distance) {
  collected <- list()
  remaining_dist <- max_distance
  current_seg_id <- segment_id
  current_fraction <- start_fraction

  while (remaining_dist > 0 && !is.na(current_seg_id)) {
    seg <- lines_sf %>% dplyr::filter(subc_id == current_seg_id)
    if (nrow(seg) == 0) break

    # Calculate available length from current position to end of segment
    available_length <- seg$length[1] * (1 - current_fraction)

    if (available_length <= remaining_dist) {
      # Take the whole remaining segment
      collected[[length(collected) + 1]] <- list(
        subc_id = current_seg_id,
        fraction_start = current_fraction,
        fraction_end = 1,
        length_used = available_length
      )
      remaining_dist <- remaining_dist - available_length
      current_seg_id <- seg$target[1]
      current_fraction <- 0
    } else {
      # Take partial segment
      fraction_end <- current_fraction + (remaining_dist / seg$length[1])
      collected[[length(collected) + 1]] <- list(
        subc_id = current_seg_id,
        fraction_start = current_fraction,
        fraction_end = fraction_end,
        length_used = remaining_dist
      )
      remaining_dist <- 0
    }
  }

  return(collected)
}


#' Traverse upstream along a stream network
#'
#' @description
#' Starting from a given segment and position along that segment, this function
#' traverses upstream along the network for a specified distance, collecting
#' all segments (or partial segments) encountered. Handles branching networks
#' by exploring all upstream tributaries.
#'
#' @param lines_sf An `sf` object representing the stream network with columns
#'   `subc_id`, `target`, and `length`.
#' @param segment_id Character or numeric. The starting segment ID.
#' @param start_fraction Numeric between 0 and 1. The fractional position along
#'   the starting segment (0 = start of segment, 1 = end of segment).
#' @param max_distance Numeric. Maximum distance to traverse upstream (in the
#'   same units as `length` column in `lines_sf`).
#'
#' @return A list of segment information, where each element contains:
#'   \item{subc_id}{Segment ID}
#'   \item{fraction_start}{Starting fraction along the segment}
#'   \item{fraction_end}{Ending fraction along the segment}
#'   \item{length_used}{Length of segment used in the traversal}
#'
#' @details
#' The function explores upstream by finding all segments whose `target` points
#' to the current segment. It uses a queue-based approach to handle multiple
#' upstream branches, avoiding revisiting segments. Partial segments are included
#' if the distance budget is exhausted partway through a segment.
#'
#' @keywords internal
traverse_upstream <- function(lines_sf, segment_id, start_fraction, max_distance) {
  collected <- list()
  queue <- list(list(seg_id = segment_id, fraction = start_fraction, dist = max_distance))
  visited <- character()

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    if (current$seg_id %in% visited || current$dist <= 0) next
    visited <- c(visited, current$seg_id)

    seg <- lines_sf %>% dplyr::filter(subc_id == current$seg_id)
    if (nrow(seg) == 0) next

    # Calculate available length from start of segment to current position
    available_length <- seg$length[1] * current$fraction

    if (available_length <= current$dist) {
      # Take the whole segment up to the fraction
      collected[[length(collected) + 1]] <- list(
        subc_id = current$seg_id,
        fraction_start = 0,
        fraction_end = current$fraction,
        length_used = available_length
      )

      # Find upstream segments (segments that flow into this one)
      upstream_segs <- lines_sf %>% dplyr::filter(target == current$seg_id)
      for (j in seq_len(nrow(upstream_segs))) {
        queue[[length(queue) + 1]] <- list(
          seg_id = upstream_segs$subc_id[j],
          fraction = 1,
          dist = current$dist - available_length
        )
      }
    } else {
      # Take partial segment
      fraction_start <- current$fraction - (current$dist / seg$length[1])
      collected[[length(collected) + 1]] <- list(
        subc_id = current$seg_id,
        fraction_start = fraction_start,
        fraction_end = current$fraction,
        length_used = current$dist
      )
    }
  }

  return(collected)
}

