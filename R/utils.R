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

#' Extract snapping information from Strahler API response
#'
#' This helper function parses the API result returned by
#' `api_get_snapped_points_strahler()` and extracts key information
#' about the original and snapped coordinates, subcatchment IDs,
#' and resulting Strahler order.
#'
#' @param result API response of the api_get_snapped_points_strahler.R
#'
#' @return A tibble containing original/snapped coordinates, subcatchment
#'   IDs before and after snapping, and the Strahler order.
#'
#' @import dplyr
#' @keywords internal
extract_snapping_info <- function(result) {
  # Get before/after subcatchment IDs
  subc_before <- result$subc_id_before_snapping
  subc_after  <- result$subc_id_after_snapping

  # Extract point feature (snapped point)
  point_feature <- result$features[1,]

  if (nrow(point_feature) != 1) {
    warning("Unexpected number of Point features; skipping.")
    return(NULL)
  }

  # After snapping (geometry)
  lon_after <- point_feature$geometry$coordinates[[1]][1]
  lat_after <- point_feature$geometry$coordinates[[1]][2]
  strahler  <- point_feature$properties$strahler

  # Before snapping (properties)
  lon_before <- point_feature$properties$lon_original
  lat_before <- point_feature$properties$lat_original

  tibble::tibble(
    subc_id_before = subc_before,
    subc_id_after  = subc_after,
    lon_before = lon_before,
    lat_before = lat_before,
    lon_after  = lon_after,
    lat_after  = lat_after,
    strahler = strahler
  )
}


#' Standardize Point Input Column Names
#'
#' @description
#' This helper function renames user-supplied longitude, latitude, and site ID
#' columns to the standardized names expected by the internal workflow:
#' `lon_before`, `lat_before`, and `site_id`.
#' It ensures consistent column naming regardless of the original input
#' dataframe structure.
#'
#' @param df A data frame containing point locations to be standardized.
#' @param colname_lon Character. Name of the column in `df` containing longitude values.
#' @param colname_lat Character. Name of the column in `df` containing latitude values.
#' @param colname_site_id Character. Name of the column in `df` containing site IDs.
#'
#' @return
#' A data frame with renamed columns:
#' \describe{
#'   \item{lon_before}{Longitude column renamed from `colname_lon`.}
#'   \item{lat_before}{Latitude column renamed from `colname_lat`.}
#'   \item{site_id}{Site identifier column renamed from `colname_site_id`.}
#' }
#'
#' @import dplyr
#' @keywords internal
standardize_input <- function(df,
                              col_lon,
                              col_lat,
                              col_site_id) {

  df_std <- df %>%
    dplyr::rename(
      lon_before = all_of(col_lon),
      lat_before = all_of(col_lat),
      site_id    = all_of(col_site_id)
    )

  return(df_std)
}


#' Trace downstream until reaching a Strahler threshold
#'
#' @description
#' Internal helper function. Given a starting subcatchment ID, this function
#' follows the downstream `target` chain and collects all downstream
#' segments until one of the following conditions is met:
#'
#' - the next segment has a Strahler order greater than or equal to
#'   `strahler_retain_threshold`
#' - a segment has no downstream `target`
#' - a loop is detected
#'
#' @param start_id Integer. The starting subcatchment ID.
#' @param df A data.frame or tibble containing at least the columns
#'   `subc_id`, `target`, and `strahler_order`.
#' @param strahler_retain_threshold Integer. The Strahler order at or above
#'   which downstream tracing stops.
#'
#' @return An integer vector of subcatchment IDs traced downstream.
#'
#' @keywords internal

trace_downstream <- function(start_id, df, strahler_retain_threshold) {
  out <- integer(0)
  current <- start_id

  repeat {
    if (is.na(current)) break
    out <- c(out, current)

    row <- df[df$subc_id == current, ]
    if (nrow(row) == 0) break

    if (row$strahler_order >= strahler_retain_threshold)
      break

    next_id <- row$target
    if (is.na(next_id) || next_id %in% out)
      break

    current <- next_id
  }

  unique(out)
}


#' Trace N upstream steps from a subcatchment
#'
#' @description
#' Internal helper function. Starting from a given subcatchment ID, this
#' function walks upstream through all parent segments (those for which
#' `target == start_id`) for a fixed number of recursive steps. This is useful
#' for including contextual upstream segments near fine-order snapped points.
#'
#' @param start_id Integer. The starting subcatchment ID.
#' @param df A data.frame or tibble containing at least the columns
#'   `subc_id` and `target`.
#' @param steps Integer. Maximum number of upstream levels to traverse.
#'
#' @return An integer vector of subcatchment IDs traced upstream.
#'
#' @keywords internal

trace_upstream_n <- function(start_id, df, steps) {
  if (steps == 0) return(start_id)

  out <- start_id
  current <- start_id

  for (i in seq_len(steps)) {
    parents <- df$subc_id[df$target == current]
    if (length(parents) == 0) break

    out <- c(out, parents)
    current <- parents
  }

  unique(out)
}


#' Traverse upstream along a stream network from a given point
#'
#' @description
#' Extracts all segments upstream of a starting point along a stream network
#' up to a cumulative distance `X`. Handles branching and partial segments.
#'
#' @param lines_sf sf object with columns `subc_id`, `target`, `length`, and LINESTRING geometry
#' @param start_segment_id ID of the segment to start from
#' @param start_fraction Fraction along segment to start (0=start, 1=end)
#' @param X Numeric. Distance to traverse upstream
#'
#' @return list with elements:
#'   - `geoms`: list of LINESTRING geometries
#'   - `data`: list of segment attributes with updated lengths
#' @keywords internal
traverse_upstream <- function(lines_sf, start_segment_id, start_fraction, X) {
  # Build upstream adjacency map
  upstream_map <- split(lines_sf$subc_id, lines_sf$target)

  out_geoms <- list()
  out_data <- list()
  cumdist <- 0

  # First segment
  seg <- lines_sf %>% filter(subc_id == start_segment_id)
  if (nrow(seg) == 0) return(list(geoms = list(), data = list()))
  seg_len <- seg$length
  available <- seg_len * start_fraction

  if (available > 0) {
    clipped_geom <- lwgeom::st_linesubstring(st_geometry(seg)[[1]], 0, start_fraction)
    out_geoms <- list(clipped_geom)
    seg_data <- st_drop_geometry(seg)
    seg_data$length <- available
    out_data <- list(seg_data)
    cumdist <- available
  }

  # Queue for upstream traversal
  queue <- list(list(cumdist = cumdist, seg_id = start_segment_id))
  best_distance <- list()

  while (length(queue) > 0) {
    current <- queue[[1]]; queue <- queue[-1]
    seg_id <- as.character(current$seg_id)
    seg <- lines_sf %>% filter(subc_id == seg_id)
    if (nrow(seg) == 0) next
    seg_len <- seg$length
    seg_data <- st_drop_geometry(seg)

    # Skip if already reached with shorter distance
    if (seg_id %in% names(best_distance) && best_distance[[seg_id]] <= current$cumdist) next
    best_distance[[seg_id]] <- current$cumdist

    remaining <- X - current$cumdist
    if (remaining <= 0) next

    if (seg_len <= remaining) {
      out_geoms <- append(out_geoms, list(st_geometry(seg)[[1]]))
      seg_data$length <- seg_len
      out_data <- append(out_data, list(seg_data))
    } else {
      # Clip segment
      frac <- remaining / seg_len
      clipped_geom <- lwgeom::st_linesubstring(st_geometry(seg)[[1]], 1 - frac, 1)
      out_geoms <- append(out_geoms, list(clipped_geom))
      seg_data$length <- remaining
      out_data <- append(out_data, list(seg_data))
      next
    }

    # Add upstream segments to queue
    upstream_ids <- upstream_map[[seg_id]]
    if (!is.null(upstream_ids)) {
      for (up_id in upstream_ids) {
        queue[[length(queue) + 1]] <- list(
          cumdist = current$cumdist + seg_len,
          seg_id = up_id
        )
      }
    }
  }

  return(list(geoms = out_geoms, data = out_data))
}


#' Traverse downstream along a stream network from a given point
#'
#' @description
#' Extracts all segments downstream of a starting point along a stream network
#' up to a cumulative distance `X`. Handles partial segments.
#'
#' @param lines_sf sf object with columns `subc_id`, `target`, `length`, and LINESTRING geometry
#' @param start_segment_id ID of the segment to start from
#' @param start_fraction Fraction along segment to start (0=start, 1=end)
#' @param X Numeric. Distance to traverse downstream
#'
#' @return list with elements:
#'   - `geoms`: list of LINESTRING geometries
#'   - `data`: list of segment attributes with updated lengths
#' @keywords internal
traverse_downstream <- function(lines_sf, start_segment_id, start_fraction, X) {
  out_geoms <- list()
  out_data <- list()
  cumdist <- 0
  current_id <- start_segment_id

  while (!is.null(current_id)) {
    seg <- lines_sf %>% filter(subc_id == current_id)
    if (nrow(seg) == 0) break
    if (nrow(seg) > 1) seg <- seg[1, ]
    seg_len <- seg$length
    seg_data <- st_drop_geometry(seg)

    remaining <- X - cumdist
    if (remaining <= 0) break

    # Compute fraction to clip
    frac_start <- ifelse(current_id == start_segment_id, start_fraction, 0)

    if ((1 - frac_start) * seg_len <= remaining) {
      # Take full remaining segment
      out_geoms <- append(out_geoms, list(st_geometry(seg)[[1]]))
      seg_data$length <- seg_len * (1 - frac_start)
      out_data <- append(out_data, list(seg_data))
      cumdist <- cumdist + seg_data$length
    } else {
      # Clip partial segment
      frac_end <- frac_start + remaining / seg_len
      clipped_geom <- lwgeom::st_linesubstring(st_geometry(seg)[[1]], frac_start, frac_end)
      out_geoms <- append(out_geoms, list(clipped_geom))
      seg_data$length <- remaining
      out_data <- append(out_data, list(seg_data))
      break
    }

    # Move to next downstream segment
    next_id <- seg$target
    if (is.na(next_id) || next_id == "" || !next_id %in% lines_sf$subc_id) break
    current_id <- next_id
  }

  return(list(geoms = out_geoms, data = out_data))
}


#' Combine lists of geometries and data into an sf object
#'
#' @description
#' Safely combines a list of geometries and corresponding attribute data frames
#' into a single sf object. Ensures there is only one geometry column.
#'
#' @param geoms_list List of LINESTRING geometries
#' @param data_list List of data frames corresponding to each geometry
#' @param crs Coordinate reference system for output sf
#'
#' @return sf object combining all geometries and data
#' @keywords internal
combine_results_sf <- function(geoms_list, data_list, crs) {
  if (length(geoms_list) == 0) return(NULL)
  sf_list <- purrr::map2(data_list, geoms_list, ~ st_sf(.x, geometry = .y))
  final_sf <- do.call(rbind, sf_list)

  # Remove existing 'geom' if present to avoid duplicates
  if ("geom" %in% colnames(final_sf)) final_sf <- final_sf %>% select(-geom)

  st_geometry(final_sf) <- "geometry"
  st_crs(final_sf) <- crs
  return(final_sf)
}

