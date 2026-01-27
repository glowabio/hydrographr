#' Extract partial stream network needed for snapped points
#'
#' @description
#' Given a stream network and snapped point subcatchments, this returns the
#' reduced stream network consisting of:
#' - All segments with Strahler order >= `strahler_retain_threshold`
#' - All downstream paths from finer-order snapped segments until reaching
#'   a segment with Strahler >= `strahler_retain_threshold`
#' - PLUS a configurable number of *upstream* same-Strahler segments for context
#'
#' @param stream sf object with columns:
#'   `subc_id`, `target`, `strahler_order`, and LINESTRING geometry.
#' @param snapped_subcs integer vector: subc_ids after snapping.
#' @param strahler_retain_threshold integer: minimum Strahler to fully retain (default 4).
#' @param upstream_buffer integer: how many same-Strahler upstream segments to retain.
#'
#' @return sf object containing the partial stream network.
#' @export
extract_partial_stream_network <- function(
    stream,
    snapped_subcs,
    strahler_retain_threshold = 4,
    upstream_buffer = 0      # number of upstream segments to include
) {


  # --- 1: keep all >= strahler_retain_threshold ---
  stream_high <- subset(stream, strahler_order >= strahler_retain_threshold)

  # --- 2: fine-order snapped segments (strict original condition) ---
  snapped_fine <- intersect(
    snapped_subcs,
    stream$subc_id[stream$strahler_order < strahler_retain_threshold]
  )

  # IMPORTANT: always retain snapped segments exactly as original
  snapped_fine <- unique(snapped_fine)

  # --- 3: downstream tracing for fine snapped segments ---
  downstream_ids <- unique(unlist(
    purrr::map(snapped_fine, trace_downstream,
               df = stream, strahler_retain_threshold = strahler_retain_threshold)
  ))

  # --- 4: upstream N steps (buffer) for same snapped segments ---
  upstream_ids <- unique(unlist(
    purrr::map(snapped_fine, trace_upstream_n,
               df = stream, steps = upstream_buffer)
  ))

  # --- 5: merge needed IDs ---
  needed_ids <- unique(c(downstream_ids, upstream_ids, snapped_fine))

  # --- 6: extract partial network ---
  stream_needed <- subset(stream, subc_id %in% needed_ids)

  # --- 7: union with high strahler segments ---
  out <- rbind(stream_high, stream_needed)

  return(out)
}
