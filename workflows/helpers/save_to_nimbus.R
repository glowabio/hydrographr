save_to_nimbus <- function(data, filename, save_function = NULL, ...) {
  # Create local temp path
  local_path <- file.path(tempdir(), basename(filename))

  # Save locally first
  if (is.null(save_function)) {
    # Auto-detect save function based on file extension
    ext <- tools::file_ext(filename)
    if (ext == "html") {
      # HTML widget
      htmlwidgets::saveWidget(data, local_path, selfcontained = TRUE)
    } else if (ext %in% c("gpkg", "shp", "geojson")) {
      # Spatial data
      # For GPKG, use layer name from filename if not provided
      if (ext == "gpkg" && !"layer" %in% names(list(...))) {
        layer_name <- tools::file_path_sans_ext(basename(filename))
        sf::st_write(data, local_path, layer = layer_name, delete_dsn = TRUE, quiet = TRUE, ...)
      } else {
        sf::st_write(data, local_path, delete_dsn = TRUE, quiet = TRUE, ...)
      }
    } else if (ext == "csv") {
      # CSV
      data.table::fwrite(data, local_path, ...)
    } else if (ext == "rds") {
      # RDS
      saveRDS(data, local_path, ...)
    } else {
      stop("Unknown file type. Please provide save_function argument.")
    }
  } else {
    # Use custom save function
    save_function(data, local_path, ...)
  }

  # Verify local file was created and has size
  if (!file.exists(local_path)) {
    stop("Local file was not created: ", local_path)
  }
  local_size <- file.info(local_path)$size
  if (local_size == 0) {
    stop("Local file has 0 bytes: ", local_path)
  }

  # Copy to Nimbus
  nimbus_dest <- file.path(getwd(), filename)

  # Create directory if needed
  dir.create(dirname(nimbus_dest), recursive = TRUE, showWarnings = FALSE)

  # Use system cp command for WebDAV mounts
  copy_result <- system2("cp", args = c(shQuote(local_path), shQuote(nimbus_dest)))
  if (copy_result != 0) {
    stop("Failed to copy file to Nimbus: ", nimbus_dest)
  }

  # Clean up
  unlink(local_path)

  message(sprintf("✓ Saved to Nimbus: %s", filename))
  invisible(nimbus_dest)
}
