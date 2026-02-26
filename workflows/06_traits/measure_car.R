library(imager)
library(magick)

# Load image
img <- load.image("traits/barbus_prespensis01-500.jpg")

# Display and click points
plot(img)
# Click to mark:
# 1-2: Total length line (for scale)
# 3-4: Fin height (tip to tip)
# Then trace fin outline for area

# Manual measurement script
measure_CAR <- function(img_path, total_length_cm = 30) {

  # Load and display image LARGE
  img <- image_read(img_path)

  # Get image info
  info <- image_info(img)
  cat("Image dimensions:", info$width, "x", info$height, "\n\n")

  # Open a large plotting window
  dev.new(width = 14, height = 10)  # Large window

  # Display image
  plot(img)

  # STEP 1: Set scale
  cat("STEP 1: Click TWO points along the 30cm body length\n")
  cat("(Click point 1, then point 2)\n")
  scale_points <- locator(2, type = "p", pch = 20, col = "red", cex = 2)

  # Draw scale line
  lines(scale_points$x, scale_points$y, col = "red", lwd = 3)

  scale_pixels <- sqrt((scale_points$x[2] - scale_points$x[1])^2 +
                         (scale_points$y[2] - scale_points$y[1])^2)
  pixels_per_cm <- scale_pixels / total_length_cm

  cat("Scale set:", round(pixels_per_cm, 2), "pixels per cm\n\n")

  # STEP 2: Measure fin height
  cat("STEP 2: Click fin height (top lobe tip, then bottom lobe tip)\n")
  fin_points <- locator(2, type = "p", pch = 20, col = "blue", cex = 2)

  # Draw height line
  lines(fin_points$x, fin_points$y, col = "blue", lwd = 3)

  fin_height_pixels <- sqrt((fin_points$x[2] - fin_points$x[1])^2 +
                              (fin_points$y[2] - fin_points$y[1])^2)
  fin_height_cm <- fin_height_pixels / pixels_per_cm

  cat("Fin height:", round(fin_height_cm, 2), "cm\n\n")

  # STEP 3: Trace fin outline - FIXED VERSION
  cat("STEP 3: Trace fin outline\n")
  cat("Click points around the fin edge\n")
  cat("Press ESC when done (not right-click)\n\n")

  # Collect points until user presses ESC
  area_points <- locator(n = 1000, type = "o", col = "green", lwd = 2)

  # Close polygon
  if (length(area_points$x) > 2) {
    lines(c(area_points$x[length(area_points$x)], area_points$x[1]),
          c(area_points$y[length(area_points$y)], area_points$y[1]),
          col = "green", lwd = 2)
  }

  # Calculate polygon area (shoelace formula)
  n <- length(area_points$x)
  if (n < 3) {
    stop("Need at least 3 points to calculate area")
  }

  # Add first point to end to close polygon
  x <- c(area_points$x, area_points$x[1])
  y <- c(area_points$y, area_points$y[1])

  area_pixels <- 0.5 * abs(sum(x[1:n] * y[2:(n+1)] - x[2:(n+1)] * y[1:n]))
  area_cm2 <- area_pixels / (pixels_per_cm^2)

  cat("Fin area:", round(area_cm2, 2), "cm²\n")
  cat("Number of outline points:", n, "\n\n")

  # Calculate CAR
  CAR <- (fin_height_cm^2) / area_cm2

  # Summary
  cat("=" , rep("=", 50), "\n", sep = "")
  cat("RESULTS:\n")
  cat("  Fin height (h):", round(fin_height_cm, 2), "cm\n")
  cat("  Fin area (A):  ", round(area_cm2, 2), "cm²\n")
  cat("  CAR = h²/A:    ", round(CAR, 2), "\n")
  cat("=" , rep("=", 50), "\n", sep = "")

  # Sanity check
  if (CAR < 1.5) {
    warning("CAR is very low (<1.5) - check if fin outline is correct")
  } else if (CAR > 5.0) {
    warning("CAR is very high (>5.0) - check if fin outline is correct")
  } else {
    cat("\n✓ CAR value looks reasonable for a forked fin (2.5-4.0)\n")
  }

  return(list(
    height_cm = fin_height_cm,
    area_cm2 = area_cm2,
    CAR = CAR,
    n_points = n
  ))
}

# Use it
results <- measure_CAR("traits/barbus_prespensis01-500.jpg", total_length_cm = 30)
