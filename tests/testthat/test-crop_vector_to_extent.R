###################################################
### Testing the crop_vector_to_extent() function ###
###################################################

# Cases to be covered:
# * normal case: crop lines (streams) with bounding box (test 1)
# * normal case: crop polygons (sub-catchments) with bounding box (test 2)
# * normal case: crop with a polygon clip layer (test 3)
# * normal case: crop using a spatial object (SpatRaster) as bounding box (test 4)
# * normal case: output as shapefile (.shp) (test 5)
# * normal case: output as GeoJSON (.geojson) (test 6)
# * normal case: read = FALSE returns NULL but writes file (test 7)
# * MUST FAIL: input file does not exist (test 8)
# * MUST FAIL: neither bounding_box nor clip_layer provided (test 9)
# * MUST FAIL: unsupported input format (.csv) (test 10)
# * MUST FAIL: unsupported output format (.csv) (test 11)
# * MUST FAIL: clip layer path does not exist (test 12)
# * MUST FAIL: output directory does not exist (test 13)


#########################
### Some preparations ###
#########################

# Where to store and download files:
if (!exists("tmpdir")) {
  tmpdir <- tempdir()
}

download_test_data(tmpdir)

# Input files (from hydrographr test data)
basin_vector  <- file.path(tmpdir, "hydrography90m_test_data", "basin_59.gpkg")
stream_vector <- file.path(tmpdir, "hydrography90m_test_data", "order_vect_59.gpkg")
subc_vector   <- file.path(tmpdir, "hydrography90m_test_data", "sub_catchment_59.gpkg")
spi_raster    <- file.path(tmpdir, "hydrography90m_test_data", "spi_1264942.tif")

# Define a crop bbox: middle half of the stream extent
stream_input <- sf::st_read(stream_vector, quiet = TRUE)
full_bbox    <- as.numeric(sf::st_bbox(stream_input))
crop_bbox <- c(
  full_bbox[1] + (full_bbox[3] - full_bbox[1]) * 0.25,
  full_bbox[2] + (full_bbox[4] - full_bbox[2]) * 0.25,
  full_bbox[3] - (full_bbox[3] - full_bbox[1]) * 0.25,
  full_bbox[4] - (full_bbox[4] - full_bbox[2]) * 0.25
)

# Create a circular clip polygon around the centroid of the stream layer
centroid   <- sf::st_centroid(sf::st_as_sfc(sf::st_bbox(stream_input)))
clip_circle <- sf::st_buffer(centroid, dist = 0.2)
clip_path  <- file.path(tmpdir, "test_clip_circle.gpkg")
sf::st_write(clip_circle, clip_path, delete_dsn = TRUE, quiet = TRUE)


#############
### Tests ###
#############

testname <- "1: Crop lines (streams) with bounding box"
test_that(testname, {

  result <- crop_vector_to_extent(
    vector_layer = stream_vector,
    bounding_box = crop_bbox,
    out_dir      = tmpdir,
    file_name    = "test1_streams_bbox.gpkg",
    read         = TRUE,
    quiet        = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  expect_lt(nrow(result), nrow(stream_input))
})


testname <- "2: Crop polygons (sub-catchments) with bounding box"
test_that(testname, {

  subc_input <- sf::st_read(subc_vector, quiet = TRUE)

  result <- crop_vector_to_extent(
    vector_layer = subc_vector,
    bounding_box = crop_bbox,
    out_dir      = tmpdir,
    file_name    = "test2_subc_bbox.gpkg",
    read         = TRUE,
    quiet        = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  expect_lt(nrow(result), nrow(subc_input))
})


testname <- "3: Crop with a polygon clip layer"
test_that(testname, {

  result <- crop_vector_to_extent(
    vector_layer = stream_vector,
    clip_layer   = clip_path,
    out_dir      = tmpdir,
    file_name    = "test3_streams_clip.gpkg",
    read         = TRUE,
    quiet        = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  expect_lt(nrow(result), nrow(stream_input))
})


testname <- "4: Crop using a spatial object (SpatRaster) as bounding box"
test_that(testname, {

  r <- terra::rast(spi_raster)

  result <- crop_vector_to_extent(
    vector_layer = stream_vector,
    bounding_box = r,
    out_dir      = tmpdir,
    file_name    = "test4_streams_rast_bbox.gpkg",
    read         = TRUE,
    quiet        = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
})


testname <- "5: Output as shapefile (.shp)"
test_that(testname, {

  expect_warning(
    result <- crop_vector_to_extent(
      vector_layer = stream_vector,
      bounding_box = crop_bbox,
      out_dir      = tmpdir,
      file_name    = "test5_streams.shp",
      read         = TRUE,
      quiet        = TRUE
    )
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  expect_true(file.exists(file.path(tmpdir, "test5_streams.shp")))
})


testname <- "6: Output as GeoJSON (.geojson)"
test_that(testname, {

  result <- crop_vector_to_extent(
    vector_layer = stream_vector,
    bounding_box = crop_bbox,
    out_dir      = tmpdir,
    file_name    = "test6_streams.geojson",
    read         = TRUE,
    quiet        = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  expect_true(file.exists(file.path(tmpdir, "test6_streams.geojson")))
})


testname <- "7: read = FALSE returns NULL and writes file to disk"
test_that(testname, {

  result <- crop_vector_to_extent(
    vector_layer = stream_vector,
    bounding_box = crop_bbox,
    out_dir      = tmpdir,
    file_name    = "test7_noread.gpkg",
    read         = FALSE,
    quiet        = TRUE
  )

  expect_null(result)
  expect_true(file.exists(file.path(tmpdir, "test7_noread.gpkg")))
})


testname <- "8: MUST FAIL: input file does not exist"
test_that(testname, {

  expect_error(
    crop_vector_to_extent(
      vector_layer = "nonexistent_file.gpkg",
      bounding_box = crop_bbox,
      out_dir      = tmpdir,
      file_name    = "should_fail.gpkg"
    ),
    regexp = "does not exist"
  )
})


testname <- "9: MUST FAIL: neither bounding_box nor clip_layer provided"
test_that(testname, {

  expect_error(
    crop_vector_to_extent(
      vector_layer = stream_vector,
      out_dir      = tmpdir,
      file_name    = "should_fail.gpkg"
    ),
    regexp = "clip layer or bounding box"
  )
})


testname <- "10: MUST FAIL: unsupported input file format (.csv)"
test_that(testname, {

  expect_error(
    crop_vector_to_extent(
      vector_layer = "file.csv",
      bounding_box = crop_bbox,
      out_dir      = tmpdir,
      file_name    = "should_fail.gpkg"
    ),
    regexp = "\\.shp, \\.gpkg, or \\.geojson"
  )
})


testname <- "11: MUST FAIL: unsupported output file format (.csv)"
test_that(testname, {

  expect_error(
    crop_vector_to_extent(
      vector_layer = stream_vector,
      bounding_box = crop_bbox,
      out_dir      = tmpdir,
      file_name    = "should_fail.csv"
    ),
    regexp = "\\.gpkg, \\.shp, or \\.geojson"
  )
})


testname <- "12: MUST FAIL: clip layer path does not exist"
test_that(testname, {

  expect_error(
    crop_vector_to_extent(
      vector_layer = stream_vector,
      clip_layer   = "nonexistent_clip.gpkg",
      out_dir      = tmpdir,
      file_name    = "should_fail.gpkg"
    ),
    regexp = "does not exist"
  )
})


testname <- "13: MUST FAIL: output directory does not exist"
test_that(testname, {

  expect_error(
    crop_vector_to_extent(
      vector_layer = stream_vector,
      bounding_box = crop_bbox,
      out_dir      = "/nonexistent/directory",
      file_name    = "should_fail.gpkg"
    ),
    regexp = "does not exist"
  )
})
