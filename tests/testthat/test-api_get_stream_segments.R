##################################################
### Testing the api_get_stream_segments() function
##################################################

# Cases covered:
# --- MODE: BASIN (upstream = FALSE) ---
# * normal case: basin_id returns sf LINESTRING object             (test 1)
# * normal case: subc_id returns sf LINESTRING object              (test 2)
# * normal case: lon/lat returns sf LINESTRING object              (test 3)
# * normal case: min_strahler filters result                       (test 4)
# * normal case: geometry_only = TRUE returns sf without attrs     (test 5)
# * normal case: add_segment_ids = TRUE adds column to result      (test 6)
# --- MODE: UPSTREAM (upstream = TRUE) ---
# * normal case: subc_id returns upstream sf LINESTRING            (test 7)
# * normal case: lon/lat returns upstream sf LINESTRING            (test 8)
# * normal case: min_strahler filters upstream result              (test 9)
# * normal case: add_upstream_ids = TRUE adds column               (test 10)
# * normal case: geometry_only = TRUE in upstream mode             (test 11)
# --- CROSS-CHECKS ---
# * cross-check: upstream result is subset of basin result         (test 12)
# * cross-check: min_strahler reduces number of segments           (test 13)
# --- ERROR CASES ---
# * MUST FAIL: no input provided                                   (test 14)
# * MUST FAIL: basin_id with upstream = TRUE                       (test 15)
# * MUST FAIL: upstream = TRUE with no subc_id or lon/lat          (test 16)
# * MUST FAIL: non-numeric basin_id                                (test 17)
# * MUST FAIL: non-numeric subc_id                                 (test 18)
# * MUST FAIL: lat out of range                                    (test 19)
# * MUST FAIL: lon out of range                                    (test 20)
# * MUST FAIL: invalid upstream parameter type                     (test 21)
# * MUST FAIL: min_strahler below 1                                (test 23)


#########################
### Some preparations ###
#########################

# Known valid inputs (Schlei area, Germany)
basin_id_schlei <- 1288419
subc_id_schlei  <- 506586041
lon_schlei      <- 9.931555
lat_schlei      <- 54.695070

# Known valid inputs (Sarantaporos outlet, Greece)
lon_greece <- 20.538704
lat_greece <- 40.113735


#############
### Tests ###
#############

# --- MODE: BASIN ---

testname <- "1: Basin mode with basin_id returns sf LINESTRING"
test_that(testname, {

  result <- api_get_stream_segments(
    basin_id = basin_id_schlei,
    upstream = FALSE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  geom_types <- as.character(unique(sf::st_geometry_type(result)))
  expect_true("LINESTRING" %in% geom_types)
})


testname <- "2: Basin mode with subc_id returns sf LINESTRING"
test_that(testname, {

  result <- api_get_stream_segments(
    subc_id  = subc_id_schlei,
    upstream = FALSE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  geom_types <- as.character(unique(sf::st_geometry_type(result)))
  expect_true("LINESTRING" %in% geom_types)
})


testname <- "3: Basin mode with lon/lat returns sf LINESTRING"
test_that(testname, {

  result <- api_get_stream_segments(
    lon      = lon_schlei,
    lat      = lat_schlei,
    upstream = FALSE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  geom_types <- as.character(unique(sf::st_geometry_type(result)))
  expect_true("LINESTRING" %in% geom_types)
})


testname <- "4: Basin mode with min_strahler returns fewer segments"
test_that(testname, {

  result_all <- api_get_stream_segments(
    basin_id = basin_id_schlei,
    upstream = FALSE
  )

  result_filtered <- api_get_stream_segments(
    basin_id     = basin_id_schlei,
    upstream     = FALSE,
    min_strahler = 4
  )

  expect_s3_class(result_filtered, "sf")
  expect_gt(nrow(result_all), nrow(result_filtered))
})


testname <- "5: Basin mode with geometry_only = TRUE returns sf without attributes"
test_that(testname, {

  result <- api_get_stream_segments(
    basin_id      = basin_id_schlei,
    upstream      = FALSE,
    geometry_only = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  # geometry_only means only the geometry column, no other attributes
  non_geom_cols <- setdiff(colnames(result), attr(result, "sf_column"))
  expect_equal(length(non_geom_cols), 0)
})


testname <- "6: Basin mode with add_segment_ids = TRUE adds column to output"
test_that(testname, {

  result <- api_get_stream_segments(
    basin_id        = basin_id_schlei,
    upstream        = FALSE,
    add_segment_ids = TRUE
  )

  expect_s3_class(result, "sf")
  # Should have at least one non-geometry column (the segment ID column)
  non_geom_cols <- setdiff(colnames(result), attr(result, "sf_column"))
  expect_gt(length(non_geom_cols), 0)
})


# --- MODE: UPSTREAM ---

testname <- "7: Upstream mode with subc_id returns sf LINESTRING"
test_that(testname, {

  result <- api_get_stream_segments(
    subc_id  = subc_id_schlei,
    upstream = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  geom_types <- as.character(unique(sf::st_geometry_type(result)))
  expect_true("LINESTRING" %in% geom_types)
})


testname <- "8: Upstream mode with lon/lat returns sf LINESTRING"
test_that(testname, {

  result <- api_get_stream_segments(
    lon      = lon_greece,
    lat      = lat_greece,
    upstream = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  geom_types <- as.character(unique(sf::st_geometry_type(result)))
  expect_true("LINESTRING" %in% geom_types)
})


testname <- "10: Upstream mode with add_upstream_ids = TRUE adds column"
test_that(testname, {

  result <- api_get_stream_segments(
    subc_id          = subc_id_schlei,
    upstream         = TRUE,
    add_upstream_ids = TRUE
  )

  expect_s3_class(result, "sf")
  non_geom_cols <- setdiff(colnames(result), attr(result, "sf_column"))
  expect_gt(length(non_geom_cols), 0)
})


testname <- "11: Upstream mode with geometry_only = TRUE returns sf without attributes"
test_that(testname, {

  result <- api_get_stream_segments(
    subc_id       = subc_id_schlei,
    upstream      = TRUE,
    geometry_only = TRUE
  )

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  non_geom_cols <- setdiff(colnames(result), attr(result, "sf_column"))
  expect_equal(length(non_geom_cols), 0)
})


# --- CROSS-CHECKS ---

testname <- "12: Upstream result has fewer segments than basin result"
test_that(testname, {

  basin_result <- api_get_stream_segments(
    subc_id  = subc_id_schlei,
    upstream = FALSE
  )

  upstream_result <- api_get_stream_segments(
    subc_id  = subc_id_schlei,
    upstream = TRUE
  )

  # Upstream can only be a subset of the basin
  expect_lte(nrow(upstream_result), nrow(basin_result))
})


testname <- "13: Higher min_strahler progressively reduces segment count"
test_that(testname, {

  r1 <- api_get_stream_segments(basin_id = basin_id_schlei, upstream = FALSE,
                                min_strahler = 1)
  r3 <- api_get_stream_segments(basin_id = basin_id_schlei, upstream = FALSE,
                                min_strahler = 3)
  r5 <- api_get_stream_segments(basin_id = basin_id_schlei, upstream = FALSE,
                                min_strahler = 5)

  expect_gte(nrow(r1), nrow(r3))
  expect_gte(nrow(r3), nrow(r5))
})


# --- ERROR CASES ---

testname <- "14: MUST FAIL: no input provided"
test_that(testname, {

  expect_error(
    api_get_stream_segments(),
    regexp = "basin_id|subc_id|lon"
  )
})


testname <- "15: MUST FAIL: basin_id with upstream = TRUE"
test_that(testname, {

  expect_error(
    api_get_stream_segments(basin_id = basin_id_schlei, upstream = TRUE),
    regexp = "basin_id.*upstream|upstream.*basin_id"
  )
})


testname <- "16: MUST FAIL: upstream = TRUE with no location input"
test_that(testname, {

  expect_error(
    api_get_stream_segments(upstream = TRUE),
    regexp = "subc_id|lon"
  )
})


testname <- "17: MUST FAIL: non-numeric basin_id"
test_that(testname, {

  expect_error(
    api_get_stream_segments(basin_id = "abc"),
    regexp = "numeric"
  )
})


testname <- "18: MUST FAIL: non-numeric subc_id"
test_that(testname, {

  expect_error(
    api_get_stream_segments(subc_id = "abc"),
    regexp = "numeric"
  )
})


testname <- "19: MUST FAIL: lat out of range"
test_that(testname, {

  expect_error(
    api_get_stream_segments(lon = lon_schlei, lat = 95),
    regexp = "-90 and 90|lat"
  )
})


testname <- "20: MUST FAIL: lon out of range"
test_that(testname, {

  expect_error(
    api_get_stream_segments(lon = 200, lat = lat_schlei),
    regexp = "-180 and 180|lon"
  )
})


testname <- "21: MUST FAIL: upstream is not logical"
test_that(testname, {

  expect_error(
    api_get_stream_segments(basin_id = basin_id_schlei, upstream = "yes"),
    regexp = "logical"
  )
})



testname <- "22: MUST FAIL: min_strahler below 1"
test_that(testname, {

  expect_error(
    api_get_stream_segments(basin_id = basin_id_schlei, min_strahler = 0),
    regexp = "positive"
  )
})
