##################################################
### Testing the api_get_ids() function         ###
##################################################

# Cases covered:
# --- MODE: BASIN ---
# * normal case: single basin_id                                        (test 1)
# * normal case: single basin_id with min_strahler filter              (test 2)
# * normal case: multiple basin_ids                                     (test 3)
# * normal case: input via subc_ids                                     (test 4)
# * normal case: input via points data.frame                            (test 5)
# * normal case: Strahler filter shrinks result                         (test 6)
# --- MODE: UPSTREAM ---
# * normal case: single site, correct columns returned                  (test 7)
# * normal case: multiple sites, all site_ids present                   (test 8)
# * normal case: non-default column names preserved in output           (test 9)
# --- MODE: LOCAL ---
# * normal case: returns data.frame with three ID columns appended      (test 10)
# * normal case: input column names preserved in output                 (test 11)
# * normal case: non-default column names preserved in output           (test 12)
# * normal case: different location (Amiens, France)                    (test 13)
# * cross-check: local basin_id contains local subc_id (basin mode)    (test 14)
# * cross-check: upstream subc_id matches local subc_id                (test 15)
# --- ERROR CASES ---
# * MUST FAIL: invalid mode                                             (test 16)
# * MUST FAIL: basin mode, no input provided                            (test 17)
# * MUST FAIL: basin mode, multiple inputs provided                     (test 18)
# * MUST FAIL: basin mode, non-numeric basin_ids                        (test 19)
# * MUST FAIL: upstream mode, no points provided                        (test 20)
# * MUST FAIL: upstream mode, missing site_id column                    (test 21)
# * MUST FAIL: local mode, no points provided                           (test 22)
# * MUST FAIL: local mode, missing lon/lat columns                      (test 23)


#########################
### Some preparations ###
#########################

basin_id_schlei <- 1288419
basin_ids_multi <- c(1293500, 1173222)
subc_ids_two    <- c(506319029, 509342352)

# Points with default column names
pts_df <- data.frame(
  site_id   = c("A", "B"),
  longitude = c(10.217977, 10.233422),
  latitude  = c(54.301799, 54.314711)
)

sites1 <- data.frame(
  site_id   = "schlei",
  longitude = 9.931555,
  latitude  = 54.695070
)

sites2 <- data.frame(
  site_id   = c("S1", "S2", "S3"),
  longitude = c(9.931555, 10.217977, 2.172944),
  latitude  = c(54.695070, 54.301799, 49.914233)
)

# Points with non-default column names
sites_custom <- data.frame(
  id  = "test",
  lon = 10.217977,
  lat = 54.301799
)

# Single point for local + cross-check tests
lon_check <- 10.217977
lat_check <- 54.301799

sites_check <- data.frame(
  site_id   = "check",
  longitude = lon_check,
  latitude  = lat_check
)


#############
### Tests ###
#############

# --- MODE: BASIN ---

testname <- "1: Single basin_id returns integer vector"
test_that(testname, {

  result <- api_get_ids(basin_ids = basin_id_schlei, mode = "basin")

  expect_type(result, "integer")
  expect_gt(length(result), 0)
})


testname <- "2: Single basin_id with min_strahler returns integer vector"
test_that(testname, {

  result <- api_get_ids(basin_ids = basin_id_schlei, mode = "basin",
                        min_strahler = 4)

  expect_type(result, "integer")
  expect_gt(length(result), 0)
})


testname <- "3: Multiple basin_ids returns combined integer vector"
test_that(testname, {

  result <- api_get_ids(basin_ids = basin_ids_multi, mode = "basin")

  expect_type(result, "integer")
  expect_gt(length(result), 0)
})


testname <- "4: Input via subc_ids returns integer vector containing inputs"
test_that(testname, {

  result <- api_get_ids(subc_ids = subc_ids_two, mode = "basin")

  expect_type(result, "integer")
  expect_gt(length(result), 0)
  expect_true(all(subc_ids_two %in% result))
})


testname <- "5: Input via points data.frame returns integer vector"
test_that(testname, {

  result <- api_get_ids(points = pts_df, mode = "basin")

  expect_type(result, "integer")
  expect_gt(length(result), 0)
})


testname <- "6: Higher min_strahler returns fewer subcatchments"
test_that(testname, {

  r3 <- api_get_ids(basin_ids = basin_id_schlei, mode = "basin", min_strahler = 3)
  r5 <- api_get_ids(basin_ids = basin_id_schlei, mode = "basin", min_strahler = 5)

  expect_gt(length(r3), length(r5))
})


# --- MODE: UPSTREAM ---

testname <- "7: Single site returns data.frame with correct columns"
test_that(testname, {

  result <- api_get_ids(points = sites1, mode = "upstream")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("site_id", "subc_id", "upstream_id") %in% colnames(result)))
  expect_gt(nrow(result), 0)
  expect_equal(unique(result$site_id), "schlei")
})


testname <- "8: Multiple sites - all site_ids present in result"
test_that(testname, {

  result <- api_get_ids(points = sites2, mode = "upstream")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("S1", "S2", "S3") %in% result$site_id))
  counts <- table(result$site_id)
  expect_true(all(counts > 0))
})


testname <- "9: Non-default column names are preserved in upstream output"
test_that(testname, {

  result <- api_get_ids(
    points          = sites_custom,
    mode            = "upstream",
    colname_lon     = "lon",
    colname_lat     = "lat",
    colname_site_id = "id"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  # site_id in output should reflect the value from the "id" column
  expect_equal(unique(result$site_id), "test")
})


# --- MODE: LOCAL ---

testname <- "10: Local mode returns data.frame with ID columns appended"
test_that(testname, {

  result <- api_get_ids(points = sites_check, mode = "local")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(sites_check))
  # ID columns appended
  expect_true(all(c("subc_id", "basin_id", "reg_id") %in% colnames(result)))
  # No NAs in ID columns
  expect_false(any(is.na(result$subc_id)))
  expect_false(any(is.na(result$basin_id)))
  expect_false(any(is.na(result$reg_id)))
})


testname <- "11: Local mode preserves input column names in output"
test_that(testname, {

  result <- api_get_ids(points = sites_check, mode = "local")

  # All original columns must still be present
  expect_true(all(colnames(sites_check) %in% colnames(result)))
  # Values must be unchanged
  expect_equal(result$site_id,   sites_check$site_id)
  expect_equal(result$longitude, sites_check$longitude)
  expect_equal(result$latitude,  sites_check$latitude)
})


testname <- "12: Local mode preserves non-default column names"
test_that(testname, {

  result <- api_get_ids(
    points          = sites_custom,
    mode            = "local",
    colname_lon     = "lon",
    colname_lat     = "lat",
    colname_site_id = "id"
  )

  expect_s3_class(result, "data.frame")
  # Original non-default columns must be present
  expect_true(all(c("id", "lon", "lat") %in% colnames(result)))
  # ID columns appended
  expect_true(all(c("subc_id", "basin_id", "reg_id") %in% colnames(result)))
  # Values unchanged
  expect_equal(result$lon, sites_custom$lon)
  expect_equal(result$lat, sites_custom$lat)
})


testname <- "13: Local mode works for a different location (Amiens, France)"
test_that(testname, {

  amiens <- data.frame(
    site_id   = "amiens",
    longitude = 2.172944,
    latitude  = 49.914233
  )

  result <- api_get_ids(points = amiens, mode = "local")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_false(is.na(result$subc_id))
  expect_false(is.na(result$basin_id))
  expect_false(is.na(result$reg_id))
})


testname <- "14: Cross-check: local basin_id contains local subc_id"
test_that(testname, {

  local_result <- api_get_ids(points = sites_check, mode = "local")
  basin_result <- api_get_ids(basin_ids = local_result$basin_id, mode = "basin")

  expect_true(local_result$subc_id %in% basin_result)
})


testname <- "15: Cross-check: upstream subc_id matches local subc_id"
test_that(testname, {

  up    <- api_get_ids(points = sites_check, mode = "upstream")
  local <- api_get_ids(points = sites_check, mode = "local")

  expect_equal(unique(up$subc_id), local$subc_id)
})


# --- ERROR CASES ---

testname <- "16: MUST FAIL: invalid mode"
test_that(testname, {

  expect_error(
    api_get_ids(basin_ids = basin_id_schlei, mode = "banana"),
    regexp = "mode"
  )
})


testname <- "17: MUST FAIL: basin mode with no input"
test_that(testname, {

  expect_error(
    api_get_ids(mode = "basin"),
    regexp = "must provide one of"
  )
})


testname <- "18: MUST FAIL: basin mode with multiple inputs"
test_that(testname, {

  expect_error(
    api_get_ids(basin_ids = basin_id_schlei, subc_ids = subc_ids_two,
                mode = "basin"),
    regexp = "only one"
  )
})


testname <- "19: MUST FAIL: basin mode with non-numeric basin_ids"
test_that(testname, {

  expect_error(
    api_get_ids(basin_ids = "abc", mode = "basin"),
    regexp = "numeric"
  )
})


testname <- "20: MUST FAIL: upstream mode with no points"
test_that(testname, {

  expect_error(
    api_get_ids(mode = "upstream"),
    regexp = "non-empty data.frame"
  )
})


testname <- "21: MUST FAIL: upstream mode with missing site_id column"
test_that(testname, {

  bad_df <- data.frame(longitude = 9.93, latitude = 54.7)

  expect_error(
    api_get_ids(points = bad_df, mode = "upstream"),
    regexp = "site_id"
  )
})


testname <- "22: MUST FAIL: local mode with no points"
test_that(testname, {

  expect_error(
    api_get_ids(mode = "local"),
    regexp = "non-empty data.frame"
  )
})


testname <- "23: MUST FAIL: local mode with missing lon/lat columns"
test_that(testname, {

  bad_df <- data.frame(site_id = "x", some_col = 1)

  expect_error(
    api_get_ids(points = bad_df, mode = "local"),
    regexp = "longitude|latitude|lon|lat"
  )
})
