##################################################
### Testing the api_get_ids() function         ###
##################################################

# Cases covered:
# --- MODE: BASIN ---
# * normal case: single basin_id                                  (test 1)
# * normal case: single basin_id with min_strahler filter         (test 2)
# * normal case: multiple basin_ids                               (test 3)
# * normal case: input via subc_ids                               (test 4)
# * normal case: input via points data.frame                      (test 5)
# * normal case: Strahler filter shrinks result                   (test 6)
# --- MODE: UPSTREAM ---
# * normal case: single site                                      (test 7)
# * normal case: multiple sites, all site_ids present             (test 8)
# * normal case: non-default column names                         (test 9)
# --- MODE: POINT ---
# * normal case: all three IDs returned                           (test 10)
# * normal case: different location                               (test 11)
# * cross-check: point basin_id contains point subc_id            (test 12)
# * cross-check: upstream subc_id matches point subc_id           (test 13)
# --- ERROR CASES ---
# * MUST FAIL: invalid mode                                       (test 14)
# * MUST FAIL: basin mode, no input provided                      (test 15)
# * MUST FAIL: basin mode, multiple inputs provided               (test 16)
# * MUST FAIL: basin mode, non-numeric basin_ids                  (test 17)
# * MUST FAIL: upstream mode, no points provided                  (test 18)
# * MUST FAIL: upstream mode, missing site_id column              (test 19)
# * MUST FAIL: point mode, missing lon                            (test 20)
# * MUST FAIL: point mode, lon out of range                       (test 21)


#########################
### Some preparations ###
#########################

# Reusable input objects

basin_id_schlei  <- 1288419
basin_ids_multi  <- c(1293500, 1173222)

subc_ids_two     <- c(506319029, 509342352)

pts_df <- data.frame(
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

sites3 <- data.frame(
  id  = "test",
  lon = 10.217977,
  lat = 54.301799
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


testname <- "2: Single basin_id with min_strahler returns fewer subcatchments"
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


testname <- "4: Input via subc_ids returns integer vector"
test_that(testname, {

  result <- api_get_ids(subc_ids = subc_ids_two, mode = "basin")

  expect_type(result, "integer")
  expect_gt(length(result), 0)
  # The input subc_ids should be contained in the result
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
  # One row per upstream subcatchment - could be many
  expect_equal(unique(result$site_id), "schlei")
})


testname <- "8: Multiple sites - all site_ids present in result"
test_that(testname, {

  result <- api_get_ids(points = sites2, mode = "upstream")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("S1", "S2", "S3") %in% result$site_id))
  # Each site should have at least one upstream segment
  counts <- table(result$site_id)
  expect_true(all(counts > 0))
})


testname <- "9: Non-default column names work correctly"
test_that(testname, {

  result <- api_get_ids(
    points          = sites3,
    mode            = "upstream",
    colname_lon     = "lon",
    colname_lat     = "lat",
    colname_site_id = "id"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_equal(unique(result$site_id), "test")
})


# --- MODE: POINT ---

testname <- "10: Point mode returns list with subc_id, basin_id, reg_id"
test_that(testname, {

  result <- api_get_ids(lon = 10.7, lat = 53.5, mode = "point")

  expect_type(result, "list")
  expect_true(all(c("subc_id", "basin_id", "reg_id", "coordinates") %in% names(result)))
  expect_false(is.na(result$subc_id))
  expect_false(is.na(result$basin_id))
  expect_false(is.na(result$reg_id))
})


testname <- "11: Point mode works for a different location (Amiens, France)"
test_that(testname, {

  result <- api_get_ids(lon = 2.172944, lat = 49.914233, mode = "point")

  expect_type(result, "list")
  expect_false(is.na(result$subc_id))
  expect_false(is.na(result$basin_id))
  expect_false(is.na(result$reg_id))
  # Coordinates echoed back correctly
  expect_equal(result$coordinates[["lon"]], 2.172944)
  expect_equal(result$coordinates[["lat"]], 49.914233)
})


testname <- "12: Cross-check: point basin_id contains point subc_id"
test_that(testname, {

  pt  <- api_get_ids(lon = 10.217977, lat = 54.301799, mode = "point")
  bsn <- api_get_ids(basin_ids = pt$basin_id, mode = "basin")

  expect_true(pt$subc_id %in% bsn)
})


testname <- "13: Cross-check: upstream subc_id matches point subc_id"
test_that(testname, {

  sites_check <- data.frame(
    site_id   = "check",
    longitude = 10.217977,
    latitude  = 54.301799
  )

  up  <- api_get_ids(points = sites_check, mode = "upstream")
  pt  <- api_get_ids(lon = 10.217977, lat = 54.301799, mode = "point")

  expect_equal(unique(up$subc_id), pt$subc_id)
})


# --- ERROR CASES ---

testname <- "14: MUST FAIL: invalid mode"
test_that(testname, {

  expect_error(
    api_get_ids(basin_ids = basin_id_schlei, mode = "banana"),
    regexp = "mode"
  )
})


testname <- "15: MUST FAIL: basin mode with no input"
test_that(testname, {

  expect_error(
    api_get_ids(mode = "basin"),
    regexp = "must provide one of"
  )
})


testname <- "16: MUST FAIL: basin mode with multiple inputs"
test_that(testname, {

  expect_error(
    api_get_ids(basin_ids = basin_id_schlei, subc_ids = subc_ids_two,
                mode = "basin"),
    regexp = "only one"
  )
})


testname <- "17: MUST FAIL: basin mode with non-numeric basin_ids"
test_that(testname, {

  expect_error(
    api_get_ids(basin_ids = "abc", mode = "basin"),
    regexp = "numeric"
  )
})


testname <- "18: MUST FAIL: upstream mode with no points"
test_that(testname, {

  expect_error(
    api_get_ids(mode = "upstream"),
    regexp = "non-empty data.frame"
  )
})


testname <- "19: MUST FAIL: upstream mode with missing site_id column"
test_that(testname, {

  bad_df <- data.frame(longitude = 9.93, latitude = 54.7)

  expect_error(
    api_get_ids(points = bad_df, mode = "upstream"),
    regexp = "site_id"
  )
})


testname <- "20: MUST FAIL: point mode with missing lon"
test_that(testname, {

  expect_error(
    api_get_ids(lat = 53.5, mode = "point"),
    regexp = "lon.*lat|lat.*lon"
  )
})


testname <- "21: MUST FAIL: point mode with lon out of range"
test_that(testname, {

  expect_error(
    api_get_ids(lon = 200, lat = 53.5, mode = "point"),
    regexp = "-180 to 180"
  )
})
