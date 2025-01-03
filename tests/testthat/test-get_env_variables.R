
###############################################
### Testing the get_env_variables functions ###
###############################################

# Cases to be covered:
# * ...
# * ...

#########################
### Some preparations ###
#########################

tmpdir <- tempdir()
tmpdir <- "/tmp"
print(paste0('Tempdir: ', tmpdir))

#Sys.setenv(SKIP_SLOW = "FALSE")
#Sys.setenv(SKIP_SLOW = "TRUE")

SKIP_SLOW <- Sys.getenv("SKIP_SLOW") == "TRUE" # empty string / FALSE if not set
if (SKIP_SLOW) {
    print('SKIP_SLOW is set to TRUE, skipping slow tests. If you want to run them, set SKIP_SLOW to FALSE')
} else {
    print('SKIP_SLOW is set to FALSE, not skipping slow tests. If you want to skip them, set SKIP_SLOW to TRUE')
}


#############
### Tests ###
#############

test_that("1 test base function", {

  # Prepare:

  # Run:
  tab <- get_file_size_table(
    file_name = "env90m_presentclimate_paths_file_sizes.txt",
    tempdir = tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  created_cols <- names(tab)
  expect_true(all(sort(expected_cols) == sort(created_cols)))
  expect_length(tab$file_name, 2204)
})

test_that("2.1 landcover table getter", {

  # Prepare:

  # Run:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 73840)
})

test_that("2.2.1 landcover, all vars, not sep, no tiles", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(
    separated = FALSE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  expect_length(vars, 638)
  some_examples <- c("c100_1992", "c100_1993", "c100_1994", "c100_1995", "c100_1996", "c100_1997")
  expect_true(all(some_examples %in% vars))
})

test_that("2.2.2 landcover, all vars, download size as message (slow)", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)
  tiles <- c("h02v02", "h04v02")
  expected_message <- "Download size: 638 variables, 2 tiles: 19.295762175 GB (19295762175 bytes)."

  # Run and check:
  skip_if(SKIP_SLOW, 'Slow, so we skip it this time...')
  expect_message(
    vars <- get_landcover_variables(
        separated = FALSE,
        file_size_table = tab,
        tempdir = tmpdir,
        quiet = TRUE,
        tile_ids = tiles),
    expected_message, fixed=TRUE)

  # Check:
  expect_length(vars, 638)
  some_examples <- c("c100_1992", "c100_1993", "c100_1994", "c100_1995", "c100_1996", "c100_1997")
  expect_true(all(some_examples %in% vars))
})

test_that("2.3.1 landcover, all vars, sep, no tiles", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(
    separated = TRUE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  expect_length(vars, 5)
  expected_names <- c("base_vars", "years", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_length(vars$base_vars, 22)
  expect_length(vars$years, 29)
  expect_equal(vars$download_bytes, NA)
})

test_that("2.3.2 landcover, all vars, download size as number (sep only) (slow)", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  skip_if(SKIP_SLOW, 'Slow, so we skip it this time...')
  vars <- get_landcover_variables(
    separated = TRUE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE,
    tile_ids = c("h02v02", "h04v02")
  )

  # Check:
  expect_length(vars, 5)
  expected_names <- c("base_vars", "years", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_equal(vars$download_bytes, 19295762175)
})

test_that("2.4.1 landcover, not all vars, not sep", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(
    separated = FALSE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE,
    years=c(1992, 1994)
  )

  # Check:
  expect_length(vars, 44)
  some_examples <- c("c100_1992", "c100_1994", "c20_1992", "c20_1994", "c30_1992", "c30_1994")
  expect_true(all(some_examples %in% vars))
  some_examples <- c("c100_1993", "c100_1995", "c20_1993", "c20_1995", "c30_1993", "c30_1995")
  expect_false(any(some_examples %in% vars))
})

test_that("2.4.2 landcover, not all vars, not sep 2", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(
    separated = FALSE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE,
    years=c(1992, 1994),
    base_vars=c("c10", "c20", "c30")
  )

  # Check:
  all_expected <- c("c10_1992", "c10_1994", "c20_1992", "c20_1994", "c30_1992", "c30_1994")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("2.4.3 landcover, not all vars, download size as message", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)
  expected_message <- "Download size: 44 variables, 2 tiles: 1.332422613 GB (1332422613 bytes)."
  
  # Run:
  expect_message(
    vars <- get_landcover_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=TRUE,
      years=c(1992, 1994),
      tile_ids = c("h02v02", "h04v02")),
    expected_message, fixed=TRUE)

  # Check:
  expect_length(vars, 44)
  some_examples <- c("c100_1992", "c100_1994", "c20_1992", "c20_1994", "c30_1992", "c30_1994")
  expect_true(all(some_examples %in% vars))
  some_examples <- c("c100_1993", "c100_1995", "c20_1993", "c20_1995", "c30_1993", "c30_1995")
  expect_false(any(some_examples %in% vars))
})

test_that("2.5.1 landcover, not all vars, sep", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(
    separated = TRUE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE,
    years=c(1992, 1994))

  # Check:
  expect_length(vars, 5)
  expected_names <- c("base_vars", "years", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_length(vars$base_vars, 22)
  expect_length(vars$years, 2)
  expect_equal(vars$years, c(1992, 1994))
})

test_that("2.5.2 landcover, not all vars, download size as number (sep only)", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(
    separated = TRUE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE,
    tile_ids = c("h02v02", "h04v02"),
    years=c(1992),
    base_vars=c("c10")
  )

  # Check:
  expect_length(vars, 5)
  expected_names <- c("base_vars", "years", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_equal(vars$base_vars, "c10")
  expect_equal(vars$years, 1992)
  expect_equal(vars$download_bytes, 27700300)
})

test_that("2.6.1 landcover, not all vars, not sep: Years not available!", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Year(s) 9999, 8888. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_landcover_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=FALSE,
      years=c(1992, 9999, 8888)),
    expected_error_message, fixed=TRUE)
})

test_that("2.6.2 landcover, not all vars, not sep: Base vars not available!", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Base var(s) c99. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_landcover_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=FALSE,
      years=c(1992, 1994),
      base_vars=c("c10", "c20", "c99")),
    expected_error_message, fixed=TRUE)
})

test_that("2.6.3 landcover, not all vars, sep: Years not available!", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Year(s) 9999, 8888. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_landcover_variables(
      separated = TRUE,
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=FALSE,
      years=c(1992, 9999, 8888)
    ),
    expected_error_message, fixed=TRUE)
})

test_that("3.1 futureclimate table getter", {

  # Prepare:

  # Run:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 39672)
})

test_that("3.2.1 futureclimate, all vars, not sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(
    separated = FALSE,
    file_size_table = tab,
    tempdir = tmpdir,
    quiet = FALSE)

  # Check:
  expect_length(vars, 342)
  some_examples <- c(
    "bio5_2071-2100_mpi-esm1-2-hr_ssp370_V.2.1",
    "bio1_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1",
    "bio9_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1",
    "bio13_2041-2070_ukesm1-0-ll_ssp126_V.2.1")
  expect_true(all(some_examples %in% vars))
})

test_that("3.2.2 futureclimate, all vars, download size as message (slow)", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)
  expected_message <- "Download size: 342 variables, 2 tiles: 49.746345249 GB (49746345249 bytes)."
  
  # Run:
  skip_if(SKIP_SLOW, 'Slow, so we skip it this time...')
  expect_message(
    vars <- get_future_climate_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir = tmpdir,
      quiet = TRUE,
      tile_ids = c("h02v02", "h04v02")),
    expected_message, fixed=TRUE)

  # Check:
  expect_length(vars, 342)
  some_examples <- c(
    "bio5_2071-2100_mpi-esm1-2-hr_ssp370_V.2.1",
    "bio1_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1",
    "bio9_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1",
    "bio13_2041-2070_ukesm1-0-ll_ssp126_V.2.1")
  expect_true(all(some_examples %in% vars))
})

test_that("3.3.1 futureclimate, all vars, sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(
    separated = TRUE,
    file_size_table = tab,
    tempdir = tmpdir,
    quiet = FALSE)

  # Check:
  expect_length(vars, 8)
  expected_names <- c("base_vars", "time_periods", "scenarios", "models", "versions", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_length(vars$base_vars, 19)
  expect_true(all(c("2041-2070", "2071-2100") %in% vars$time_periods))
  expect_true(all(c("ssp126", "ssp370", "ssp585") %in% vars$scenarios))
  expect_true(all(c("ipsl-cm6a-lr", "mpi-esm1-2-hr", "ukesm1-0-ll") %in% vars$models))
  expect_equal(vars$versions, "V.2.1")
})

test_that("3.3.2 futureclimate, all vars, download size as number (sep only) (slow)", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  #skip_if(SKIP_SLOW, 'Slow, so we skip it this time...')
  vars <- get_future_climate_variables(
    tile_ids = c("h02v02", "h04v02"),
    separated = TRUE,
    file_size_table = tab,
    tempdir = tmpdir,
    quiet = FALSE
  )

  # Check:
  expect_length(vars, 8)
  expected_names <- c("base_vars", "time_periods", "scenarios", "models", "versions", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_length(vars$base_vars, 19)
  expect_true(all(c("2041-2070", "2071-2100") %in% vars$time_periods))
  expect_true(all(c("ssp126", "ssp370", "ssp585") %in% vars$scenarios))
  expect_true(all(c("ipsl-cm6a-lr", "mpi-esm1-2-hr", "ukesm1-0-ll") %in% vars$models))
  expect_equal(vars$versions, "V.2.1")
  expect_equal(vars$download_bytes, 49746345249)
})

test_that("3.4.1 futureclimate, not all vars, not sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(
    separated = FALSE,
    file_size_table = tab,
    tempdir = tmpdir,
    quiet = FALSE,
    models = c("ipsl-cm6a-lr"),
    base_vars=c("bio1", "bio10"))

  # Check:
  expect_length(vars, 12)
  all_expected <- c(
    "bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio10_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1",
    "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1", "bio10_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1",
    "bio1_2041-2070_ipsl-cm6a-lr_ssp370_V.2.1", "bio10_2041-2070_ipsl-cm6a-lr_ssp370_V.2.1",
    "bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1", "bio10_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1",
    "bio1_2041-2070_ipsl-cm6a-lr_ssp585_V.2.1", "bio10_2041-2070_ipsl-cm6a-lr_ssp585_V.2.1",
    "bio1_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1", "bio10_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1")
  expect_equal(vars, all_expected)
})

test_that("3.4.2 futureclimate, not all vars, download size as message", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)
  expected_message <- "Download size: 4 variables, 2 tiles: 0.513805694 GB (513805694 bytes)."
  
  # Run:
  expect_message(
    vars <- get_future_climate_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir = tmpdir,
      quiet = TRUE,
      tile_ids = c("h02v02", "h04v02"),
      base_vars = c("bio10", "bio1"),
      models = c("ipsl-cm6a-lr"),
      scenarios = c("ssp370")),
    expected_message, fixed=TRUE)

  # Check:
  expect_length(vars, 4)
  all_expected <- c(
    "bio1_2041-2070_ipsl-cm6a-lr_ssp370_V.2.1", "bio10_2041-2070_ipsl-cm6a-lr_ssp370_V.2.1",
    "bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1", "bio10_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1")
  expect_equal(vars, all_expected)
})

test_that("3.5.1 futureclimate, not all vars, sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(
    separated = TRUE,
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE,
    models=c("ipsl-cm6a-lr"),
    base_vars=c("bio1", "bio10")
  )

  # Check:
  expect_length(vars, 8)
  expected_names <- c("base_vars", "time_periods", "scenarios", "models", "versions", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_equal(vars$base_vars, c("bio1", "bio10"))
  expect_equal(vars$models, c("ipsl-cm6a-lr"))
  expect_equal(vars$versions, "V.2.1")
  expect_equal(vars$time_periods, c("2041-2070", "2071-2100"))
})

test_that("3.5.2 futureclimate, not all vars, download size as number (sep only)", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(
    separated = TRUE,
    file_size_table = tab,
    tempdir = tmpdir,
    quiet = FALSE,
    models = c("ipsl-cm6a-lr"),
    base_vars = c("bio1", "bio10"),
    tile_ids = c("h02v02", "h04v02")
  )

  # Check:
  expect_length(vars, 8)
  expected_names <- c("base_vars", "time_periods", "scenarios", "models", "versions", "download_bytes", "comment", "complete_variable_names")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_equal(vars$base_vars, c("bio1", "bio10"))
  expect_equal(vars$models, c("ipsl-cm6a-lr"))
  expect_equal(vars$versions, "V.2.1")
  expect_equal(vars$time_periods, c("2041-2070", "2071-2100"))
  expect_equal(vars$download_bytes, 1540975863)
})

test_that("3.6.1 futureclimate: Model not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Model(s) xyz. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_future_climate_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir = tmpdir,
      quiet = FALSE,
      models=c("ipsl-cm6a-lr", "xyz"),
      base_vars=c("bio1", "bio10")),
    expected_error_message, fixed=TRUE
  )
})

test_that("3.6.2 futureclimate: Scenario not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Scenario(s) xyz. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_future_climate_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=FALSE,
      scenarios=c("ssp585", "xyz"),
      base_vars=c("bio1", "bio10")),
    expected_error_message, fixed=TRUE)
})

test_that("3.6.3 futureclimate: Version not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Version(s) xyz. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_future_climate_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=FALSE,
      versions=c("V.2.1", "xyz"),
      base_vars=c("bio1", "bio10")),
    expected_error_message, fixed=TRUE)
})

test_that("3.6.4 futureclimate: Base var not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Base var(s) bio999. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_future_climate_variables(
      separated = FALSE,
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=FALSE,
      scenarios=c("ssp585"),
      base_vars=c("bio1", "bio10", "bio999")),
    expected_error_message, fixed=TRUE)
})

test_that("4.1 presentclimate table getter", {

  # Prepare:

  # Run:
  tab <- get_present_climate_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 2204)
})

test_that("4.2 presentclimate", {

  # Prepare:
  tab <- get_present_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_present_climate_variables(
    file_size_table = tab,
    tempdir = tmpdir,
    quiet = FALSE)

  # Check:
  all_expected = c("bio1",  "bio10", "bio11", "bio12", "bio13",
                   "bio14", "bio15", "bio16", "bio17", "bio18",
                   "bio19", "bio2",  "bio3",  "bio4",  "bio5",
                   "bio6",  "bio7",  "bio8",  "bio9")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("4.3.1 presentclimate: download size as message", {

  # Prepare:
  tab <- get_present_climate_variable_table(tempdir=tmpdir)
  expected_message <- "Download size: 19 variables, 2 tiles: 3.457746309 GB (3457746309 bytes)."

  # Run:
  expect_message(
    vars <- get_present_climate_variables(
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=TRUE,
      tile_ids=c("h02v02", "h04v02")),
    expected_message, fixed=TRUE)

  # Check:
  all_expected = c("bio1",  "bio10", "bio11", "bio12", "bio13",
                   "bio14", "bio15", "bio16", "bio17", "bio18",
                   "bio19", "bio2",  "bio3",  "bio4",  "bio5",
                   "bio6",  "bio7",  "bio8",  "bio9")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("4.2.2 presentclimate: download size as number (sep only)", {

  # Prepare:
  tab <- get_present_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_present_climate_variables(
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=TRUE,
    tile_ids=c("h02v02", "h04v02"),
    separated = TRUE)
  
  # Check:
  all_expected = c("bio1",  "bio10", "bio11", "bio12", "bio13",
                   "bio14", "bio15", "bio16", "bio17", "bio18",
                   "bio19", "bio2",  "bio3",  "bio4",  "bio5",
                   "bio6",  "bio7",  "bio8",  "bio9")
  expect_equal(sort(vars$complete_variable_names), sort(all_expected))
  expect_equal(vars$download_bytes, 3457746309)
})

test_that("5.1 hydro90m table getter", {

  # Prepare:

  # Run:
  tab <- get_hydro90m_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 5684)
})

test_that("5.2 hydro90m", {

  # Prepare:
  tab <- get_hydrography90m_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_hydrography90m_variables(
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  all_expected = c(
    "channel_curv_cel", "channel_dist_dw_seg", "channel_dist_up_cel",
    "channel_dist_up_seg", "channel_elv_dw_cel", "channel_elv_dw_seg",
    "channel_elv_up_cel", "channel_elv_up_seg", "channel_grad_dw_seg",
    "channel_grad_up_cel", "channel_grad_up_seg", "connections", "cti",
    "cum_length", "elev_drop", "flow", "flow_accum", "flow_pos", "flow1k",
    "gradient", "length", "out_dist", "out_drop", "outlet_diff_dw_basin",
    "outlet_diff_dw_scatch", "outlet_dist_dw_basin", "outlet_dist_dw_scatch",
    "outlet_elev", "sinusoid", "slope_curv_max_dw_cel", "slope_curv_min_dw_cel",
    "slope_elv_dw_cel", "slope_grad_dw_cel", "source_elev", "spi", "sti",
    "stream_diff_dw_near", "stream_diff_up_farth", "stream_diff_up_near",
    "stream_dist_dw_near", "stream_dist_proximity", "stream_dist_up_farth",
    "stream_dist_up_near", "stream_hack", "stream_horton", "stream_shreve",
    "stream_strahler", "stream_topo", "stright")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("5.3 hydro90m: download size as message", {

  # Prepare:
  tab <- get_hydrography90m_variable_table(tempdir=tmpdir)

  # Run:
  expected_message <- "Download size: 49 variables, 2 tiles: 12.629606053 GB (12629606053 bytes)."
  expect_message(
    vars <- get_hydrography90m_variables(
        file_size_table = tab,
        tempdir=tmpdir,
        quiet=TRUE,
        tile_ids= c("h02v02", "h04v02")),
    expected_message, fixed=TRUE
  )

  # Check:
  all_expected = c(
    "channel_curv_cel", "channel_dist_dw_seg", "channel_dist_up_cel",
    "channel_dist_up_seg", "channel_elv_dw_cel", "channel_elv_dw_seg",
    "channel_elv_up_cel", "channel_elv_up_seg", "channel_grad_dw_seg",
    "channel_grad_up_cel", "channel_grad_up_seg", "connections", "cti",
    "cum_length", "elev_drop", "flow", "flow_accum", "flow_pos", "flow1k",
    "gradient", "length", "out_dist", "out_drop", "outlet_diff_dw_basin",
    "outlet_diff_dw_scatch", "outlet_dist_dw_basin", "outlet_dist_dw_scatch",
    "outlet_elev", "sinusoid", "slope_curv_max_dw_cel", "slope_curv_min_dw_cel",
    "slope_elv_dw_cel", "slope_grad_dw_cel", "source_elev", "spi", "sti",
    "stream_diff_dw_near", "stream_diff_up_farth", "stream_diff_up_near",
    "stream_dist_dw_near", "stream_dist_proximity", "stream_dist_up_farth",
    "stream_dist_up_near", "stream_hack", "stream_horton", "stream_shreve",
    "stream_strahler", "stream_topo", "stright")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("5.4 hydro90m: download size as number (sep only)", {

  # Prepare:
  tab <- get_hydrography90m_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_hydrography90m_variables(
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=TRUE,
    tile_ids= c("h02v02", "h04v02"),
    separated=TRUE)

  # Check:
  all_expected = c(
    "channel_curv_cel", "channel_dist_dw_seg", "channel_dist_up_cel",
    "channel_dist_up_seg", "channel_elv_dw_cel", "channel_elv_dw_seg",
    "channel_elv_up_cel", "channel_elv_up_seg", "channel_grad_dw_seg",
    "channel_grad_up_cel", "channel_grad_up_seg", "connections", "cti",
    "cum_length", "elev_drop", "flow", "flow_accum", "flow_pos", "flow1k",
    "gradient", "length", "out_dist", "out_drop", "outlet_diff_dw_basin",
    "outlet_diff_dw_scatch", "outlet_dist_dw_basin", "outlet_dist_dw_scatch",
    "outlet_elev", "sinusoid", "slope_curv_max_dw_cel", "slope_curv_min_dw_cel",
    "slope_elv_dw_cel", "slope_grad_dw_cel", "source_elev", "spi", "sti",
    "stream_diff_dw_near", "stream_diff_up_farth", "stream_diff_up_near",
    "stream_dist_dw_near", "stream_dist_proximity", "stream_dist_up_farth",
    "stream_dist_up_near", "stream_hack", "stream_horton", "stream_shreve",
    "stream_strahler", "stream_topo", "stright")
  expect_equal(sort(vars$complete_variable_names), sort(all_expected))
  expect_equal(vars$download_bytes, 12629606053)
})

test_that("6.1 soil table getter", {

  # Prepare:

  # Run:
  tab <- get_soil_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 1856)
})

test_that("6.2 soil", {

  # Prepare:
  tab <- get_soil_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_soil_variables(
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  all_expected = c(
    "acdwrb", "awcts", "bdricm", "bdrlog", "bldfie", "cecsol", "clyppt", "crfvol",
    "histpr", "orcdrc", "phihox", "slgwrb", "sltppt", "sndppt", "texmht", "wwp")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("6.3 soil: download size as message", {

  # Prepare:
  tab <- get_soil_variable_table(tempdir=tmpdir)
  expected_message <- "Download size: 16 variables, 2 tiles: 2.469417656 GB (2469417656 bytes)."

  # Run:
  expect_message(
    vars <- get_soil_variables(
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=TRUE,
      tile_ids=c("h02v02", "h04v02")),
    expected_message, fixed=TRUE)

  # Check:
  all_expected = c(
    "acdwrb", "awcts", "bdricm", "bdrlog", "bldfie", "cecsol", "clyppt", "crfvol",
    "histpr", "orcdrc", "phihox", "slgwrb", "sltppt", "sndppt", "texmht", "wwp")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("6.4 soil: download size as number (sep only)", {

  # Prepare:
  tab <- get_soil_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_soil_variables(
    file_size_table = tab,
    tempdir=tmpdir,
    quiet=TRUE,
    tile_ids=c("h02v02", "h04v02"),
    separated=TRUE)
  
  # Check:
  all_expected = c(
    "acdwrb", "awcts", "bdricm", "bdrlog", "bldfie", "cecsol", "clyppt", "crfvol",
    "histpr", "orcdrc", "phihox", "slgwrb", "sltppt", "sndppt", "texmht", "wwp")
  expect_equal(sort(vars$complete_variable_names), sort(all_expected))
  expect_equal(vars$download_bytes, 2469417656)
})

test_that("6.5 soil: download size failure", {

  # Prepare:
  tab <- get_soil_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Tile id(s) h99v99. Please check your spelling and try again!"

  # Run:
  expect_error(
    get_soil_variables(
      file_size_table = tab,
      tempdir=tmpdir,
      quiet=FALSE,
      tile_ids=c("h99v99")),
    expected_error_message, fixed=TRUE)
})

test_that("7.1: download size", {

  # Prepare:
  tab <- get_soil_variable_table(tempdir=tmpdir)

  # Run:
  bytes <- download_size(
    c("h02v02", "h04v02"),
    c("acdwrb", "awcts", "bdricm", "bdrlog"),
    tab,
    quiet = FALSE,
    ignore_missing = FALSE)

  # Check:
  expect_equal(bytes, 575879821)
})

test_that("7.2: download size: failure", {

  # Prepare:
  tab <- get_soil_variable_table(tempdir=tmpdir)
  expected_error_message <- "Not available: Tile id(s) h99v99. Please check your spelling and try again!"

  # Run:
  expect_error(
    download_size(
    c("h02v02", "h04v02", "h99v99"),
    c("acdwrb", "awcts", "bdricm", "bdrlog"),
    tab,
    quiet = FALSE,
    ignore_missing = FALSE),
    expected_error_message, fixed=TRUE
  )
})
