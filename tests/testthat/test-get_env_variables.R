
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

#############
### Tests ###
#############

test_that("1 test base function", {

  # Prepare:

  # Run:
  tab <- get_file_size_table(file_name = "env90m_presentclimate_paths_file_sizes.txt", tempdir = tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  created_cols <- names(tab)
  expect_true(all(sort(expected_cols) == sort(created_cols)))
  expect_length(tab$file_name, 2204)

})


test_that("2.1 landcover tab", {

  # Prepare:

  # Run:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 73840)
})


test_that("2.2 landcover, all, not sep", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE)

  # Check:
  expect_length(vars, 638)
  some_examples <- c("c100_1992", "c100_1993", "c100_1994", "c100_1995", "c100_1996", "c100_1997")
  expect_true(all(some_examples %in% vars))
})


test_that("2.3 landcover, all, sep", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(separated = TRUE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE)

  # Check:
  expect_length(vars, 2)
  expected_names <- c("base_vars", "years")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_length(vars$base_vars, 22)
  expect_length(vars$years, 29)
})

test_that("2.4 landcover, not all, not sep", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE, years=c(1992, 1994))

  # Check:
  expect_length(vars, 44)
  some_examples <- c("c100_1992", "c100_1994", "c20_1992", "c20_1994", "c30_1992", "c30_1994")
  expect_true(all(some_examples %in% vars))
  some_examples <- c("c100_1993", "c100_1995", "c20_1993", "c20_1995", "c30_1993", "c30_1995")
  expect_false(any(some_examples %in% vars))
})

test_that("2.4.2 landcover, not all, not sep 2", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
    years=c(1992, 1994),
    base_vars=c("c10", "c20", "c30")
  )

  # Check:
  all_expected <- c("c10_1992", "c10_1994", "c20_1992", "c20_1994", "c30_1992", "c30_1994")
  expect_equal(sort(vars), sort(all_expected))
})


test_that("2.5 landcover, not all, sep", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_landcover_variables(separated = TRUE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE, years=c(1992, 1994))

  # Check:
  expect_length(vars, 2)
  expected_names <- c("base_vars", "years")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_length(vars$base_vars, 22)
  expect_length(vars$years, 2)
  expect_equal(vars$years, c(1992, 1994))
})


test_that("2.6 landcover, not all, not sep: not available!", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  expected_error_message <- "Not available: Year(s) 9999, 8888. Please check your spelling and try again!"
  expected_error_message <- "Not available: Year(s) 9999, 8888. Please check your spelling and try again!"
  expect_error(
    get_landcover_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
      years=c(1992, 9999, 8888)
    ),
    expected_error_message, fixed=TRUE
  )
})


test_that("2.6.2 landcover, not all, not sep: not available!", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  expected_error_message <- "Not available: Base var(s) c99. Please check your spelling and try again!"
  expect_error(
    get_landcover_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
      years=c(1992, 1994),
      base_vars=c("c10", "c20", "c99")
    ),
    expected_error_message, fixed=TRUE
  )
})


test_that("2.7 landcover, not all, sep: not available!", {

  # Prepare:
  tab <- get_landcover_variable_table(tempdir=tmpdir)

  # Run:
  expected_error_message <- "Not available: Year(s) 9999, 8888. Please check your spelling and try again!"
  expect_error(
    get_landcover_variables(separated = TRUE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
      years=c(1992, 9999, 8888)
    ),
    expected_error_message, fixed=TRUE
  )
})


test_that("3.1 futureclimate tab", {

  # Prepare:

  # Run:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 39672)
})


test_that("3.2 futureclimate, all, not sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE)

  # Check:
  expect_length(vars, 342)
  some_examples <- c(
    "bio5_2071-2100_mpi-esm1-2-hr_ssp370_V.2.1",
    "bio1_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1",
    "bio9_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1",
    "bio13_2041-2070_ukesm1-0-ll_ssp126_V.2.1")
  expect_true(all(some_examples %in% vars))
})


test_that("3.3 futureclimate, all, sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(separated = TRUE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE)

  # Check:
  expect_length(vars, 5)
  expected_names <- c("base_vars", "time_periods", "scenarios", "models", "versions")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_length(vars$base_vars, 19)
  expect_true(all(c("2041-2070", "2071-2100") %in% vars$time_periods))
  expect_true(all(c("ssp126", "ssp370", "ssp585") %in% vars$scenarios))
  expect_true(all(c("ipsl-cm6a-lr", "mpi-esm1-2-hr", "ukesm1-0-ll") %in% vars$models))
  expect_equal(vars$versions, "V.2.1")
})

test_that("3.4 futureclimate, not all, not sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
    models=c("ipsl-cm6a-lr"), base_vars=c("bio1", "bio10"))

  # Check:
  expect_length(vars, 12)
  all_expected <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio10_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1", "bio10_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2041-2070_ipsl-cm6a-lr_ssp370_V.2.1", "bio10_2041-2070_ipsl-cm6a-lr_ssp370_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1", "bio10_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1", "bio1_2041-2070_ipsl-cm6a-lr_ssp585_V.2.1", "bio10_2041-2070_ipsl-cm6a-lr_ssp585_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1", "bio10_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1")
  expect_equal(vars, all_expected)
})


test_that("3.5 futureclimate, not all, sep", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_future_climate_variables(separated = TRUE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
    models=c("ipsl-cm6a-lr"), base_vars=c("bio1", "bio10"))

  # Check:
  expect_length(vars, 5)
  expected_names <- c("base_vars", "models", "time_periods", "scenarios", "versions")
  expect_equal(sort(names(vars)), sort(expected_names))
  expect_equal(vars$base_vars, c("bio1", "bio10"))
  expect_equal(vars$models, c("ipsl-cm6a-lr"))
  expect_equal(vars$versions, "V.2.1")
  expect_equal(vars$time_periods, c("2041-2070", "2071-2100"))
})

test_that("3.6.1 futureclimate: Model not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  expected_error_message <- "Not available: Model(s) xyz. Please check your spelling and try again!"
  expect_error(
    get_future_climate_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
    models=c("ipsl-cm6a-lr", "xyz"), base_vars=c("bio1", "bio10")),
    expected_error_message, fixed=TRUE
  )
})

test_that("3.6.2 futureclimate: Scenario not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  expected_error_message <- "Not available: Scenario(s) xyz. Please check your spelling and try again!"
  expect_error(
    get_future_climate_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
    scenarios=c("ssp585", "xyz"), base_vars=c("bio1", "bio10")),
    expected_error_message, fixed=TRUE
  )
})


test_that("3.6.3 futureclimate: Version not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  expected_error_message <- "Not available: Version(s) xyz. Please check your spelling and try again!"
  expect_error(
    get_future_climate_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
    versions=c("V.2.1", "xyz"), base_vars=c("bio1", "bio10")),
    expected_error_message, fixed=TRUE
  )
})


test_that("3.6.4 futureclimate: Base var not available", {

  # Prepare:
  tab <- get_future_climate_variable_table(tempdir=tmpdir)

  # Run:
  expected_error_message <- "Not available: Base var(s) bio999. Please check your spelling and try again!"
  expect_error(
    get_future_climate_variables(separated = FALSE, file_size_table = tab, tempdir=tmpdir, quiet=FALSE,
    scenarios=c("ssp585"), base_vars=c("bio1", "bio10", "bio999")),
    expected_error_message, fixed=TRUE
  )
})

test_that("4.1 presentclimate tab", {

  # Prepare:

  # Run:
  tab <- get_present_climate_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 2204)
})

test_that("4.2 presentclimate, all, not sep", {

  # Prepare:
  tab <- get_present_climate_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_present_climate_variables(file_size_table = tab, tempdir=tmpdir, quiet=FALSE)

  # Check:
  all_expected = c("bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17",
                   "bio18", "bio19", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9")
  expect_equal(sort(vars), sort(all_expected))
})

test_that("5.1 hydro90m tab", {

  # Prepare:

  # Run:
  tab <- get_hydro90m_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 5684)
})



test_that("5.2 hydro90m, all, not sep", {

  # Prepare:
  tab <- get_hydrography90m_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_hydrography90m_variables(file_size_table = tab, tempdir=tmpdir, quiet=FALSE)

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


test_that("6.1 soil tab", {

  # Prepare:

  # Run:
  tab <- get_soil_variable_table(tempdir=tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  expect_equal(sort(names(tab)), sort(expected_cols))
  expect_length(tab$file_name, 1856)
})

test_that("6.2 soil, all, not sep", {

  # Prepare:
  tab <- get_soil_variable_table(tempdir=tmpdir)

  # Run:
  vars <- get_soil_variables(file_size_table = tab, tempdir=tmpdir, quiet=FALSE)

  # Check:
  all_expected = c(
    "acdwrb", "awcts", "bdricm", "bdrlog", "bldfie", "cecsol", "clyppt", "crfvol",
    "histpr", "orcdrc", "phihox", "slgwrb", "sltppt", "sndppt", "texmht", "wwp")
  expect_equal(sort(vars), sort(all_expected))
})

