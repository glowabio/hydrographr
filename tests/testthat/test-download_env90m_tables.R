
####################################################
### Testing the download_env90m_tables functions ###
####################################################


tests_quiet=TRUE

if (!(tests_quiet)) print("_______________________________")
if (!(tests_quiet)) print("Testing: download_env90m_tables")


#########################
### Some preparations ###
#########################

# Where to store and download files:
if (! exists("tmpdir")){
  tmpdir <- tempdir()
}
if (! exists("download_dir")){
  download_dir <- tempdir()
}

# Get which tests to skip:
R_SKIP_HUGE_DOWNLOAD <- !(Sys.getenv("R_SKIP_HUGE_DOWNLOAD") == "FALSE")
R_SKIP_DOWNLOAD <- Sys.getenv("R_SKIP_DOWNLOAD") == "TRUE"

#############
### Tests ###
#############

testname = "1.1 helpers: table getter"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  tab <- get_file_size_table(
    file_name="env90m_observedclimate_paths_file_sizes.txt",
    quiet=tests_quiet)

  # Check whether the specified table was loaded
  # and has the expected columns, number of rows, ...
  expect_equal(sort(names(tab)), c("file_id", "file_name", "file_path", "file_size"))
  expect_length(tab$file_name, 2204)
})


testname = "1.2.1: helpers: compute download size"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  file_size_table <- get_file_size_table(
    file_name="env90m_landcover_paths_file_sizes.txt",
    quiet=tests_quiet)
  
  # Run:
  bytes <- compute_download_size(
    c("c100_1992", "c100_1993"),
    c("h10v04", "h00v04"),
    file_size_table,
    quiet=FALSE,
    ignore_missing=FALSE
  )

  # Check whether the correct download size is computed:
  expect_equal(bytes, 788)
})


testname = "1.2.2: helpers: compute download size: failure, wrong tile"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Tile id(s) h99v99. Please check your spelling and try again!"
  file_size_table <- get_file_size_table(
    file_name="env90m_landcover_paths_file_sizes.txt",
    quiet=tests_quiet)

  # Run and check whether error happens:
  expect_error(
    bytes <- compute_download_size(
      c("c100_1992", "c100_1993"),
      c("h02v02", "h04v02", "h99v99"),
      file_size_table,
      quiet=FALSE,
      ignore_missing=FALSE
    ),
    expected_error_message, fixed=TRUE
  )
})


testname = "1.3.1: helpers: download zips (not delete)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  file_size_table <- get_file_size_table(
    file_name="env90m_landcover_paths_file_sizes.txt",
    quiet=tests_quiet)

  # Run:
  skip_if_offline("public.igb-berlin.de") # downloads 8.5 KB (or only 316 bytes), no need to skip
  result <- do_env90m_download(
    c("c100_1992", "c100_1993"),
    c("h10v04"),
    file_size_table,
    download_dir=download_dir,
    file_format="zip",
    quiet=FALSE,
    delete_zips=FALSE)

  # Check whether zips exist:
  expected_zips <- c(
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1992_h10v04.zip"),
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1993_h10v04.zip")
  )
  # Check whether zips are reported:
  expect_equal(result$downloaded, expected_zips)
  # Check whether zips actually exist:
  for (zipfilename in expected_zips) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "1.3.2: helpers: download zip, unzip to txt (not delete)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  file_size_table <- get_file_size_table(
    file_name="env90m_landcover_paths_file_sizes.txt",
    quiet=tests_quiet)

  # Run:
  skip_if_offline("public.igb-berlin.de")
  result <- do_env90m_download(
    c("c100_1992", "c100_1993"),
    c("h10v04"),
    file_size_table,
    download_dir=download_dir,
    file_format="txt",
    quiet=FALSE,
    delete_zips=FALSE)

  # Check whether zips and txts exist:
  expected_zips <- c(
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1992_h10v04.zip"),
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1993_h10v04.zip")
  )
  expected_unzipped <- c(
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1992_h10v04.txt"),
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1993_h10v04.txt")
  )
  # Check whether zips are reported:
  expect_equal(result$downloaded, expected_zips)
  # Check whether zips actually exist:
  for (zipfilename in expected_zips) {
    expect_true(file.exists(zipfilename))
  }
  # Check whether unzipped is reported:
  expect_equal(result$unzipped, expected_unzipped)
  # Check whether unzipped files actually exist:
  for (zipfilename in expected_zips) {
    expect_true(file.exists(gsub('zip', 'txt', zipfilename)))
  }
})


testname = "1.3.3: helpers: download zip, unzip to txt (do delete)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  file_size_table <- get_file_size_table(
    file_name="env90m_landcover_paths_file_sizes.txt",
    quiet=tests_quiet)

  # Run:
  skip_if_offline("public.igb-berlin.de")
  result <- do_env90m_download(
    c("c100_1992", "c100_1993"),
    c("h10v04"),
    file_size_table,
    download_dir=download_dir,
    file_format="txt",
    quiet=FALSE,
    delete_zips=TRUE)

  # Check whether txts exist and zips don't:
  expected_zips <- c(
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1992_h10v04.zip"),
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1993_h10v04.zip")
  )
  expected_unzipped <- c(
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1992_h10v04.txt"),
    paste0(download_dir,"/esa_cci_landcover_v2_1_1/c100/c100_1993_h10v04.txt")
  )
  # Check whether zips and their deletion are reported:
  expect_equal(result$downloaded, expected_zips)
  expect_equal(result$deleted, expected_zips)
  # Check whether zips actually were deleted:
  for (zipfilename in expected_zips) {
    expect_false(file.exists(zipfilename))
  }
  # Check whether unzipped is reported:
  expect_equal(result$unzipped, expected_unzipped)
  # Check whether unzipped files actually exist:
  for (zipfilename in expected_zips) {
    expect_true(file.exists(gsub('zip', 'txt', zipfilename)))
  }
})


testname = "2.1 landcover: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run three times, each should yield the same result!
  vars <- download_landcover_tables(
    quiet=FALSE)

  vars2 <- download_landcover_tables(
    subset="ALL",
    quiet=FALSE)

  vars3 <- download_landcover_tables(
    base_vars="ALL",
    years="ALL",
    quiet=FALSE)

  # Check that both ways of requesting all variables return the same results,
  # and that the number of variable names, base vars, years are correct.
  expect_equal(vars, vars2)
  expect_equal(vars, vars3)
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_length(vars$base_vars, 22)
  expect_length(vars$years, 29)
  examples <- c("c100_1992", "c100_1993", "c100_1994", "c100_1995", "c100_1996", "c100_1997")
  expect_true(all(examples %in% vars$variable_names))
})


testname = "2.2.1 landcover: show variable names (of a subset, specified: subset)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("c20_1992", "c20_1994", "c30_1992", "c30_1994")

  # Run:
  vars <- download_landcover_tables(
    quiet=FALSE,
    subset=mysubset)

  # Check that the correct variable names are listed:
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
})


testname = "2.2.2 landcover: show variable names (of a subset, specified: base_vars and years)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mybasevars <- c("c20", "c30")
  myyears <- c(1992, 1994)
  
  # Run:
  vars <- download_landcover_tables(
    quiet=FALSE,
    base_vars=mybasevars,
    years=myyears)

  # Check that the correct variables names are listed:
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$variable_names), sort(c("c20_1992", "c20_1994", "c30_1992", "c30_1994")))
  expect_equal(sort(vars$base_vars), sort(mybasevars))
  expect_equal(sort(vars$years), sort(myyears))
})


testname = "2.2.3 landcover: show variable names (test passing years=ALL)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_landcover_tables(
    quiet=FALSE,
    base_vars=c("c20", "c30"),
    years="ALL")

  # Check that the correct variable names, years, and base_vars are listed.
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  all_expected = c(
    "c20_1992", "c20_1993", "c20_1994", "c20_1995", "c20_1996", "c20_1997", "c20_1998", "c20_1999",
    "c20_2000", "c20_2001", "c20_2002", "c20_2003", "c20_2004", "c20_2005", "c20_2006", "c20_2007",
    "c20_2008", "c20_2009", "c20_2010", "c20_2011", "c20_2012", "c20_2013", "c20_2014", "c20_2015",
    "c20_2016", "c20_2017", "c20_2018", "c20_2019", "c20_2020", "c30_1992", "c30_1993", "c30_1994",
    "c30_1995", "c30_1996", "c30_1997", "c30_1998", "c30_1999", "c30_2000", "c30_2001", "c30_2002",
    "c30_2003", "c30_2004", "c30_2005", "c30_2006", "c30_2007", "c30_2008", "c30_2009", "c30_2010",
    "c30_2011", "c30_2012", "c30_2013", "c30_2014", "c30_2015", "c30_2016", "c30_2017", "c30_2018",
    "c30_2019", "c30_2020")
  expect_equal(sort(vars$variable_names), sort(all_expected))
  expect_equal(sort(vars$base_vars), c("c20", "c30"))
  all_years = c("1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
                "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
  expect_equal(sort(vars$years), sort(all_years))
})


testname = "2.3.1 landcover: show download_size (specified: subset / various components)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02", "h04v02")
  
  # Run:
  vars <- download_landcover_tables(
    quiet=FALSE,
    tile_ids=mytiles,
    subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"))

  # Run (same subset, specified using separate base_vars and years)
  vars2 <- download_landcover_tables(
    quiet=FALSE,
    tile_ids=mytiles,
    base_vars=c("c20", "c30"),
    years=c(1992, 1994))

  # Check whether the correct tiles and download size are listed:
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(vars2$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$tile_ids), sort(mytiles))
  expect_equal(sort(vars2$tile_ids), sort(mytiles))
  expect_equal(vars$download_bytes, 107945857)
  expect_equal(vars2$download_bytes, 107945857)
})


testname = "2.3.2 landcover: show download size (test passing subset=ALL)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  # TODO: This is quite slow, maybe add a skip?
  vars <- download_landcover_tables(
    quiet=FALSE,
    subset="ALL",
    tile_ids=c("h02v02"))

  # Check whether the correct number of variable names and the correct download size are returned.
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(length(vars$variable_names), 638)
  expect_equal(vars$download_bytes, 5900197812)
})


testname = "2.3.3 landcover: show download size (test passing tile_ids=ALL)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_landcover_tables(
    quiet=FALSE,
    subset=c("c20_1992"),
    tile_ids="ALL")

  # Check whether the correct number of tiles and the correct download size are returned.
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(length(vars$tile_ids), 116)
  expect_equal(vars$download_bytes, 1752891843)
})


testname = "2.4.1 landcover: download subset"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 107.9 MB, so we skip it.')
  vars <- download_landcover_tables(
    subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"),
    tile_ids=c("h02v02", "h04v02"),
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether download size is listed correctly,
  # and whether the files are actually downloaded.
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(vars$download_bytes, 107945857)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "2.4.2 landcover: download all variables (for one tile)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if(R_SKIP_HUGE_DOWNLOAD, 'R_SKIP_HUGE_DOWNLOAD: This test downloads 5.9 GB, so we skip it.')
  # Note: Test 2.3.2 (computing download size of subset=ALL) tests quite similar behaviour,
  # just without the actual download in the end.
  vars <- download_landcover_tables(
    subset="ALL",
    tile_ids=c("h02v02"),
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether download size is listed correctly,
  # and whether the files are actually downloaded.
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(vars$download_bytes, 5900197812)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "2.4.3 landcover: download all tiles (for one variables)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if(R_SKIP_HUGE_DOWNLOAD, 'R_SKIP_HUGE_DOWNLOAD: This test downloads 1.7 GB, so we skip it.')
  # Note: Test 2.3.3 (computing download size of tile_ids=ALL) tests quite similar behaviour,
  # just without the actual download in the end.
  vars <- download_landcover_tables(
    subset=c("c20_1992"),
    tile_ids="ALL",
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether download size is listed correctly,
  # and whether the files are actually downloaded.
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(vars$download_bytes, 1752891843)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "2.5.1 landcover: failure: year not available"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Year(s) xyz. Please check your spelling and try again!"

  # Run:
  expect_error(
    vars <- download_landcover_tables(
      base_vars=c("c20", "c30"),
      years=c(1992, "xyz"),
      tile_ids=c("h02v02", "h04v02"),
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})

testname = "2.5.2 landcover: warning: no subset passed"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "You did not provide any subset. If you are sure that you want ALL variables, please specify subset='ALL'!"

  # Run:
  expect_warning(
    vars <- download_landcover_tables(
      tile_ids=c("h02v02", "h04v02"),
      download=TRUE,
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})


testname = "3.1 projectedclimate: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run three times, each should yield the same result!
  vars <- download_projected_climate_tables(
    quiet=FALSE)

  vars2 <- download_projected_climate_tables(
    quiet=FALSE,
    subset="ALL")

  vars3 <- download_projected_climate_tables(
    quiet=FALSE,
    time_periods="ALL",
    scenarios="ALL",
    models="ALL",
    base_vars="ALL",
    versions="ALL")

  # Check whether all base variables, models, scenarios etc. are listed,
  # and whether the correct number of variable names is listed.
  expect_equal(vars, vars2)
  expect_equal(vars, vars3)
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  all_base_vars <- c(
    "bio1",  "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18",
    "bio19", "bio2",  "bio3",  "bio4",  "bio5",  "bio6",  "bio7",  "bio8",  "bio9")
  expect_equal(sort(vars$base_vars), all_base_vars)
  expect_equal(sort(vars$models), c("ipsl-cm6a-lr", "mpi-esm1-2-hr", "ukesm1-0-ll"))
  expect_equal(sort(vars$scenarios), c("ssp126", "ssp370", "ssp585"))
  expect_equal(sort(vars$time_periods), c("2041-2070", "2071-2100"))
  expect_equal(sort(vars$versions), c("V.2.1"))
  expect_length(vars$variable_names, 342)
  some_full_variables <- c(
    "bio5_2071-2100_mpi-esm1-2-hr_ssp370_V.2.1",
    "bio1_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1",
    "bio9_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1",
    "bio13_2041-2070_ukesm1-0-ll_ssp126_V.2.1")
  expect_true(all(some_full_variables %in% vars$variable_names))
})


testname = "3.2.1 projectedclimate: show variable names (of a subset, specified: subset)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")

  # Run:
  vars <- download_projected_climate_tables(
    quiet=FALSE,
    subset=mysubset)

  # Check whether that subset was returned:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
})


testname = "3.2.2 projectedclimate: show variable names (of a subset, specified: various components)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  
  # Run:
  vars <- download_projected_climate_tables(
    quiet=FALSE,
    base_vars=mybasevars,
    models=mymodels,
    scenarios=myscenarios,
    versions=myversions,
    time_periods=mytimeperiods)

  # Run another time, this time, two components are specified "ALL",
  # which should lead to the same result!
  vars2 <- download_projected_climate_tables(
    quiet=FALSE,
    base_vars=mybasevars,
    models=mymodels,
    scenarios=myscenarios,
    versions="ALL",
    time_periods="ALL")

  # Check:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars, vars2)
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$models), mymodels)
  expect_equal(sort(vars$scenarios), myscenarios)
  expect_equal(sort(vars$versions), myversions)
  expect_equal(sort(vars$base_vars), mybasevars)
  expect_equal(sort(vars$time_periods), mytimeperiods)
})


testname = "3.3.1 projectedclimate: show download size (specified: subset / various components)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02", "h04v02")

  # Run:
  vars <- download_projected_climate_tables(
    quiet=FALSE,
    tile_ids=mytiles,
    subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"))

  # Run (same subset, specified using separate base_vars and years)
  vars2 <- download_projected_climate_tables(
    quiet=FALSE,
    tile_ids=mytiles,
    base_vars=c("bio1"),
    models=c("ipsl-cm6a-lr"),
    scenarios=c("ssp126"),
    versions=c("V.2.1"),
    time_periods=c("2041-2070", "2071-2100"))
    
  # Check that the tiles and the download size is returned:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars2$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(sort(vars2$tile_ids), mytiles)
  expect_equal(vars$download_bytes, 250104945)
  expect_equal(vars2$download_bytes, 250104945)
})


testname = "3.3.2 projectedclimate: show download size (test passing subset=ALL)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_projected_climate_tables(
    subset="ALL",
    tile_ids=c("h02v02"),
    quiet=FALSE)

  # Check whether the correct number of variables and the correct download size are returned.
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(length(vars$variable_names), 342)
  expect_equal(vars$download_bytes, 14643512823)
})


testname = "3.3.3 projectedclimate: show download size (test passing tile_ids=ALL)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_projected_climate_tables(
    subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1"),
    tile_ids="ALL",
    quiet=FALSE)

  # Check whether the correct number of tiles and the correct download size are returned.
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars$download_bytes, 5260577574)
  expect_equal(length(vars$tile_ids), 116)
})


testname = "3.4.1 projectedclimate: download subset"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 68 MB, so we skip it.')
  vars <- download_projected_climate_tables(
    tile_ids=c("h02v02"),
    subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"),
    download=TRUE,
    file_format="zip",
    delete_zips=FALSE,
    download_dir=download_dir,
    quiet=FALSE)

  # Check whether download size is listed correctly,
  # and whether the files are actually downloaded.
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars$download_bytes, 68103362)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "3.4.2 projectedclimate: download all variables (for one tile)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if(R_SKIP_HUGE_DOWNLOAD, "R_SKIP_HUGE_DOWNLOAD: This test download 12.9 GB, so we skip it.")
  # Note: Test 3.3.2 (computing download size of subset=ALL) tests quite similar behaviour,
  # just without the actual download in the end.
  vars <- download_projected_climate_tables(
    subset="ALL",
    tile_ids=c("h10v04"),
    download=TRUE,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether the correct number of variables and the correct download size are returned.
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(length(vars$variable_names), 342)
  expect_equal(vars$download_bytes, 12906642444)
})


testname = "3.4.3 projectedclimate: download all tiles (for one variable)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if(R_SKIP_HUGE_DOWNLOAD, "R_SKIP_HUGE_DOWNLOAD: This test download 10.5 GB, so we skip it.")
  # Note: Test 3.3.3 (computing download size of tile_ids=ALL) tests quite similar behaviour,
  # just without the actual download in the end.
  vars <- download_projected_climate_tables(
    subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"),
    tile_ids="ALL",
    download=TRUE,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether the correct number of variables and the correct download size are returned.
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(length(vars$variable_names), 342)
  expect_equal(vars$download_bytes, 10521001472)
})


testname = "3.5.1 projectedclimate: failure: model not available"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Model(s) xyz. Please check your spelling and try again!"

  # Run:
  expect_error(
    vars <- download_projected_climate_tables(
      models=c("ipsl-cm6a-lr", "xyz"),
      base_vars=c("bio1"),
      time_periods=c("2041-2070", "2071-2100"),
      scenarios=c("ssp126"),
      versions=c("V.2.1"),
      tile_ids=c("h02v02", "h04v02"),
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})


testname = "3.5.2 projectedclimate: failure: scenarios not available"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Scenario(s) xyz, abc. Please check your spelling and try again!"

  # Run:
  expect_error(
    vars <- download_projected_climate_tables(
      scenarios=c("ssp126", "xyz", "abc"),
      base_vars=c("bio1"),
      models=c("ipsl-cm6a-lr"),
      time_periods=c("2041-2070", "2071-2100"),
      versions=c("V.2.1"),
      tile_ids=c("h02v02", "h04v02"),
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})


testname = "3.5.3 projectedclimate: failure: version not available"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Version(s) xyz. Please check your spelling and try again!"

  # Run:
  expect_error(
    vars <- download_projected_climate_tables(
      versions=c("V.2.1", "xyz"),
      base_vars=c("bio1"),
      models=c("ipsl-cm6a-lr"),
      scenarios=c("ssp126"),
      tile_ids=c("h02v02", "h04v02"),
      time_periods=c("2041-2070", "2071-2100"),
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})


testname = "3.5.4 projectedclimate: failure: base_var not available"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Base var(s) xyz, abc, def. Please check your spelling and try again!"
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  expect_error(
    vars <- download_projected_climate_tables(
      tempdir=tmpdir,
      quiet=FALSE,
      base_vars=c("bio1", "xyz", "abc", "def"),
      models=mymodels,
      scenarios=myscenarios,
      versions=myversions,
      tile_ids=mytiles),
    expected_error_message, fixed=TRUE
  )
})

testname = "3.5.5 projectedclimate: failure: no subset passed"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "You did not provide any subset. If you are sure that you want ALL variables, please specify subset='ALL'!"

  # Run:
  expect_warning(
    vars <- download_projected_climate_tables(
      tile_ids=c("h02v02", "h04v02"),
      download=TRUE,
      tempdir=tmpdir,
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})


testname = "4.1 observedclimate: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run twice, should yield the same result!
  vars <- download_observed_climate_tables(quiet=FALSE)
  vars2 <- download_observed_climate_tables(quiet=FALSE, subset="ALL")

  # Check that the correct variable names are listed:
  expect_equal(vars, vars2)
  all_clim_vars <- c(
    "bio1",  "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18",
    "bio19", "bio2",  "bio3",  "bio4",  "bio5",  "bio6",  "bio7",  "bio8",  "bio9")
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), all_clim_vars)
})


testname = "4.2 observedclimate (or other simple tables): show variable names (of a subset)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_observed_climate_tables(
    subset=c("bio1",  "bio10"),
    quiet=FALSE)

  # Check that the correct variable names are listed:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), c("bio1",  "bio10"))
})


testname = "4.3.1 observedclimate (or other simple tables): show download size (specified: subset)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_observed_climate_tables(
    subset=c("bio10", "bio13"),
    tile_ids=c("h00v04", "h10v04"),
    quiet=FALSE)

  # Check whether tile_ids and download size are correctly listed:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$tile_ids), c("h00v04", "h10v04"))
  expect_equal(vars$download_bytes, 1296)
})


testname = "4.3.2 observedclimate (or other simple tables): show download size (test passing subset=ALL)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_observed_climate_tables(
    subset="ALL",
    tile_ids=c("h10v04"),
    download=FALSE,
    quiet=FALSE)

  # Check whether download size is correctly listed:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars$download_bytes, 3041)
})


testname = "4.3.3 observedclimate (or other simple tables): show download size (test passing tile_ids=ALL)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_observed_climate_tables(
    subset=c("bio5"),
    tile_ids="ALL",
    download=FALSE,
    quiet=FALSE)

  # Check whether download size is correctly listed:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars$download_bytes, 6982430850)
})


testname = "4.4.1 observedclimate (or other simple tables): download subset"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if_offline("public.igb-berlin.de")
  vars <- download_observed_climate_tables(
    subset=c("bio10", "bio13"),
    tile_ids=c("h00v04", "h10v04"),
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether download size is correctly listed, and whether zip files exist:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars$download_bytes, 1296)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "4.4.2 observedclimate (or other simple tables): download all variables (for one tile)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if_offline("public.igb-berlin.de")
  # Note: Test 4.3.2 (computing download size of subset=ALL) tests quite similar behaviour,
  # just without the actual download in the end.
  vars <- download_observed_climate_tables(
    subset="ALL",
    tile_ids=c("h10v04"),
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether download size is correctly listed, and whether zip files exist:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars$download_bytes, 3041)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "4.4.3 observedclimate (or other simple tables): download all tiles (for one variable)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  skip_if(R_SKIP_HUGE_DOWNLOAD, "R_SKIP_HUGE_DOWNLOAD: This test downloads 6.9 GB, so we skip.")
  # Note: Test 4.3.3 (computing download size of tile_ids=ALL) tests quite similar behaviour,
  # just without the actual download in the end.
  vars <- download_observed_climate_tables(
    subset=c("bio5"),
    tile_ids="ALL",
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE,
    quiet=FALSE)

  # Check whether download size is correctly listed, and whether zip files exist:
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(vars$download_bytes, 6982430850)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})


testname = "4.5.1 observedclimate (or other simple tables): failure: variable not available"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Variable(s) xyz. Please check your spelling and try again!"
  mysubset <- c("bio1", "bio10")
  mytiles <- 

  # Run:
  expect_error(
    vars <- download_observed_climate_tables(
      subset=c("bio1", "bio10", "xyz"),
      tile_ids=c("h02v02", "h04v02"),
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})

testname = "4.5.2 observedclimate (or other simple tables): failure: no subset provided"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "You did not provide any subset. If you are sure that you want ALL variables, please specify subset='ALL'!"

  # Run:
  expect_warning(
    vars <- download_observed_climate_tables(
      tile_ids=c("h02v02", "h04v02"),
      download=TRUE,
      quiet=FALSE),
    expected_error_message, fixed=TRUE
  )
})


testname = "5 soil: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
  # Note: All other soil download functionality is tested by testing the observed climate
  # download functionality - they all use the same download_simple_tables function!

  # Run twice, should yield the same result!
  vars <- download_soil_tables(quiet=FALSE)
  vars2 <- download_soil_tables(quiet=FALSE, subset="ALL")

  # Check whether all variable names are listed:
  expect_equal(vars, vars2)
  expect_equal(vars$dataset_name, "soilgrids250m_v2_0")
  all_soil_vars <- c(
    "acdwrb", "awcts", "bdricm", "bdrlog", "bldfie", "cecsol", "clyppt", "crfvol",
    "histpr", "orcdrc", "phihox", "slgwrb", "sltppt", "sndppt", "texmht", "wwp")
  expect_equal(sort(vars$variable_names), all_soil_vars)
})


testname = "6 hydro90m: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
  # Note: All other hydro90m download functionality is tested by testing the observed climate
  # download functionality - they all use the same download_simple_tables function!

  # Run twice, should yield the same result!
  vars <- download_hydrography90m_tables(quiet=FALSE)
  vars2 <- download_hydrography90m_tables(quiet=FALSE, subset="ALL")

  # Check:
  expect_equal(vars, vars2)
  expect_equal(vars$dataset_name, "hydrography90m_v1_0")
  all_hy90m_vars <- c(
    "channel_curv_cel", "channel_dist_dw_seg", "channel_dist_up_cel", "channel_dist_up_seg",
    "channel_elv_dw_cel", "channel_elv_dw_seg", "channel_elv_up_cel", "channel_elv_up_seg",
    "channel_grad_dw_seg", "channel_grad_up_cel", "channel_grad_up_seg", "connections",
    "cum_length", "elev_drop", "flow", "flow_accum", "flow_pos", "gradient", "length", "out_dist",
    "out_drop", "outlet_diff_dw_basin", "outlet_diff_dw_scatch", "outlet_dist_dw_basin",
    "outlet_dist_dw_scatch", "outlet_elev", "sinosoid", "slope_curv_max_dw_cel", "cti",
    "slope_curv_min_dw_cel", "slope_elv_dw_cel", "slope_grad_dw_cel", "source_elev", "spi", "sti",
    "stream_diff_dw_near", "stream_diff_up_farth", "stream_diff_up_near", "stream_dist_dw_near",
    "stream_dist_proximity", "stream_dist_up_farth", "stream_dist_up_near", "stream_drwal_old",
    "stream_hack", "stream_horton", "stream_scheidegger", "stream_shreve", "stream_strahler",
    "stream_topo", "stream_topo_dim", "stright")
  expect_equal(sort(vars$variable_names), sort(all_hy90m_vars))
})


testname = "7 cgiar: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
  # Note: All other cgiar download functionality is tested by testing the observed climate
  # download functionality - they all use the same download_simple_tables function!

  # Run twice, should yield the same result!
  vars <- download_cgiar_tables(quiet=FALSE)
  vars2 <- download_cgiar_tables(quiet=FALSE, subset="ALL")

  # Check:
  expect_equal(vars, vars2)
  expect_equal(vars$dataset_name, "cgiar_csi_v3")
  expect_equal(sort(vars$variable_names), c("garid", "gevapt"))
})


testname = "8 flo1k: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
  # Note: All other flo1k download functionality is tested by testing the observed climate
  # download functionality - they all use the same download_simple_tables function!

  # Run twice, should yield the same result!
  vars <- download_flo1k_tables(quiet=FALSE)
  vars2 <- download_flo1k_tables(quiet=FALSE, subset="ALL")

  # Check:
  expect_equal(vars, vars2)
  expect_equal(vars$dataset_name, "flo1k_v1_0")
  expect_equal(sort(vars$variable_names), c("flo1k"))
})


testname = "9 merit_dem: show variable names (all)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
  # Note: All other merit_dem download functionality is tested by testing the observed climate
  # download functionality - they all use the same download_simple_tables function!

  # Run twice, should yield the same result!
  vars <- download_merit_dem_tables(quiet=FALSE)
  vars2 <- download_merit_dem_tables(quiet=FALSE, subset="ALL")

  # Check:
  expect_equal(vars, vars2)
  expect_equal(vars$dataset_name, "merit_dem_v1_0_3")
  expect_equal(sort(vars$variable_names), c("elev"))
})

