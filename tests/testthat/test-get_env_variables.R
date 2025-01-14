
###############################################
### Testing the get_env_variables functions ###
###############################################

print('TESTING GET ENV VARIABLES')


#########################
### Some preparations ###
#########################

# Where to store and download files:
if (is.null(tmpdir)){
  tmpdir <- tempdir()
}
#tmpdir <- "/tmp"
download_dir <- "/tmp"

# Get which tests to skip:
SKIP_DOWNLOAD <- Sys.getenv("SKIP_DOWNLOAD") == "TRUE" # empty string / FALSE if not set
SKIP_HUGE_DOWNLOAD <- Sys.getenv("SKIP_HUGE_DOWNLOAD") == "TRUE" || SKIP_DOWNLOAD
SKIP_FAILING_DOWNLOAD <- Sys.getenv("SKIP_FAILING_DOWNLOAD") == "TRUE" # empty string / FALSE if not set

#############
### Tests ###
#############

testname = "1.1 helpers: table getter"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  fname <- "env90m_presentclimate_paths_file_sizes.txt"

  # Run:
  tab <- get_file_size_table(
    file_name = fname,
    tempdir = tmpdir)

  # Check:
  expect_length(tab, 4)
  expected_cols <- c("file_path", "file_id", "file_size", "file_name")
  created_cols <- names(tab)
  expect_true(all(sort(expected_cols) == sort(created_cols)))
  expect_length(tab$file_name, 2204)
})


testname = "1.1.2 helpers: table getter: failure"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  fname <- "env90m_presentclimate_paths_file_sizes.txt"

  # Run:
  expect_warning(expect_error(
    tab <- get_file_size_table(
      file_name = "BLABLUB.txt",
      tempdir = tmpdir)
  ))
})

testname = "1.2.1: helpers: compute download size"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02", "h04v02")
  # mysubset <- c("awcts", "wwp")
  # fname <- "env90m_soil_paths_file_sizes.txt"
  # expected_bytes <- 310093648
  # mysubset <- c("c20_1992", "c20_1994")
  # fname <- "env90m_landcover_paths_file_sizes.txt"
  # expected_bytes <- 53921030
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  fname <- "env90m_futureclimate_paths_file_sizes.txt"
  expected_bytes <- 250104945
  file_size_table <- get_file_size_table(
    file_name = fname,
    tempdir = tmpdir)
  
  # Run:
  bytes <- compute_download_size(
    mytiles,
    mysubset,
    file_size_table,
    quiet = FALSE,
    ignore_missing = FALSE
  )

  # Check:
  expect_equal(bytes, expected_bytes)
})

testname = "1.2.2: helpers: compute download size: failure"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Tile id(s) h99v99. Please check your spelling and try again!"
  mytiles <- c("h02v02", "h04v02")
  # mysubset <- c("awcts", "wwp")
  # fname <- "env90m_soil_paths_file_sizes.txt"
  # expected_bytes <- 310093648
  # mysubset <- c("c20_1992", "c20_1994")
  # fname <- "env90m_landcover_paths_file_sizes.txt"
  # expected_bytes <- 53921030
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  fname <- "env90m_futureclimate_paths_file_sizes.txt"
  expected_bytes <- 250104945
  file_size_table <- get_file_size_table(
    file_name = fname,
    tempdir = tmpdir)

  # Run:
  expect_error(
    bytes <- compute_download_size(
      c("h02v02", "h04v02", "h99v99"),
      mysubset,
      file_size_table,
      quiet = FALSE,
      ignore_missing = FALSE
    ),
    expected_error_message, fixed=TRUE
  )
})



testname = "1.3.1: helpers: download zips (not delete)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02")
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  fname <- "env90m_futureclimate_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(
    file_name = fname,
    tempdir = tmpdir)

  # Run:
  skip_if(SKIP_DOWNLOAD, 'SKIP_DOWNLOAD: This test downloads 65.0 MB, so we skip it.')
  result <- do_download(
    mysubset, # TODO: Look for small subset for testing download...
    mytiles,
    file_size_table,
    download_dir = ".",
    file_format = "zip",
    quiet = FALSE,
    delete_zips = FALSE)

  # Check:
  expected_zips <- c(
    "./chelsa_bioclim_v2_1/2041_2070/bio1/bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1_h02v02.zip",
    "./chelsa_bioclim_v2_1/2071_2100/bio1/bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1_h02v02.zip"
  )
  # Check whether zips are reported:
  expect_equal(result$downloaded, expected_zips)
  # Check whether zips actually exist:
  for (zipfilename in expected_zips) {
    expect_true(file.exists(zipfilename))
  }
})

testname = "1.3.2: helpers: download zip, unzip to txt (not delete)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02")
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  fname <- "env90m_futureclimate_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(
    file_name = fname,
    tempdir = tmpdir)

  # Run:
  skip_if(SKIP_DOWNLOAD, 'SKIP_DOWNLOAD: This test downloads 65.0 MB, so we skip it.')
  result <- do_download(
    mysubset, # TODO: Look for small subset for testing download...
    mytiles,
    file_size_table,
    download_dir = ".",
    file_format = "txt",
    quiet = FALSE,
    delete_zips = FALSE)

  # Check:
  expected_zips <- c(
    "./chelsa_bioclim_v2_1/2041_2070/bio1/bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1_h02v02.zip",
    "./chelsa_bioclim_v2_1/2071_2100/bio1/bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1_h02v02.zip"
  )
  expected_unzipped <- c(
    "./chelsa_bioclim_v2_1/2041_2070/bio1",
    "./chelsa_bioclim_v2_1/2071_2100/bio1"
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
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02")
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  fname <- "env90m_futureclimate_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(
    file_name = fname,
    tempdir = tmpdir)

  # Run:
  skip_if(SKIP_DOWNLOAD, 'SKIP_DOWNLOAD: This test downloads 65.0 MB, so we skip it.')
  result <- do_download(
    mysubset, # TODO: Look for small subset for testing download...
    mytiles,
    file_size_table,
    download_dir = ".",
    file_format = "txt",
    quiet = FALSE,
    delete_zips = TRUE)

  # Check:
  expected_zips <- c(
    "./chelsa_bioclim_v2_1/2041_2070/bio1/bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1_h02v02.zip",
    "./chelsa_bioclim_v2_1/2071_2100/bio1/bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1_h02v02.zip"
  )
  expected_unzipped <- c(
    "./chelsa_bioclim_v2_1/2041_2070/bio1",
    "./chelsa_bioclim_v2_1/2071_2100/bio1"
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


testname = "2.1 landcover: show all"
print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_landcover_tables(
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "base_vars", "years", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_length(vars$base_vars, 22)
  expect_length(vars$years, 29)
  examples <- c("c100_1992", "c100_1993", "c100_1994", "c100_1995", "c100_1996", "c100_1997")
  expect_true(all(examples %in% vars$variable_names))
})

test_that("2.2.1 landcover: show subset (specified: subset)", {

  # Prepare:
  mysubset <- c("c20_1992", "c20_1994", "c30_1992", "c30_1994")

  # Run:
  vars <- download_landcover_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_length(vars$variable_names, 4) # WIPPP
  expect_equal(sort(vars$variable_names), sort(mysubset))
})

testname = "2.2.2 landcover: show subset (specified: base_vars and years)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("c20_1992", "c20_1994", "c30_1992", "c30_1994")
  mybasevars <- c("c20", "c30")
  myyears <- c(1992, 1994)
  
  # Run:
  vars <- download_landcover_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    base_vars=mybasevars,
    years=myyears)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "base_vars", "years", "dataset_name")))
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
  expect_equal(sort(vars$base_vars), sort(mybasevars))
  expect_equal(sort(vars$years), sort(myyears))
})

testname = "2.3.1 landcover: show subset and download_size (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("c20_1992", "c20_1994", "c30_1992", "c30_1994")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 107945857
  
  # Run:
  vars <- download_landcover_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "download_bytes", "tile_ids", "dataset_name")))
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
  expect_equal(vars$download_bytes, expected_bytes)
  expect_equal(sort(vars$tile_ids), sort(mytiles))
})

testname = "2.3.2 landcover: show subset and download_size (specified: base_vars and years)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("c20_1992", "c20_1994", "c30_1992", "c30_1994")
  mybasevars <- c("c20", "c30")
  myyears <- c(1992, 1994)
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 107945857

  # Run:
  vars <- download_landcover_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    base_vars=mybasevars,
    years=myyears,
    tile_ids=mytiles)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "base_vars", "years", "tile_ids", "download_bytes", "dataset_name")))
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
  expect_equal(sort(vars$base_vars), sort(mybasevars))
  expect_equal(sort(vars$years), sort(myyears))
  expect_equal(sort(vars$tile_ids), sort(mytiles))
  expect_equal(vars$download_bytes, expected_bytes)
})

testname = "2.4.1 landcover: show subset and download size, then download (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("c20_1992", "c20_1994", "c30_1992", "c30_1994")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 107945857

  # Run:
  skip_if(SKIP_DOWNLOAD, 'SKIP_DOWNLOAD: This test downloads 51.4 MB, so we skip it.')
  vars <- download_landcover_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles,
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "download_bytes", "tile_ids", "downloaded", "dataset_name")))
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
  expect_equal(vars$download_bytes, expected_bytes)
  expect_equal(sort(vars$tile_ids), sort(mytiles))
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
  expect_true(FALSE) # TODO: Check if the criteria are ok! When writing this test, the files to be downloaded were not on the server yet.
})

testname = "2.4.2 landcover: show subset and download size, then download (specified: base_vars and years)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("c20_1992", "c20_1994", "c30_1992", "c30_1994")
  mybasevars <- c("c20", "c30")
  myyears <- c(1992, 1994)
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 107945857

  # Run:
  skip_if(SKIP_DOWNLOAD, 'SKIP_DOWNLOAD: This test downloads 51.4 MB, so we skip it.')
  vars <- download_landcover_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    base_vars=mybasevars,
    years=myyears,
    tile_ids=mytiles,
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "base_vars", "years", "tile_ids", "download_bytes", "dataset_name")))
  expect_equal(vars$dataset_name, "esa_cci_landcover_v2_1_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
  expect_equal(sort(vars$base_vars), sort(mybasevars))
  expect_equal(sort(vars$years), sort(myyears))
  expect_equal(sort(vars$tile_ids), sort(mytiles))
  expect_equal(vars$download_bytes, expected_bytes)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
  expect_true(FALSE) # TODO: Check if the criteria are ok! When writing this test, the files to be downloaded were not on the server yet.
})

testname = "3.1 future climate: show all"
print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_future_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE)

  # Check: WIP
  expect_equal(sort(names(vars)), sort(c("comment", "base_vars", "models", "scenarios", "time_periods", "versions", "variable_names", "dataset_name")))
  all_base_vars <- c(
    "bio1",  "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18",
    "bio19", "bio2",  "bio3",  "bio4",  "bio5",  "bio6",  "bio7",  "bio8",  "bio9")
  expect_equal(sort(vars$base_vars), all_base_vars)
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
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

testname = "3.2.1 futureclimate: show subset (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")

  # Run:
  vars <- download_future_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
})

testname = "3.2.2 futureclimate: show subset (specified: various components)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  
  # Run:
  vars <- download_future_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    base_vars=mybasevars,
    models=mymodels,
    scenarios=myscenarios,
    versions=myversions)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "models", "scenarios", "versions", "time_periods", "base_vars", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$models), mymodels)
  expect_equal(sort(vars$scenarios), myscenarios)
  expect_equal(sort(vars$versions), myversions)
  expect_equal(sort(vars$base_vars), mybasevars)
  expect_equal(sort(vars$time_periods), mytimeperiods)
})

testname = "3.3.1 futureclimate: show subset and download size (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  vars <- download_future_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "tile_ids", "download_bytes", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(vars$download_bytes, 250104945)
})

testname = "3.3.2 futureclimate: show subset and download size (specified: various components)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  vars <- download_future_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    base_vars=mybasevars,
    models=mymodels,
    scenarios=myscenarios,
    versions=myversions,
    tile_ids=mytiles)

  # Check:
  all_colnames <- c("comment", "variable_names", "models", "scenarios", "versions", "time_periods", "base_vars", "tile_ids", "download_bytes", "dataset_name")
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(names(vars)), sort(all_colnames))
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$models), mymodels)
  expect_equal(sort(vars$scenarios), myscenarios)
  expect_equal(sort(vars$versions), myversions)
  expect_equal(sort(vars$base_vars), mybasevars)
  expect_equal(sort(vars$time_periods), mytimeperiods)
  expect_equal(vars$download_bytes, 250104945)
})

testname = "3.4.1 futureclimate: show subset and download size, then download (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02")
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  expected_bytes <- 68103362

  # Run:
  skip_if(SKIP_HUGE_DOWNLOAD, 'SKIP_HUGE_DOWNLOAD: This test downloads 250.1 MB, so we skip it.')
  vars <- download_future_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles,
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "download_bytes", "tile_ids", "downloaded", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), sort(mysubset))
  expect_equal(vars$download_bytes, expected_bytes)
  expect_equal(sort(vars$tile_ids), sort(mytiles))
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})

testname = "3.4.2 futureclimate: show subset and download size, then download (specified: various components)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mytiles <- c("h02v02")
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  expected_bytes <- 68103362

  # Run:
  skip_if(SKIP_DOWNLOAD, 'SKIP_DOWNLOAD: This test downloads 6.8 MB, so we skip it.')
  vars <- download_future_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    base_vars=mybasevars,
    models=mymodels,
    scenarios=myscenarios,
    versions=myversions,
    time_periods=mytimeperiods,
    tile_ids=mytiles,
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE)

  # Check:
  expected_cols <- c("comment", "variable_names", "download_bytes", "tile_ids", "downloaded", "versions", "models", "scenarios", "base_vars", "time_periods", "dataset_name")
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(names(vars)), sort(expected_cols))
  expect_equal(sort(vars$variable_names), sort(mysubset))
  expect_equal(vars$download_bytes, expected_bytes)
  expect_equal(sort(vars$tile_ids), sort(mytiles))
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
})

testname = "3.5.1 futureclimate: failure: model not available"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Model(s) xyz. Please check your spelling and try again!"
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  expect_error(
    vars <- download_future_climate_tables(
      tempdir=tmpdir,
      quiet=FALSE,
      base_vars=mybasevars,
      models=c("ipsl-cm6a-lr", "xyz"),
      scenarios=myscenarios,
      versions=myversions,
      tile_ids=mytiles),
    expected_error_message, fixed=TRUE
  )
})

testname = "3.5.2 futureclimate: failure: scenarios not available"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Scenario(s) xyz, abc. Please check your spelling and try again!"
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  expect_error(
    vars <- download_future_climate_tables(
      tempdir=tmpdir,
      quiet=FALSE,
      base_vars=mybasevars,
      models=mymodels,
      scenarios=c("ssp126", "xyz", "abc"),
      versions=myversions,
      tile_ids=mytiles),
    expected_error_message, fixed=TRUE
  )
})

testname = "3.5.3 futureclimate: failure: version not available"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Version(s) xyz. Please check your spelling and try again!"
  mysubset <- c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1")
  mybasevars <- c("bio1")
  mytimeperiods <- c("2041-2070", "2071-2100")
  mymodels <- c("ipsl-cm6a-lr")
  myscenarios <- c("ssp126")
  myversions <- c("V.2.1")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  expect_error(
    vars <- download_future_climate_tables(
      tempdir=tmpdir,
      quiet=FALSE,
      base_vars=mybasevars,
      models=mymodels,
      scenarios=myscenarios,
      versions=c("V.2.1", "xyz"),
      tile_ids=mytiles),
    expected_error_message, fixed=TRUE
  )
})

testname = "3.5.4 futureclimate: failure: base_var not available"
print(paste("TEST: ", testname))
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
    vars <- download_future_climate_tables(
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

testname = "4.1 presentclimate: show all"
print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_present_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  all_clim_vars <- c(
    "bio1",  "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18",
    "bio19", "bio2",  "bio3",  "bio4",  "bio5",  "bio6",  "bio7",  "bio8",  "bio9")
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), all_clim_vars)
})

testname = "4.2 presentclimate: show subset (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1",  "bio10")

  # Run:
  vars <- download_present_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), mysubset)
})

testname = "4.3 presentclimate: show subset and download size (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1",  "bio10")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 335702566

  # Run:
  vars <- download_present_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "tile_ids", "download_bytes", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(vars$download_bytes, expected_bytes)
})

testname = "4.4 presentclimate: show subset and download size, then download (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("bio1",  "bio10")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 335702566

  # Run:
  skip_if(SKIP_HUGE_DOWNLOAD, 'SKIP_HUGE_DOWNLOAD: This test downloads 335.7 MB, so we skip it.')
  vars <- download_present_climate_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles,
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "tile_ids", "download_bytes", "downloaded", "dataset_name")))
  expect_equal(vars$dataset_name, "chelsa_bioclim_v2_1")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(vars$download_bytes, expected_bytes)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
  expect_true(FALSE) # TODO: Check if the criteria are ok! When writing this test, the files to be downloaded were not on the server yet.
})

testname = "4.5 presentclimate: failure: variable not available"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Variable(s) xyz. Please check your spelling and try again!"
  mysubset <- c("bio1", "bio10")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  expect_error(
    vars <- download_present_climate_tables(
      tempdir=tmpdir,
      quiet=FALSE,
      subset=c("bio1", "bio10", "xyz"),
      tile_ids=mytiles),
    expected_error_message, fixed=TRUE
  )
})

testname = "5.1 soil: show all"
print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_soil_tables(
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "soilgrids250m_v2_0")
  all_soils <- c(
    "acdwrb", "awcts", "bdricm", "bdrlog", "bldfie", "cecsol", "clyppt", "crfvol",
    "histpr", "orcdrc", "phihox", "slgwrb", "sltppt", "sndppt", "texmht", "wwp")
  expect_equal(sort(vars$variable_names), all_soils)
})

testname = "5.2 soil: show subset (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("awcts", "wwp")

  # Run:
  vars <- download_soil_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "soilgrids250m_v2_0")
  expect_equal(sort(vars$variable_names), mysubset)
})

testname = "5.3 soil: show subset and download size (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("awcts", "wwp")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 310093648

  # Run:
  vars <- download_soil_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "tile_ids", "download_bytes", "dataset_name")))
  expect_equal(vars$dataset_name, "soilgrids250m_v2_0")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(vars$download_bytes, expected_bytes)
})

testname = "5.4 soil: show subset and download size, then download (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("awcts", "wwp")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 310093648

  # Run:
  skip_if(SKIP_HUGE_DOWNLOAD, 'SKIP_HUGE_DOWNLOAD: This test downloads 310.1 MB, so we skip it.')
  vars <- download_soil_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles,
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "tile_ids", "download_bytes", "downloaded", "dataset_name")))
  expect_equal(vars$dataset_name, "soilgrids250m_v2_0")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(vars$download_bytes, expected_bytes)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
  expect_true(FALSE) # TODO: Check if the criteria are ok! When writing this test, the files to be downloaded were not on the server yet.
})

testname = "5.5 soil: failure: variable not available"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Variable(s) xyz. Please check your spelling and try again!"
  mysubset <- c("awcts", "wwp")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  expect_error(
    vars <- download_soil_tables(
      tempdir=tmpdir,
      quiet=FALSE,
      subset=c("awcts", "wwp", "xyz"),
      tile_ids=mytiles),
    expected_error_message, fixed=TRUE
  )
})

testname = "6.1 hydro90m: show all"
print(paste("TEST: ", testname))
test_that(testname, {

  # Run:
  vars <- download_hydrography90m_tables(
    tempdir=tmpdir,
    quiet=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "hydrography90m_v1_0")
  all_hy90m_vars <- c(
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
  expect_equal(sort(vars$variable_names), all_hy90m_vars)
})

testname = "6.2 hydro90m: show subset (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("flow_accum", "spi")

  # Run:
  vars <- download_hydrography90m_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "dataset_name")))
  expect_equal(vars$dataset_name, "hydrography90m_v1_0")
  expect_equal(sort(vars$variable_names), mysubset)
})

testname = "6.3 hydro90m: show subset and download size (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("flow_accum", "spi")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 242013279

  # Run:
  vars <- download_hydrography90m_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "tile_ids", "download_bytes", "dataset_name")))
  expect_equal(vars$dataset_name, "hydrography90m_v1_0")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(vars$download_bytes, expected_bytes)
})

testname = "6.4 hydro90m: show subset and download size, then download (specified: subset)"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  mysubset <- c("flow_accum", "spi")
  mytiles <- c("h02v02", "h04v02")
  expected_bytes <- 242013279

  # Run:
  skip_if(SKIP_DOWNLOAD, 'SKIP_DOWNLOAD: This test downloads 242.0 MB, so we skip it.')
  vars <- download_hydrography90m_tables(
    tempdir=tmpdir,
    quiet=FALSE,
    subset=mysubset,
    tile_ids=mytiles,
    download=TRUE,
    download_dir=download_dir,
    file_format="zip",
    delete_zips=FALSE)

  # Check:
  expect_equal(sort(names(vars)), sort(c("comment", "variable_names", "tile_ids", "download_bytes", "downloaded", "dataset_name")))
  expect_equal(vars$dataset_name, "hydrography90m_v1_0")
  expect_equal(sort(vars$variable_names), mysubset)
  expect_equal(sort(vars$tile_ids), mytiles)
  expect_equal(vars$download_bytes, expected_bytes)
  # Check whether downloaded zips actually exist:
  for (zipfilename in vars$downloaded) {
    expect_true(file.exists(zipfilename))
  }
  expect_true(FALSE) # TODO: Check if the criteria are ok! When writing this test, the files to be downloaded were not on the server yet.
})

testname = "6.5 hydro90m: failure: variable not available"
print(paste("TEST: ", testname))
test_that(testname, {

  # Prepare:
  expected_error_message <- "Not available: Variable(s) xyz. Please check your spelling and try again!"
  mysubset <- c("flow_accum", "spi")
  mytiles <- c("h02v02", "h04v02")

  # Run:
  expect_error(
    vars <- download_hydrography90m_tables(
      tempdir=tmpdir,
      quiet=FALSE,
      subset=c("flow_accum", "spi", "xyz"),
      tile_ids=mytiles),
    expected_error_message, fixed=TRUE
  )
})


