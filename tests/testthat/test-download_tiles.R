
#############################################
### Testing the download_tiles() function ###
#############################################

tests_quiet=TRUE

# Get which tests to skip:
R_SKIP_HUGE_DOWNLOAD <- !(Sys.getenv("R_SKIP_HUGE_DOWNLOAD") == "FALSE")
R_SKIP_DOWNLOAD <- Sys.getenv("R_SKIP_DOWNLOAD") == "TRUE"

# Cases to be covered:
# * Global case
# * Regional units with regional unit ids
# * Proper tiles with tile ids
# * ...? TODO Think of more cases!

if (!(tests_quiet)) print("_______________________________")
if (!(tests_quiet)) print("Testing: download_env90m_tables")


# TODO: Make small test data on the server, to run these faster!

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



#############
### Tests ###
#############

testname = "1 - global"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_HUGE_DOWNLOAD, 'R_SKIP_HUGE_DOWNLOAD: This test downloads 7.4 GB, so we skip it.')

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_1")

    # Run:
    download_tiles(variable = "direction", file_format = "tif", global = TRUE, download_dir = download_dir)
    #download_tiles(variable = "regional_unit", file_format = "tif", global = TRUE) # this is example 3, what is the difference? TODO

    # Check:
    created_files <- list.files(file.path(download_dir, 'global'))
    expect_length(created_files, 1)
    expect_setequal(created_files, c("direction_ovr.tif"))
    file_size = file.info(file.path(download_dir, 'global','direction_ovr.tif'))[["size"]]
    #skip('That damn virus check is beating me again...')
    expect_true(file_size > 5000)
})


testname = "2 - two regional unit raster masks"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 4.5 MB, so we skip it.')

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_2")

    # Run:
    download_tiles(
        variable = "regional_unit",
        file_format = "tif",
        reg_unit_id = c("33","34"),
        download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'r.watershed', 'regional_unit'))
    expect_length(created_files, 2)
    expect_setequal(created_files, c("regional_unit_33.tif", "regional_unit_34.tif"))
    file_size33 = file.info(file.path(download_dir, 'r.watershed', 'regional_unit', 'regional_unit_33.tif'))[["size"]]
    file_size34 = file.info(file.path(download_dir, 'r.watershed', 'regional_unit', 'regional_unit_34.tif'))[["size"]]
    expect_true(file_size33 > 5000)
    expect_true(file_size34 > 5000)
})


testname = "3 - downloading WITHOUT preexisting hydrography90m_paths_file_sizes.txt"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 2.4 MB, so we skip it.')

    # TODO: This basically just tests downloading hydrography90m_paths_file_sizes.txt,
    # doesn't it? Maybe redundancy between this test and some tests in test-download_tiles_base.R

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_3")
    if (file.exists(file.path(tmpdir,'hydrography90m_paths_file_sizes.txt'))) {
        file.remove(file.path(tmpdir,'hydrography90m_paths_file_sizes.txt'))
    }

    # Run:
    download_tiles(variable = "regional_unit", file_format = "tif", reg_unit_id = c("33"), download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'r.watershed', 'regional_unit'))
    expect_length(created_files, 1)
    expect_setequal(created_files, c("regional_unit_33.tif"))
    file_size33 = file.info(file.path(download_dir, 'r.watershed', 'regional_unit', 'regional_unit_33.tif'))[["size"]]
    expect_true(file_size33 > 5000)
})


testname = "4 - downloading WITH preexisting hydrography90m_paths_file_sizes.txt"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 2.4 MB, so we skip it.')

    # Prepare:
    fname <- 'hydrography90m_paths_file_sizes.txt'
    file_size_file <- file.path(tmpdir, fname)
    if (!(file.exists(file_size_file))) {
        base_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2FREADME/"
        skip_if_offline("public.igb-berlin.de")
        download.file(paste0(base_url, fname), destfile = file_size_file, mode = "wb")
    }
    download_dir <- file.path(tmpdir, "test_download_tiles_4")

    # Run:
    download_tiles(variable = "regional_unit", file_format = "tif", reg_unit_id = c("33"), download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'r.watershed', 'regional_unit'))
    expect_length(created_files, 1)
    expect_setequal(created_files, c("regional_unit_33.tif"))
    file_size33 = file.info(file.path(download_dir, 'r.watershed', 'regional_unit', 'regional_unit_33.tif'))[["size"]]
    expect_true(file_size33 > 5000)
})
