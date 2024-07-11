
######################################################
### Testing the download_future_climate() function ###
######################################################

# Cases to be covered:
# * Pass model, scenario, time_period correctly (tests 1,2,3,4)
# * Pass them wrong
# * futureclimate90m_paths_file_sizes.txt is NOT already there (test 1)
# * futureclimate90m_paths_file_sizes.txt is already there (test2, test 3)
# * files are unzipped to txt, zips are removed (test 3)
# * files are unzipped to txt, zips are NOT removed (test 4)
# * files are NOT unzipped to txt (test 1, test 2)

#########################
### Some preparations ###
#########################

tmpdir <- tempdir()
print(paste0('Tempdir: ', tmpdir))
file_size_file <- paste0(tmpdir,'/futureclimate90m_paths_file_sizes.txt')
file_size_file_url <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/futureclimate90m_paths_file_sizes.txt"
# TODO: Make small test data on the server, to run these faster!

#############
### Tests ###
#############

# test 1
test_that("1 downloading WITHOUT preexisting futureclimate90m_paths_file_sizes.txt", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_1")
    if (file.exists(file_size_file)) {
        file.remove(file_size_file)
    }

    # Run:
    download_future_climate(variable = c("bio1"), file_format = "zip",
        scenario = c("ssp370"), model = c("ipsl-cm6a-lr"),
        time_period = c("2071-2100"), tile_id = c("h00v02", "h16v02"),
        download_dir = download_dir)


    # Check:
    created_files <- list.files(paste0(download_dir, '/Climate/2071_2100/bio1'))
    expected_files <- c("bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1_h00v02.zip",
                        "bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1_h16v02.zip")
    expect_length(created_files, 2)
    expect_true(all(sort(expected_files) == sort(created_files)))
})


# test 2
test_that("2 downloading WITH preexisting environment90m_paths_file_sizes.txt...", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_2")
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_url, destfile = file_size_file, mode = "wb")
    }

    # Run:
    download_future_climate(variable = c("bio1"), file_format = "zip",
        scenario = c("ssp370"), model = c("ipsl-cm6a-lr"),
        time_period = c("2071-2100"), tile_id = c("h00v02", "h16v02"),
        download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/Climate/2071_2100/bio1'))
    expected_files <- c("bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1_h00v02.zip",
                        "bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1_h16v02.zip")
    expect_length(created_files, 2)
    expect_true(all(sort(expected_files) == sort(created_files)))
})

# test 3
test_that("3 unzipping, removing the zips", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_3")
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_url, destfile = file_size_file, mode = "wb")
    }

    # Run:
    download_future_climate(variable = c("bio1"), file_format = "txt",
        scenario = c("ssp370"), model = c("ipsl-cm6a-lr"),
        time_period = c("2071-2100"), tile_id = c("h00v02"),
        download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/Climate/2071_2100/bio1'))
    print(paste0('CREATED TEST 1', created_files))
    expected_files <- c("bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1_h00v02.txt")
    expect_length(created_files, 1)
    expect_true(all(sort(expected_files) == sort(created_files)))
})

# test 4
test_that("4 unzipping without removing the zips", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_4")
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_url, destfile = file_size_file, mode = "wb")
    }

    # Run:
    download_future_climate(variable = c("bio1"), file_format = "txt",
        scenario = c("ssp370"), model = c("ipsl-cm6a-lr"), delete_zips = FALSE,
        time_period = c("2071-2100"), tile_id = c("h00v02"),
        download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/Climate/2071_2100/bio1'))
    print(paste0('CREATED TEST 4', created_files))
    expected_files <- c("bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1_h00v02.txt",
     "bio1_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1_h00v02.zip")
    expect_length(created_files,2)
    expect_true(all(sort(expected_files) == sort(created_files)))
})


# test 5: FAIL
test_that("5: MUST FAIL: Wrong variable", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_5")
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_url, destfile = file_size_file, mode = "wb")
    }

    # Run:
    expect_error(
        download_future_climate(variable = c("bioxxx"), file_format = "txt",
            scenario = c("ssp370"), model = c("ipsl-cm6a-lr"), delete_zips = FALSE,
            time_period = c("2071-2100"), tile_id = c("h00v02"),
            download_dir = download_dir),
        regexp = "No valid variable requested!",
    )
})

# test 6: FAIL
test_that("6: MUST FAIL: Wrong scenario", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_6")
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_url, destfile = file_size_file, mode = "wb")
    }

    # Run:
    expect_error(
        download_future_climate(variable = c("bio1"), file_format = "txt",
            scenario = c("ssp370xxx"), model = c("ipsl-cm6a-lr"), delete_zips = FALSE,
            time_period = c("2071-2100"), tile_id = c("h00v02"),
            download_dir = download_dir),
        regexp = "No valid scenario requested!",
    )
})

# test 7: FAIL
test_that("7: MUST FAIL: Wrong scenario", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_7")
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_url, destfile = file_size_file, mode = "wb")
    }

    # Run:
    expect_error(
        download_future_climate(variable = c("bio1"), file_format = "txt",
            scenario = c("ssp370"), model = c("ipsl-cm6a-l-xxxr"), delete_zips = FALSE,
            time_period = c("2071-2100"), tile_id = c("h00v02"),
            download_dir = download_dir),
        regexp = "No valid model requested!",
    )
})

# test 8: FAIL
test_that("8: MUST FAIL: Wrong scenario", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_future_climate_8")
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_url, destfile = file_size_file, mode = "wb")
    }

    # Run:
    expect_error(
        download_future_climate(variable = c("bio1"), file_format = "txt",
            scenario = c("ssp370"), model = c("ipsl-cm6a-lr"), delete_zips = FALSE,
            time_period = c("2071-2100-xxx"), tile_id = c("h00v02"),
            download_dir = download_dir),
        regexp = "No valid time period requested!",
    )
})