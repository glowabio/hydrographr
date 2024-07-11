
###########################################
### Testing the download_env() function ###
###########################################

# Cases to be covered:
# * Bioclimatatic variables (test 2, LATER)
# * LandCover variables, specified as one string (test 3)
# * LandCover variables, years specified separately (test 5)
# * LandCover variables, mixed (5)
# * environment90m_paths_file_sizes.txt is already there (test 3 and many more)
# * environment90m_paths_file_sizes.txt is NOT already there  (test 1)
# * files are unzipped to txt, zips are removed (test 1, test 2, )
# * files are unzipped to txt, zips are NOT removed (test 4)
# * files are NOT unzipped to txt (test 3, test 5)


tmpdir <- tempdir()
print(paste0('Tempdir: ', tmpdir))
# TODO: Make small test data on the server, to run these faster!


# test 1
test_that("downloading WITHOUT preexisting environment90m_paths_file_sizes.txt", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_env_1")
    if (file.exists(paste0(tmpdir,'/environment90m_paths_file_sizes.txt'))) {
        file.remove(paste0(tmpdir,'/environment90m_paths_file_sizes.txt'))
    }

    # Run:
    download_env(variable = c("c20_1992"), tile_id = c("h00v02"), download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/LandCover/c20'))
    expected_files <- c("c20_1992_h00v02.txt")
    expect_length(created_files, 1)
    expect_true(all(sort(expected_files) == sort(created_files)))
})

# test 2
# For LATER:
test_that("downloading bioclimatic variables", {
    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_env_2")

    # Run:
    skip("We will test this once bioclimatic variables are renamed on the server!")
    download_env(variable = c("bio1", "bio2"), tile_id = c("h00v02", "h16v02"), download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/LandCover/c20'))
    expected_files <- c("bio1_h00v02.txt", "bio1_h16v02.txt", "bio2_h00v02.txt", "bio2_h16v02.txt")
    expect_length(created_files, 4)
    expect_true(all(sort(expected_files) == sort(created_files)))
})

# test 3
test_that("downloading WITH preexisting environment90m_paths_file_sizes.txt, not-unzipping, passing land cover as one string", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_env_3")
    if (!(file.exists(paste0(tmpdir,'/environment90m_paths_file_sizes.txt')))) {
        # TODO: Download file to here!
    }

    # Run:
    download_env(variable = c("c20_1992", "c20_1996"), file_format = "zip", tile_id = c("h00v02", "h16v02"), download_dir = download_dir)


    # Check:
    created_files <- list.files(paste0(download_dir, '/LandCover/c20'))
    expected_files <- c("c20_1992_h00v02.zip", "c20_1992_h16v02.zip", "c20_1996_h00v02.zip", "c20_1996_h16v02.zip")
    expect_length(created_files, 4)
    expect_true(all(sort(expected_files) == sort(created_files)))
})


# test 4
test_that("unzipping without removing the zips", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_env_4")

    # Run:
    download_env(variable = c("c20_1992", "c20_1996"), tile_id = c("h00v02", "h16v02"), file_format='txt', delete_zips = FALSE, download_dir = download_dir)


    # Check:
    created_files <- list.files(paste0(download_dir, '/LandCover/c20'))
    expected_files <- c("c20_1992_h00v02.txt", "c20_1992_h16v02.txt", "c20_1996_h00v02.txt", "c20_1996_h16v02.txt",
                        "c20_1992_h00v02.zip", "c20_1992_h16v02.zip", "c20_1996_h00v02.zip", "c20_1996_h16v02.zip")
    expect_length(created_files, 8)
    expect_true(all(sort(expected_files) == sort(created_files)))
})


# test 5
test_that("land cover: specifying years separately, as int and string", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_env_5")

    # Run:
    download_env(variable = c("c10", "c20"), years = c(1992, '1996'), file_format = "zip", tile_id = c("h00v02", "h16v02"), download_dir = download_dir)

    # Check:
    created_files10 <- list.files(paste0(download_dir, '/LandCover/c10'))
    created_files20 <- list.files(paste0(download_dir, '/LandCover/c20'))
    expect_length(created_files10, 4)
    expect_length(created_files20, 4)
    expected_files10 <- c("c10_1992_h00v02.zip", "c10_1992_h16v02.zip", "c10_1996_h00v02.zip", "c10_1996_h16v02.zip")
    expected_files20 <- c("c20_1992_h00v02.zip", "c20_1992_h16v02.zip", "c20_1996_h00v02.zip", "c20_1996_h16v02.zip")
    expect_true(all(sort(expected_files10) == sort(created_files10)))
    expect_true(all(sort(expected_files20) == sort(created_files20)))
})


# test 6
test_that("land cover: pass variables in a mixed way (one as one string, another with years passed separately)", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_env_6")

    # Run:
    download_env(variable = c("c20_1992", "c10"), years = c(1996, 1997), tile_id = c("h00v02", "h16v02"), download_dir = download_dir)

    # Check:
    created_files10 <- list.files(paste0(download_dir, '/LandCover/c10'))
    created_files20 <- list.files(paste0(download_dir, '/LandCover/c20'))
    expect_length(created_files10, 4)
    expect_length(created_files20, 2)
    expected_files10 <- c("c10_1996_h00v02.txt", "c10_1997_h00v02.txt", "c10_1996_h16v02.txt", "c10_1997_h16v02.txt")
    expected_files20 <- c("c20_1992_h00v02.txt", "c20_1992_h16v02.txt")
    expect_true(all(sort(expected_files10) == sort(created_files10)))
    expect_true(all(sort(expected_files20) == sort(created_files20)))
})
