
#############################################
### Testing the download_tiles() function ###
#############################################

# Cases to be covered:
# * Global case
# * Regional units with regional unit ids
# * Proper tiles with tile ids
# * ...? TODO Think of more cases!

SKIP_SUPERSLOW = TRUE



#########################
### Some preparations ###
#########################

# TODO They might be better in a setup file?
# TODO: Make small test data on the server, to run these faster!


# Temp dir for storing the results
tmpdir <- tempdir()
print(paste0('Tempdir: ', tmpdir))


#############
### Tests ###
#############

# test 1
test_that("example 1, global", {

    skip_if(SKIP_SUPERSLOW, 'Downloading this is 7431.1 MB, so we skip it this time...')

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_tiles_1")

    # Run:
    download_tiles(variable = "direction", file_format = "tif", global = TRUE, download_dir = download_dir)
    #download_tiles(variable = "regional_unit", file_format = "tif", global = TRUE) # this is example 3, what is the difference? TODO

    # Check:
    created_files <- list.files(paste0(download_dir, '/global'))
    expected_files <- c("direction_ovr.tif")
    expect_length(created_files, 1)
    expect_true(all(sort(expected_files) == sort(created_files)))
    file_size = file.info(paste0(download_dir, '/global/direction_ovr.tif'))[["size"]]
    skip('That damn virus check is beating me again...')
    expect_true(file_size > 5000)
})

# test 2
test_that("example 2, two regional unit raster masks", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_tiles_2")

    # Run:
    download_tiles(variable = "regional_unit", file_format = "tif", reg_unit_id = c("33","34"), download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/r.watershed/regional_unit'))
    expected_files <- c("regional_unit_33.tif", "regional_unit_34.tif")
    expect_length(created_files, 2)
    expect_true(all(sort(expected_files) == sort(created_files)))
    file_size33 = file.info(paste0(download_dir, '/r.watershed/regional_unit/regional_unit_33.tif'))[["size"]]
    file_size34 = file.info(paste0(download_dir, '/r.watershed/regional_unit/regional_unit_34.tif'))[["size"]]
    expect_true(file_size33 > 5000)
    expect_true(file_size34 > 5000)
})


# test 3
test_that("downloading WITHOUT preexisting hydrography90m_paths_file_sizes.txt", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_tiles_3")
    if (file.exists(paste0(tmpdir,'/hydrography90m_paths_file_sizes.txt'))) {
        file.remove(paste0(tmpdir,'/hydrography90m_paths_file_sizes.txt'))
    }

    # Run:
    download_tiles(variable = "regional_unit", file_format = "tif", reg_unit_id = c("33"), download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/r.watershed/regional_unit'))
    expected_files <- c("regional_unit_33.tif")
    expect_length(created_files, 1)
    expect_true(all(sort(expected_files) == sort(created_files)))
    file_size33 = file.info(paste0(download_dir, '/r.watershed/regional_unit/regional_unit_33.tif'))[["size"]]
    expect_true(file_size33 > 5000)
})


# test 4
test_that("downloading WITH preexisting hydrography90m_paths_file_sizes.txt", {

    # Prepare:
    download_dir = paste0(tmpdir, "/test_download_tiles_4")
    if (!(file.exists(paste0(tmpdir,'/hydrography90m_paths_file_sizes.txt')))) {
        # TODO: Download file to here!
    }

    # Run:
    download_tiles(variable = "regional_unit", file_format = "tif", reg_unit_id = c("33"), download_dir = download_dir)

    # Check:
    created_files <- list.files(paste0(download_dir, '/r.watershed/regional_unit'))
    expected_files <- c("regional_unit_33.tif")
    expect_length(created_files, 1)
    expect_true(all(sort(expected_files) == sort(created_files)))
    file_size33 = file.info(paste0(download_dir, '/r.watershed/regional_unit/regional_unit_33.tif'))[["size"]]
    expect_true(file_size33 > 5000)
})