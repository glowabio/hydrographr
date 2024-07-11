
##################################################
### Testing the download_tiles_base() function ###
##################################################

# Cases to be covered:
# * Global case
# * cti_ovr.tif case: Even if IGB is specified, we download from GDrive!
# * GDrive case, IGB case
# * Regional units with regional unit ids
# * Proper tiles with tile ids
# * ...? TODO Think of more cases!



SKIP_SUPERSLOW <- Sys.getenv("SKIP_SUPERSLOW") == "TRUE" # empty string / FALSE if not set
if (SKIP_SUPERSLOW) {
    print('SKIP_SUPERSLOW is set to TRUE, skipping superslow tests. If you want to run them, set SKIP_SUPERSLOW to FALSE')
} else {
    print('SKIP_SUPERSLOW is set to FALSE, not skipping superslow tests. If you want to skip them, set SKIP_SUPERSLOW to TRUE')
}

SKIP_SLOW <- Sys.getenv("SKIP_SLOW") == "TRUE" # empty string / FALSE if not set
if (SKIP_SLOW) {
    print('SKIP_SLOW is set to TRUE, skipping slow tests. If you want to run them, set SKIP_SLOW to FALSE')
} else {
    print('SKIP_SLOW is set to FALSE, not skipping slow tests. If you want to skip them, set SKIP_SLOW to TRUE')
}


#########################
### Some preparations ###
#########################

# TODO They might be better in a setup file?
# TODO: Make small test data on the server, to run these faster!


# Temp dir for storing the results
tmpdir <- tempdir()
print(paste0('Tempdir: ', tmpdir))

# File of file sizes that the function needs
# (Is usually downloaded and passed by the calling function)
file_size_file <- file.path(tempdir(), "hydrography90m_paths_file_sizes.txt")
if (!file.exists(file_size_file)) {
    download.file("https://drive.google.com/uc?export=download&id=1SEkcgGPutP6ZQPvYtzICh_gcGnVgH_uR&confirm=t", destfile = file_size_file, mode = "wb")
}
file_size_table <- fread(file_size_file, sep = ";")
file_size_table$file_name = basename(file_size_table$file_path)

# Server URL, is usually passed by calling function
server_url_igb <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"

#############
### Tests ###
#############

# test 1
test_that("1 Global case: 'direction_ovr.tif' (from IGB)", {

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_1")

    # Run:
    skip_if(SKIP_SUPERSLOW, 'Downloading this is 7431.1 MB, so we skip it this time...')
    download_tiles_base(variable = "direction", file_format = "tif", global = TRUE, file_size_table = file_size_table, server_url = server_url_igb, download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'global'))
    expected_files <- c("direction_ovr.tif")
    expect_length(created_files, 1)
    expect_setequal(created_files, expected_files)
    file_size = file.info(file.path(download_dir, 'global', 'direction_ovr.tif'))[["size"]]
    expect_true(file_size > 5000)
})

# test 2
test_that("2 Global special case 'cti_ovr.tif': Always downloaded from GDrive", {

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_2")

    # Run:
    skip_if(SKIP_SUPERSLOW, 'Downloading this is 82 GB, so we skip it this time...')
    download_tiles_base(variable = "cti", file_format = "tif", global = TRUE, file_size_table = file_size_table, server_url = server_url_igb, download_dir = download_dir)

    # Check:
    # TODO: How to check where it is downloaded from?
    created_files <- list.files(file.path(download_dir, 'global'))
    expected_files <- c("cti_ovr.tif")
    expect_length(created_files, 1)
    expect_setequal(created_files, expected_files)
    file_size = file.info(file.path(download_dir, 'global', 'cti_ovr.tif'))[["size"]]
    expect_true(file_size > 5000)
})


# test 3
test_that("3 Normal case, not global, from IGB", {

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_3")

    # Run: Downloads 15 MB
    skip_if(SKIP_SLOW, 'Downloading this is 15 MB, so we skip it this time...')
    download_tiles_base(variable = "direction", file_format = "tif", tile_id = "h00v02", file_size_table = file_size_table, server_url = server_url_igb, download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'r.watershed', 'direction_tiles20d'))
    expected_files <- c("direction_h00v02.tif")
    expect_length(created_files, 1)
    expect_setequal(created_files, expected_files)
    file_size = file.info(file.path(download_dir, 'r.watershed', 'direction_tiles20d', 'direction_h00v02.tif'))[["size"]]
    expect_true(file_size > 5000)
})



# test 4
test_that("4 Try downloading non-existing file", {

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_4")

    # Run: Downloads nothgin
    expected_warning <- "Problem: Did not find any file \"idontexist_h00v02.tif\" in the list of files - are you sure it is a valid file?"
    expect_warning(
        res <- download_tiles_base(variable = "idontexist", file_format = "tif", tile_id = "h00v02", file_size_table = file_size_table, server_url = server_url_igb, download_dir = download_dir),
        regexp = expected_warning
    )

    # Check:
    expect_true(is.null(res))
})
