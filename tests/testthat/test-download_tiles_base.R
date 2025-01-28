
##################################################
### Testing the download_tiles_base() function ###
##################################################

tests_quiet=TRUE

# Get which tests to skip:
R_SKIP_HUGE_DOWNLOAD <- !(Sys.getenv("R_SKIP_HUGE_DOWNLOAD") == "FALSE")
R_SKIP_DOWNLOAD <- Sys.getenv("R_SKIP_DOWNLOAD") == "TRUE"

# Cases to be covered:
# * Global case
# * cti_ovr.tif case: Even if IGB is specified, we download from GDrive!
# * GDrive case, IGB case
# * Regional units with regional unit ids
# * Proper tiles with tile ids
# * ...? TODO Think of more cases!

# TODO They might be better in a setup file?
# TODO: Make small test data on the server, to run these faster!



# Where to store and download files:
if (! exists("tmpdir")){
  tmpdir <- tempdir()
}

if (!(tests_quiet)) print("_______________________________")
if (!(tests_quiet)) print("Testing: download_tiles_base")


#############
### Tests ###
#############


testname = "1: Global case: 'direction_ovr.tif' (from IGB)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_HUGE_DOWNLOAD, 'R_SKIP_HUGE_DOWNLOAD: This test downloads 7431.1 MB, so we skip it.')

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_1")
    server_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"
    # Get file size table (code from: download_tiles.R)
    fname <- 'hydrography90m_paths_file_sizes.txt'
    file_size_file <- file.path(tmpdir, fname)
    if (!(file.exists(file_size_file))) {
        base_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2FREADME/"
        skip_if_offline("public.igb-berlin.de")
        download.file(paste0(base_url, fname), destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)

    # Run:
    download_tiles_base(
        variable = "direction",
        file_format = "tif",
        global = TRUE,
        file_size_table = file_size_table,
        server_url = server_url,
        download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'global'))
    expect_length(created_files, 1)
    expect_setequal(created_files, c("direction_ovr.tif"))
    file_size = file.info(file.path(download_dir, 'global', 'direction_ovr.tif'))[["size"]]
    expect_true(file_size > 5000)
})


testname = "2: Global special case 'cti_ovr.tif': Always downloaded from GDrive"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_HUGE_DOWNLOAD, 'R_SKIP_HUGE_DOWNLOAD: This test downloads 82 GB, so we skip it.')

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_2")

    # Run:
    download_tiles_base(variable = "cti", file_format = "tif", global = TRUE, file_size_table = file_size_table, server_url = server_url_igb, download_dir = download_dir)

    # Check:
    # TODO: How to check where it is downloaded from?
    created_files <- list.files(file.path(download_dir, 'global'))
    expect_length(created_files, 1)
    expect_setequal(created_files, c("cti_ovr.tif"))
    file_size = file.info(file.path(download_dir, 'global', 'cti_ovr.tif'))[["size"]]
    expect_true(file_size > 5000)
})


testname = "3: Normal case, not global, from IGB"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 15 MB, so we skip it.')

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_3")
    server_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"
    # Get file size table (code from: download_tiles.R)
    fname <- 'hydrography90m_paths_file_sizes.txt'
    file_size_file <- file.path(tmpdir, fname)
    if (!(file.exists(file_size_file))) {
        base_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2FREADME/"
        skip_if_offline("public.igb-berlin.de")
        download.file(paste0(base_url, fname), destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)

    # Run: Downloads 15 MB
    download_tiles_base(
        variable = "direction",
        file_format = "tif",
        tile_id = "h00v02",
        file_size_table = file_size_table,
        server_url = server_url,
        download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'r.watershed', 'direction_tiles20d'))
    expect_length(created_files, 1)
    expect_setequal(created_files, c("direction_h00v02.tif"))
    file_size = file.info(file.path(download_dir, 'r.watershed', 'direction_tiles20d', 'direction_h00v02.tif'))[["size"]]
    expect_true(file_size > 5000)
})


testname = "4: Normal case, not global, from GDrive"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 15 MB, so we skip it.')

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_4")
    server_url_gdrive <- "https://drive.google.com/uc?export=download&id="
    # Get file size table (code from: download_tiles.R)
    fname <- 'hydrography90m_paths_file_sizes.txt'
    file_size_file <- file.path(tmpdir, fname)
    if (!(file.exists(file_size_file))) {
        base_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2FREADME/"
        skip_if_offline("public.igb-berlin.de")
        download.file(paste0(base_url, fname), destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)

    # Run: Downloads 15 MB
    download_tiles_base(
        variable = "direction",
        file_format = "tif",
        tile_id = "h00v02",
        file_size_table = file_size_table,
        server_url = server_url_gdrive,
        download_dir = download_dir)

    # Check:
    created_files <- list.files(file.path(download_dir, 'r.watershed', 'direction_tiles20d'))
    expect_length(created_files, 1)
    expect_true(all(sort(c("direction_h00v02.tif")) == sort(created_files)))
    file_size = file.info(file.path(download_dir, 'r.watershed', 'direction_tiles20d', 'direction_h00v02.tif'))[["size"]]
    expect_true(file_size > 5000)
})


testname = "5: Try downloading non-existing file"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
    download_dir <- file.path(tmpdir, "test_download_tiles_base_5")
    server_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2F"
    # Get file size table (code from: download_tiles.R)
    fname <- 'hydrography90m_paths_file_sizes.txt'
    file_size_file <- file.path(tmpdir, fname)
    if (!(file.exists(file_size_file))) {
        base_url <- "https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4/download?path=%2FREADME/"
        skip_if_offline("public.igb-berlin.de")
        download.file(paste0(base_url, fname), destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)

    # Run: Downloads nothing
    expected_warning <- "Problem: Did not find any file \"idontexist_h00v02.tif\" in the list of files - are you sure it is a valid file?"
    expect_warning(
        res <- download_tiles_base(
            variable = "idontexist",
            file_format = "tif",
            tile_id = "h00v02",
            file_size_table = file_size_table,
            server_url = server_url,
            download_dir = download_dir),
        regexp = expected_warning
    )

    # Check:
    expect_true(is.null(res))
})
