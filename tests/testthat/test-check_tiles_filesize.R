
###################################################
### Testing the check_tiles_filesize() function ###
###################################################

# Cases to be covered:
# * normal case, hydrography: not global, not regional_units, normal tile_ids (test 1)
# * normal case, environment: not global, not regional_units, normal tile_ids (test 2)
# * future climate case: file names have several dots (test 3)
# * global has to be tif (test 4)
# * global with not tif (test 5)
# * normal case goes bad: not global, not regional_units, pass integer tile_ids (test 6)
# * not global: regional unit needs tile_ids as integers (test 7)
# * not global: regional unit with wrong tile_ids (test 8)
# * normal case goes bad: unmatching file format (test 9, test 10)


#########################
### Some preparations ###
#########################

tests_quiet=TRUE

# NOT COMMIT:
tests_quiet=FALSE

# Where to store and download files:
if (! exists("tmpdir")){
  tmpdir <- tempdir()
}


#############
### Tests ###
#############

testname = "1: one tile"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    bytes <- check_tiles_filesize(
        'order_topo',
        file_format = "tif",
        tile_id = "h16v02",
        h90m_varnames = all_varnames,
        h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names,
        file_size_table = file_size_table)

    # Check whether the size of those two tiles is really 27 MB
    expect_equal(bytes, 27330052)
})


testname = "2: global"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    bytes <- check_tiles_filesize(
        'order_topo',
        file_format = "tif",
        tile_id = "h16v02",
        global = TRUE,
        h90m_varnames = all_varnames,
        h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names,
        file_size_table = file_size_table)

    # Check whether the size of that global file is really 6.1 GB
    expect_equal(bytes, 6125520067)
})

testname = "3: MUST FAIL: Global case, wrong format: Asking for gpkg, not for tif"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize(
            'order_topo',
            file_format = "gpkg",
            global = TRUE,
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' should be “tif”"
    )
})


testname = "4: MUST FAIL: Passing integer tile_id instead of h00v00 pattern"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize(
            'order_topo',
            file_format = "tif",
            tile_id = 150,
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' must be NULL or a character vector"
    )
})


testname = "5.1: Normal regional unit case"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    skip_if_offline("public.igb-berlin.de") # Only 0.8 MB, no need to skip for that size...
    bytes <- check_tiles_filesize(
        'regional_unit',
        file_format = "tif",
        tile_id = "190",
        h90m_varnames = all_varnames,
        h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names,
        file_size_table = file_size_table)

    # Check:
    expect_equal(bytes, 780854)
})


testname = "5.2: Normal regional unit case, passing tile_id as integer (which is wrong, expect char)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize(
            'regional_unit',
            file_format = "tif",
            tile_id = 190,
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' must be NULL or a character vector", fixed = TRUE
    )
})


testname = "6.1: MUST FAIL: Regional unit case, but regional unit 140 does not exist! Passing tile_id as char."
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize(
            'regional_unit',
            file_format = "tif",
            tile_id = "140",
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' should be one of “1”, “2”, “3”.+", fixed=FALSE
    )
})


testname = "6.2: MUST FAIL: Regional unit case, but regional unit 140 does not exist! Passing tile_id as integer."
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # We ask for regional unit 140, which does not exist.
    # We would like regional unit 140, and pass it as integer. So it first fails as it expects characters,
    # before even checking whether 140 exists (hint: it doesn't...)

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize(
            'regional_unit',
            file_format = "tif",
            tile_id = 140,
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' must be NULL or a character vector", fixed=TRUE
    )
})


testname = "7: MUST FAIL: Regional unit case, unit is wrong: h00v00 instead of an integer."
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize(
            'regional_unit',
            file_format = "tif",
            tile_id = "h16v02",
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' should be one of “1”, “2”, “3”.+", fixed=FALSE
    )
})



testname = "8.1: MUST FAIL: Wrong file format (gpkg instead of tif)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize('order_topo',
            file_format = "gpkg",
            tile_id = "h16v02",
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' should be “tif”"
    )
})

testname = "8.2: MUST FAIL: Wrong file format (tif instead of gpkg)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
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
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    expect_error(
        bytes <- check_tiles_filesize(
            'order_vect_segment',
            file_format = "tif",
            tile_id = "h16v02",
            h90m_varnames = all_varnames,
            h90m_tile_id = all_tile_ids,
            h90m_file_names = all_file_names,
            file_size_table = file_size_table),
        regexp = "'arg' should be “gpkg”"
    )
})


testname = "9.1: Test with another file size file for the env90m case (present climate)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {



    # Prepare:
    # Get file size table (code from: download_tiles.R)
    fname <- 'env90m_presentclimate_paths_file_sizes.txt'
    file_size_file <- file.path(tmpdir, fname)
    if (!(file.exists(file_size_file))) {
        base_url <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/"
        # Note: base_url not the same as for hydrography90m!
        skip_if_offline("public.igb-berlin.de")
        download.file(paste0(base_url, fname), destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    bytes <- check_tiles_filesize(
        'bio5',
        file_format = "zip",
        tile_id = "h10v04",
        h90m_varnames = all_varnames,
        h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names,
        file_size_table = file_size_table)

    # Check:
    expect_equal(bytes, 159)
})


testname = "9.2: Test with another file size file for the env90m case (future climate)"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {

    # Prepare:
    fname <- 'env90m_futureclimate_paths_file_sizes.txt'
    file_size_file <- file.path(tmpdir, fname)
    if (!(file.exists(file_size_file))) {
        base_url <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/"
        # Note: base_url not the same as for hydrography90m!
        skip_if_offline("public.igb-berlin.de")
        download.file(paste0(base_url, fname), destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    bytes <- check_tiles_filesize(
        'bio11_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1',
        file_format = "zip",
        tile_id = "h12v04",
        h90m_varnames = all_varnames,
        h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names,
        file_size_table = file_size_table)

    # Check:
    expect_equal(bytes, 628)
})
