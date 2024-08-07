
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

tmpdir <- tempdir()
print(paste0('Tempdir: ', tmpdir))

# Get file size table (code from: download_tiles.R)
file_size_file_hy <- file.path(tmpdir, 'hydrography90m_paths_file_sizes.txt')
file_size_file_hy_url <- "https://drive.google.com/uc?export=download&id=1SEkcgGPutP6ZQPvYtzICh_gcGnVgH_uR&confirm=t"
if (!(file.exists(file_size_file_hy))) {
    download.file(file_size_file_hy_url, destfile = file_size_file_hy, mode = "wb")
}
file_size_table_hy <- fread(file_size_file_hy, sep = ";")
file_size_table_hy$file_name = basename(file_size_table_hy$file_path)
all_varnames_hy <- sort(unique(sub("_[^_]+$", "", file_size_table_hy$file_name)))
all_file_names_hy <- sort(unique(file_size_table_hy$file_name))
all_tile_ids_hy <- unique(str_extract(file_size_table_hy$file_path, "h[0-9]+v[0-9]+"))
all_tile_ids_hy <- all_tile_ids_hy[!is.na(all_tile_ids_hy)]

file_size_file_env_url <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/environment90m_paths_file_sizes.txt"
file_size_file_futclim_url <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/futureclimate90m_paths_file_sizes.txt"

#############
### Tests ###
#############

# test 1
test_that("hydrography90m_paths_file_sizes.txt", {

    # Run:
    size <- check_tiles_filesize('order_topo', file_format = "tif", tile_id = "h16v02",
        h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
        h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy)

    # Check:
    expect_equal(size, 27330052)
})

# test 2
test_that("environment90m_paths_file_sizes.txt", {

    # Prepare:
    # Get file size table (code from: download_tiles.R)
    file_size_file <- file.path(tmpdir, 'environment90m_paths_file_sizes.txt')
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_env_url,
            destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    size <- check_tiles_filesize('c10_2013', file_format = "zip", tile_id = "h10v04",
        h90m_varnames = all_varnames, h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names, file_size_table = file_size_table)

    # Check:
    expect_equal(size, 155)
})

# test 3
test_that("futureclimate90m_paths_file_sizes.txt", {

    # Get file size table (code from: download_tiles.R)
    file_size_file <- file.path(tmpdir, 'futureclimate90m_paths_file_sizes.txt')
    if (!(file.exists(file_size_file))) {
        download.file(file_size_file_futclim_url, destfile = file_size_file, mode = "wb")
    }
    file_size_table <- fread(file_size_file, sep = ";")
    file_size_table$file_name = basename(file_size_table$file_path)
    all_varnames <- sort(unique(sub("_[^_]+$", "", file_size_table$file_name)))
    all_file_names <- sort(unique(file_size_table$file_name))
    all_tile_ids <- unique(str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
    all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]

    # Run:
    size <- check_tiles_filesize('bio11_2071-2100_ipsl-cm6a-lr_ssp370_V.2.1',
        file_format = "zip", tile_id = "h12v04",
        h90m_varnames = all_varnames, h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names, file_size_table = file_size_table)

    # Check:
    expect_equal(size, 628)
})

# test 4
test_that("hydrography90m_paths_file_sizes.txt", {

    # Run:
    size <- check_tiles_filesize('order_topo', file_format = "tif", tile_id = "h16v02",
        global = TRUE,
        h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
        h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy)

    # Check:
    expect_equal(size, 6125520067)
})

# test 5, MUST FAIL: 
test_that("MUST FAIL: Global case, wrong format: Asking for gpkg, not for tif", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('order_topo', file_format = "gpkg",
            global = TRUE,
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' should be “tif”"
    )
})

# test 6, MUST FAIL:
test_that("MUST FAIL: Passing integer tile_id instead of h00v00 pattern", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('order_topo', file_format = "tif", tile_id = 150,
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' must be NULL or a character vector"
    )
})


# test 7a, existing regional unit
test_that("7a Normal regional unit case", {

    # Run:
    size <- check_tiles_filesize('regional_unit', file_format = "tif", tile_id = "190",
        h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
        h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy)

    # Check:
    expect_equal(size, 780854)
})


# test 7c, existing regional unit
test_that("7c Normal regional unit case, passing tile_id as integer (which is wrong, expect char)", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('regional_unit', file_format = "tif", tile_id = 190,
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' must be NULL or a character vector", fixed = TRUE
    )
})

# test 8a, non-existing regional unit!
# We ask for regional unit 140, which does not exist.
test_that("MUST FAIL: 8a: Regional unit case, but regional unit 140 does not exist! Passing tile_id as char.", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('regional_unit', file_format = "tif", tile_id = "140",
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' should be one of “1”, “2”, “3”.+", fixed=FALSE
    )
})

# test 8b, non-existing regional unit!
# We ask for regional unit 140, which does not exist.
# We would like regional unit 140, and pass it as integer. So it first fails as it expects characters,
# before even checking whether 140 exists (hint: it doesn't...)
test_that("MUST FAIL: 8b: Regional unit case, but regional unit 140 does not exist! Passing tile_id as integer.", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('regional_unit', file_format = "tif", tile_id = 140,
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' must be NULL or a character vector", fixed=TRUE
    )
})

# test 9, MUST FAIL
test_that("MUST FAIL: 9: Regional unit case, unit is wrong: h00v00 instead of an integer", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('regional_unit', file_format = "tif", tile_id = "h16v02",
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' should be one of “1”, “2”, “3”.+", fixed=FALSE
    )
})

# test 10, MUST FAIL:
test_that("MUST FAIL: Wrong file format (gpkg instead of tif)", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('order_topo', file_format = "gpkg", tile_id = "h16v02",
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' should be “tif”"
    )
})

# test 10, MUST FAIL:
test_that("Wrong file format (tif instead of gpkg)", {

    # Run:
    expect_error(
        size <- check_tiles_filesize('order_vect_segment', file_format = "tif", tile_id = "h16v02",
            h90m_varnames = all_varnames_hy, h90m_tile_id = all_tile_ids_hy,
            h90m_file_names = all_file_names_hy, file_size_table = file_size_table_hy),
        regexp = "'arg' should be “gpkg”"
    )
})
