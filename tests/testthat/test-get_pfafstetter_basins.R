#####################################################
### Testing the get_pfafstetter_basins() function ###
#####################################################

# Cases to be covered:
# --- NORMAL CASES (data_table = TRUE) ---
# * normal case: no hidden dependency on a global object "g_subset"   (test 1)
# * normal case: returns data.table with integer columns stream, code (test 2)
# * normal case: sorted by stream, no duplicated stream IDs           (test 3)
# * normal case: all codes are between 1 and 9                        (test 4)
# * normal case: basin is split into more than one sub-basin          (test 5)
# * normal case: every stream (except outlet) gets a code             (test 6)
# * normal case: a smaller basin (inner outlet) only returns its own
#                streams                                              (test 7)
# * normal case: result does not depend on n_cores                    (test 8)
# * normal case: quiet = FALSE prints diagnostic messages             (test 9)
# * normal case: quiet = TRUE prints no messages                      (test 10)
# * cross-check: each Pfafstetter sub-basin is a connected set of
#                stream segments in the input graph                   (test 11)
# --- NORMAL CASES (data_table = FALSE) ---
# * normal case: writes a raster to disk and returns a SpatRaster     (test 12)
# * cross-check: raster cell counts per code match the table          (test 13)
# --- ERROR CASES ---
# * MUST FAIL: quiet is not logical                                   (test 14)
# * MUST FAIL: input graph is not an igraph object                    (test 15)
# * MUST FAIL: input graph is undirected                              (test 16)
# * MUST FAIL: subc_raster missing                                    (test 17)
# * MUST FAIL: out_dir missing and data_table = FALSE                 (test 18)
# * MUST FAIL: file_name missing and data_table = FALSE               (test 19)
# * MUST FAIL: graph has too few stream segments                      (test 20)
# * MUST FAIL: graph has more than one outlet                         (test 21)
# * MUST FAIL: sub-catchment raster does not exist                    (test 22)


#########################
### Some preparations ###
#########################

# Where to store and download files:
if (!exists("tmpdir")) {
  tmpdir <- tempdir()
}

# Get which tests to skip:
R_SKIP_ALL_WITH_BASH_SCRIPT <- Sys.getenv("R_SKIP_ALL_WITH_BASH_SCRIPT") == "TRUE"

download_test_data(tmpdir)

# Input files (from hydrographr test data)
stream_vector <- file.path(tmpdir, "hydrography90m_test_data", "order_vect_59.gpkg")
subc_raster   <- file.path(tmpdir, "hydrography90m_test_data", "subcatchment_1264942.tif")

# Import the stream network as a graph, and subset it to one basin.
# NOTE: The graph is deliberately NOT called g_subset (the name used inside
# get_pfafstetter_basins()), see test 1.
my_graph <- read_geopackage(gpkg = stream_vector, import_as = "graph")
g_basin  <- get_catchment_graph(g = my_graph,
                                subc_id = "513867227",
                                use_outlet = FALSE,
                                mode = "in",
                                as_graph = TRUE)

# IDs of the whole basin and of its outlet
basin_ids_chr <- igraph::as_ids(igraph::V(g_basin))
basin_ids     <- as.integer(basin_ids_chr)
basin_outlet  <- as.integer(igraph::as_ids(
  igraph::V(g_basin)[igraph::degree(g_basin, mode = "out") == 0]
))

# A smaller basin: take the stream whose upstream area contains about half of
# the segments of the full basin, and subset the graph to its upstream part.
# induced_subgraph keeps all edge attributes (e.g. out_dist).
upstream_sizes <- vapply(
  basin_ids_chr,
  function(x) length(igraph::subcomponent(g_basin, x, mode = "in")),
  numeric(1)
)
half_id  <- basin_ids_chr[which.min(abs(upstream_sizes - length(basin_ids_chr) / 2))]
g_half   <- igraph::induced_subgraph(
  g_basin,
  igraph::subcomponent(g_basin, half_id, mode = "in")
)
half_ids <- as.integer(igraph::as_ids(igraph::V(g_half)))

# Two tiny graphs for the error cases. The edge attribute out_dist is needed
# because the function reads it before the graph checks are reached.
# (a) Only two edges: too few stream segments
g_tiny <- igraph::graph_from_data_frame(
  data.frame(from = c("1", "2"), to = c("2", "3"), out_dist = c(20, 10)),
  directed = TRUE
)
# (b) Two separate chains, i.e. two outlets (3 and 6)
g_two_outlets <- igraph::graph_from_data_frame(
  data.frame(from = c("1", "2", "4", "5"), to = c("2", "3", "5", "6"),
             out_dist = c(20, 10, 20, 10)),
  directed = TRUE
)


#############
### Tests ###
#############

testname <- "1: No hidden dependency on a global object called g_subset"
test_that(testname, {

  # The function body contains a check on an object called g_subset (the name
  # used in the roxygen example) instead of its own argument g. If such an
  # object exists in the global environment, the check silently uses it and
  # this test cannot detect the problem, so we skip in that case.
  skip_if(exists("g_subset", envir = globalenv()),
          "An object g_subset exists in the global env, test cannot detect the problem.")

  expect_no_error(
    get_pfafstetter_basins(
      g           = g_basin,
      subc_raster = subc_raster,
      data_table  = TRUE,
      n_cores     = 2,
      quiet       = TRUE
    )
  )
})


testname <- "2: Returns data.table with integer columns stream and code"
test_that(testname, {

  result <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  expect_s3_class(result, "data.table")
  expect_named(result, c("stream", "code"))
  expect_type(result$stream, "integer")
  expect_type(result$code, "integer")
  expect_gt(nrow(result), 0)
})


testname <- "3: Result is sorted by stream and has no duplicated stream IDs"
test_that(testname, {

  result <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  expect_false(is.unsorted(result$stream))
  expect_false(any(duplicated(result$stream)))
})


testname <- "4: All Pfafstetter codes are between 1 and 9"
test_that(testname, {

  result <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  expect_false(anyNA(result$code))
  expect_true(all(result$code %in% 1:9))
})


testname <- "5: Basin is split into more than one sub-basin"
test_that(testname, {

  result <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  expect_gt(length(unique(result$code)), 1)
})


testname <- "6: Every stream of the graph (except the outlet) gets a code"
test_that(testname, {

  result <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  # No stream IDs that are not in the input graph
  expect_true(all(result$stream %in% basin_ids))
  # Streams without a code: only the outlet itself may be missing
  no_code <- setdiff(basin_ids, result$stream)
  expect_true(all(no_code %in% basin_outlet),
              info = paste("Streams without a code (outlet:", basin_outlet, "):",
                           paste(head(no_code, 20), collapse = ", ")))
})


testname <- "7: Smaller basin (inner outlet) only returns its own streams"
test_that(testname, {

  result_full <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  result_half <- get_pfafstetter_basins(
    g           = g_half,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  expect_gt(nrow(result_half), 0)
  expect_true(all(result_half$stream %in% half_ids))
  expect_lt(nrow(result_half), nrow(result_full))
})


testname <- "8: Result does not depend on the number of cores"
test_that(testname, {

  result_1core <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 1,
    quiet       = TRUE
  )

  result_2core <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  expect_equal(result_1core, result_2core)
})


testname <- "9: quiet = FALSE prints diagnostic messages"
test_that(testname, {

  expect_message(
    get_pfafstetter_basins(
      g           = g_basin,
      subc_raster = subc_raster,
      data_table  = TRUE,
      n_cores     = 2,
      quiet       = FALSE
    ),
    regexp = "Number of vertices"
  )
})


testname <- "10: quiet = TRUE prints no messages"
test_that(testname, {

  expect_no_message(
    get_pfafstetter_basins(
      g           = g_basin,
      subc_raster = subc_raster,
      data_table  = TRUE,
      n_cores     = 2,
      quiet       = TRUE
    )
  )
})


testname <- "11: Each Pfafstetter sub-basin is a connected set of streams"
test_that(testname, {

  result <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  # All streams with the same code have to form one connected piece of the
  # river network (ignoring flow direction).
  for (code in sort(unique(result$code))) {
    members <- as.character(result$stream[result$code == code])
    sub_g   <- igraph::induced_subgraph(
      g_basin,
      which(basin_ids_chr %in% members)
    )
    expect_true(igraph::is_connected(sub_g, mode = "weak"),
                info = paste("Pfafstetter code", code))
  }
})


testname <- "12: data_table = FALSE writes a raster to disk and returns a SpatRaster"
test_that(testname, {
  skip_if(R_SKIP_ALL_WITH_BASH_SCRIPT,
          "R_SKIP_ALL_WITH_BASH_SCRIPT: The raster is written by a function that calls a bash script, so we skip this.")

  out_file <- file.path(tmpdir, "test12_pfafstetter_raster.tif")

  result <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    out_dir     = tmpdir,
    file_name   = "test12_pfafstetter_raster.tif",
    data_table  = FALSE,
    n_cores     = 2,
    quiet       = TRUE
  )

  expect_s4_class(result, "SpatRaster")
  expect_true(file.exists(out_file))
  expect_equal(terra::res(result), terra::res(terra::rast(subc_raster)))
})


testname <- "13: Raster cell counts per code match the data.table"
test_that(testname, {
  skip_if(R_SKIP_ALL_WITH_BASH_SCRIPT,
          "R_SKIP_ALL_WITH_BASH_SCRIPT: The raster is written by a function that calls a bash script, so we skip this.")

  result_dt <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    data_table  = TRUE,
    n_cores     = 2,
    quiet       = TRUE
  )

  result_rast <- get_pfafstetter_basins(
    g           = g_basin,
    subc_raster = subc_raster,
    out_dir     = tmpdir,
    file_name   = "test13_pfafstetter_raster.tif",
    data_table  = FALSE,
    n_cores     = 2,
    quiet       = TRUE
  )

  freq_pfaf <- terra::freq(result_rast)
  freq_subc <- terra::freq(terra::rast(subc_raster))

  for (code in sort(unique(result_dt$code))) {
    expected_cells <- sum(freq_subc$count[
      freq_subc$value %in% result_dt$stream[result_dt$code == code]
    ])
    observed_cells <- sum(freq_pfaf$count[freq_pfaf$value == code])
    expect_equal(observed_cells, expected_cells,
                 info = paste("Pfafstetter code", code))
  }
})


testname <- "14: MUST FAIL: quiet is not logical"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g           = g_basin,
      subc_raster = subc_raster,
      data_table  = TRUE,
      quiet       = "no"
    ),
    regexp = "quiet: Has to be TRUE or FALSE"
  )
})


testname <- "15: MUST FAIL: input graph is not an igraph object"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g           = data.frame(from = "1", to = "2"),
      subc_raster = subc_raster,
      data_table  = TRUE
    ),
    regexp = "Input must be an igraph object"
  )
})


testname <- "16: MUST FAIL: input graph is undirected"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g           = igraph::as_undirected(g_basin),
      subc_raster = subc_raster,
      data_table  = TRUE
    ),
    regexp = "must be a directed graph"
  )
})


testname <- "17: MUST FAIL: subc_raster missing"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g          = g_basin,
      data_table = TRUE
    ),
    regexp = "full path to the sub-catchment raster layer"
  )
})


testname <- "18: MUST FAIL: out_dir missing and data_table = FALSE"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g           = g_basin,
      subc_raster = subc_raster,
      file_name   = "should_fail.tif",
      data_table  = FALSE
    ),
    regexp = "full path of the output directory"
  )
})


testname <- "19: MUST FAIL: file_name missing and data_table = FALSE"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g           = g_basin,
      subc_raster = subc_raster,
      out_dir     = tmpdir,
      data_table  = FALSE
    ),
    regexp = "file name of the output file"
  )
})


testname <- "20: MUST FAIL: graph has too few stream segments"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g           = g_tiny,
      subc_raster = subc_raster,
      data_table  = TRUE,
      n_cores     = 2,
      quiet       = TRUE
    ),
    regexp = "too few stream segments"
  )
})


testname <- "21: MUST FAIL: graph has more than one outlet"
test_that(testname, {

  expect_error(
    get_pfafstetter_basins(
      g           = g_two_outlets,
      subc_raster = subc_raster,
      data_table  = TRUE,
      n_cores     = 2,
      quiet       = TRUE
    ),
    regexp = "Only one outlet possible"
  )
})


testname <- "22: MUST FAIL: sub-catchment raster does not exist"
test_that(testname, {

  # No regexp here: the message comes from terra::rast() and may differ
  # between terra versions.
  # GDAL prints a warning before terra throws the error, so silence it.
  expect_error(
    suppressWarnings(
      get_pfafstetter_basins(
        g           = g_basin,
        subc_raster = file.path(tmpdir, "nonexistent_subcatchment.tif"),
        data_table  = TRUE,
        n_cores     = 2,
        quiet       = TRUE
      )
    )
  )
})


# The function leaves the future plan and a parallel cluster behind (see the
# note at the end of get_pfafstetter_basins()), so reset the plan here:
future::plan(future::sequential)
