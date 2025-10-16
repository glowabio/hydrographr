
#################################################
### Testing the download_test_data() function ###
#################################################

tests_quiet=TRUE

# Get which tests to skip:
R_SKIP_HUGE_DOWNLOAD <- !(Sys.getenv("R_SKIP_HUGE_DOWNLOAD") == "FALSE")
R_SKIP_DOWNLOAD <- Sys.getenv("R_SKIP_DOWNLOAD") == "TRUE"

# Cases to be covered:
# * Failing at IGB, downloading at GDrive

# Where to store and download files:
if (! exists("tmpdir")){
  tmpdir <- tempdir()
}

if (!(tests_quiet)) print("_______________________________")
if (!(tests_quiet)) print("Testing: download_test_data")


#############
### Tests ###
#############

testname = "download from IGB works"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
    skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 36.4 MB, so we skip it.')

    # Prepare:
    download_dir = file.path(tmpdir, "test_download_test_data_1")
    ifelse(!dir.exists(download_dir), dir.create(download_dir), FALSE)

    # Run:
    download_test_data(download_dir = download_dir) 

    # Check:
    expect_true(TRUE)
    created_files <- list.files(file.path(download_dir, 'hydrography90m_test_data'))
    expect_length(created_files, 45)
    expected_files <- c("basin_1264942.tif", "basin_59.gpkg", "bid_59_bin.gpkg",
        "chancurv_1264942.tif", "chandistdwseg_1264942.tif", "chandistupcel_1264942.tif",
        "chandistupseg_1264942.tif", "chanelvdwcel_1264942.tif", "chanelvdwseg_1264942.tif",
        "chanelvupcel_1264942.tif", "chanelvupseg_1264942.tif", "changraddwseg_1264942.tif",
        "changradupcel_1264942.tif", "changradupseg_1264942.tif", "cti_1264942.tif",
        "direction_1264942.tif", "elev_1264942.tif", "flow_1264942.tif", "flowpos_1264942.tif",
        "lbasin_1264942.gpkg", "order_vect_59.gpkg", "outdiffdwbasin_1264942.tif",
        "outdiffdwscatch_1264942.tif", "outdistdwbasin_1264942.tif", "outdistdwscatch_1264942.tif",
        "outlet_59.gpkg", "regional_unit_ovr.tif", "slopcmax_1264942.tif", "slopcmin_1264942.tif",
        "slopdiff_1264942.tif", "slopgrad_1264942.tif", "spdata_1264942_subcIDs.txt",
        "spdata_1264942.txt", "spi_1264942.tif", "sti_1264942.tif", "strdiffdwnear_1264942.tif",
        "strdiffupfarth_1264942.tif", "strdiffupnear_1264942.tif", "strdistdwnear_1264942.tif",
        "strdistprox_1264942.tif", "strdistupfarth_1264942.tif", "strdistupnear_1264942.tif",
        "stream_1264942.tif", "sub_catchment_59.gpkg", "subcatchment_1264942.tif")
    expect_setequal(created_files, expected_files)
})

