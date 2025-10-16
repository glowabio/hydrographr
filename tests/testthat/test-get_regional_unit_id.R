
###################################################
### Testing the get_regional_unit_id() function ###
###################################################

tests_quiet=TRUE

# Get which tests to skip:
R_SKIP_HUGE_DOWNLOAD <- !(Sys.getenv("R_SKIP_HUGE_DOWNLOAD") == "FALSE")
R_SKIP_DOWNLOAD <- Sys.getenv("R_SKIP_DOWNLOAD") == "TRUE"
R_SKIP_ALL_WITH_BASH_SCRIPT <- Sys.getenv("R_SKIP_ALL_WITH_BASH_SCRIPT") == "TRUE"

# Notify about this one, as it is non-standard:
if (R_SKIP_ALL_WITH_BASH_SCRIPT) {
    message('R_SKIP_ALL_WITH_BASH_SCRIPT is set to TRUE, skipping tests that call bash scripts. If you want to run them, set R_SKIP_ALL_WITH_BASH_SCRIPT to FALSE')
} else{
    message('R_SKIP_ALL_WITH_BASH_SCRIPT is set to FALSE, not skipping tests that call bash scripts. If you want to skip them, set R_SKIP_ALL_WITH_BASH_SCRIPT to TRUE')
}

# Cases to be covered:
# * Getting a regional unit for two points in a dataframe
# TODO: Think of more tests: What behaviours, what aspects of the code have to be covered?

if (!(tests_quiet)) print("_______________________________")
if (!(tests_quiet)) print("Testing: get_regional_unit_id")

#############
### Tests ###
#############

testname = "get_regional_unit_id works"
if (!(tests_quiet)) print(paste("TEST: ", testname))
test_that(testname, {
  skip_if(R_SKIP_DOWNLOAD, 'R_SKIP_DOWNLOAD: This test downloads 4.5 MB, so we skip it.')
  skip_if(R_SKIP_ALL_WITH_BASH_SCRIPT, 'Testing functions that call a bash script in the background fails on Thomas Windows machine, so we skip this...')

  # Prepare:
  # Make test dataframe for get_regional_unit_id:
  schaalsee      <- c(53.59002044504782, 10.93165806482122)
  ratzeburgersee <- c(53.76921119083455, 10.77521394785896)
  df <- data.frame(t(data.frame(schaalsee, ratzeburgersee)))
  colnames(df) <- c("lati", "longi")

  # Run:
  reg_id <- get_regional_unit_id(df, "longi", "lati")
  expect_equal(reg_id, 58)
})

