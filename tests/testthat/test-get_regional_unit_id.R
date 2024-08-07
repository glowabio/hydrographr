
###################################################
### Testing the get_regional_unit_id() function ###
###################################################

# Cases to be covered:
# * Getting a regional unit for two points in a dataframe
# TODO: Think of more tests: What behaviours, what aspects of the code have to be covered?

SKIP_ALL_WITH_BASH_SCRIPT <- Sys.getenv("SKIP_ALL_WITH_BASH_SCRIPT") == "TRUE" # empty string / FALSE if not set
if (SKIP_ALL_WITH_BASH_SCRIPT) {
    message('SKIP_ALL_WITH_BASH_SCRIPT is set to TRUE, skipping tests that call bash scripts. If you want to run them, set SKIP_ALL_WITH_BASH_SCRIPT to FALSE')
} else{
    message('SKIP_ALL_WITH_BASH_SCRIPT is set to FALSE, not skipping tests that call bash scripts. If you want to skip them, set SKIP_ALL_WITH_BASH_SCRIPT to TRUE')
}


#########################
### Some preparations ###
#########################

# Make test dataframe for get_regional_unit_id:
schaalsee      <- c(53.59002044504782, 10.93165806482122)
ratzeburgersee <- c(53.76921119083455, 10.77521394785896)
df <- data.frame(t(data.frame(schaalsee, ratzeburgersee)))
colnames(df) <- c("lati", "longi")

#############
### Tests ###
#############

# Test get_regional_unit_id:
test_that("get_regional_unit_id works", {
  skip_if(SKIP_ALL_WITH_BASH_SCRIPT, 'Testing functions that call a bash script in the background fails on Thomas Windows machine, so we skip this...')

  reg_id <- get_regional_unit_id(df, "longi", "lati")
  expect_equal(reg_id, 58)
})

