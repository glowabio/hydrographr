
###################################################
### Testing the get_regional_unit_id() function ###
###################################################

# Cases to be covered:
# * Getting a regional unit for two points in a dataframe
# TODO: Think of more tests: What behaviours, what aspects of the code have to be covered?

# Make test dataframe for get_regional_unit_id:
schaalsee      <- c(53.59002044504782, 10.93165806482122)
ratzeburgersee <- c(53.76921119083455, 10.77521394785896)
df <- data.frame(t(data.frame(schaalsee, ratzeburgersee)))
colnames(df) <- c("lati", "longi")

# Test get_regional_unit_id:
test_that("get_regional_unit_id works", {
  reg_id <- get_regional_unit_id(df, "longi", "lati")
  expect_equal(reg_id, 58)
})