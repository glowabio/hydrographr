

##############################################
### Env90m: How to retrieve variable names ###
###         and download data              ###
###         (usage examples)               ###
##############################################
# Merret, 2025-01-03  

# For each dataset, there is a "download_xxx_tables()" function,
# with which you can do several things:
# 
# (1) Ask for all variables: It will display all variables in a list, also split by model, scenario etc.
# (2) Specify a subset by specifying components such as model, scenario etc. or by passing selected variable names: It will check whether they exist and return only the existing ones
# (3) Specify a subset and tile_ids: It will compute the download size
# (4) Specify a subset and tile_ids and say download=TRUE: It will download




######################
### future climate ###
######################

# (1) Ask for all variables
download_future_climate_tables(tempdir = "/tmp")

# (2a) Specify a subset by specifying components such as model, scenario etc.
download_future_climate_tables(tempdir = "/tmp", base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"))

# (3a) Specify a subset and tile_ids
download_future_climate_tables(tempdir = "/tmp", base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"), tile_ids=c("h02v02", "h04v02"))

# (4a) Specify a subset and tile_ids and say download=TRUE
download_future_climate_tables(tempdir = "/tmp", base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_future_climate_tables(tempdir = "/tmp", base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"), tile_ids=c("h02v02", "h04v02"), download=TRUE, file_format="txt", delete_zips="TRUE")

# (2b) Specify a subset directly by simply passing selected variables
download_future_climate_tables(tempdir = "/tmp", subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"))

# (3b) Specify a subset and tile_ids
download_future_climate_tables(tempdir = "/tmp", subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"), tile_ids=c("h02v02", "h04v02"))

# (4b) Specify a subset and tile_ids and say download=TRUE
download_future_climate_tables(tempdir = "/tmp", subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_future_climate_tables(tempdir = "/tmp", subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")



##################
### land cover ###
##################

# (1) Ask for all variables
download_landcover_tables(tempdir = "/tmp")
# (2) Specify a subset by specifying components such as base_vars and years
download_landcover_tables(tempdir = "/tmp", base_vars=c("c20", "c30"), years=c(1992, 1994))
# (3) Specify a subset and tile_ids
download_landcover_tables(tempdir = "/tmp", base_vars=c("c20", "c30"), years=c(1992, 1994), tile_ids=c("h02v02", "h04v02"))
# (4) Specify a subset and tile_ids and say download=TRUE
download_landcover_tables(tempdir = "/tmp", base_vars=c("c20", "c30"), years=c(1992, 1994), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_landcover_tables(tempdir = "/tmp", base_vars=c("c20", "c30"), years=c(1992, 1994), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")
# (2) Specify a subset directly by simply passing selected variables
download_landcover_tables(tempdir = "/tmp", subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"))
# (3) Specify a subset and tile_ids
download_landcover_tables(tempdir = "/tmp", subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"), tile_ids=c("h02v02", "h04v02"))
# (4) Specify a subset and tile_ids and say download=TRUE
download_landcover_tables(tempdir = "/tmp", subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_landcover_tables(tempdir = "/tmp", subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")


############
### soil ###
############

# (1) Ask for all variables
download_soil_tables(tempdir = "/tmp")
download_soil_tables(tempdir = "/tmp", subset=c("awcts", "wwp"))
download_soil_tables(tempdir = "/tmp", subset=c("awcts", "wwp"), tile_ids=c("h02v02", "h04v02"))
download_soil_tables(tempdir = "/tmp", subset=c("awcts", "wwp"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_soil_tables(tempdir = "/tmp", subset=c("awcts", "wwp"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")


#######################
### present climate ###
#######################

# (1) Ask for all variables
download_present_climate_tables(tempdir = "/tmp")
download_present_climate_tables(tempdir = "/tmp", subset=c("bio1", "bio2"))
download_present_climate_tables(tempdir = "/tmp", subset=c("bio1", "bio2"), tile_ids=c("h02v02", "h04v02"))
download_present_climate_tables(tempdir = "/tmp", subset=c("bio1", "bio2"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_present_climate_tables(tempdir = "/tmp", subset=c("bio1", "bio2"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")


######################
### hydrography90m ###
######################

# (1) Ask for all variables
download_hydrography90m_tables(tempdir = "/tmp")
download_hydrography90m_tables(tempdir = "/tmp", subset=c("flow_accum", "spi"))
download_hydrography90m_tables(tempdir = "/tmp", subset=c("flow_accum", "spi"), tile_ids=c("h02v02", "h04v02"))
download_hydrography90m_tables(tempdir = "/tmp", subset=c("flow_accum", "spi"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_hydrography90m_tables(tempdir = "/tmp", subset=c("flow_accum", "spi"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")

