

##############################################
### Env90m: How to retrieve variable names ###
###         and download data              ###
###         (usage examples)               ###
##############################################
# Merret, 2025-01-0317

# For each dataset, there is a "download_xxx_tables()" function,
# with which you can do several things:
# 
# (1) Ask for all variables: It will display all variables in a list, also split by model, scenario etc.
#     Specify a subset by specifying components such as model, scenario etc. or by passing selected variable names: It will check whether they exist and return only the existing ones
# (2) Specify a subset and tile_ids: It will compute the download size
# (3) Specify a subset and tile_ids and say download=TRUE: It will download




######################
### future climate ###
######################

# (1) Ask for all variables
download_future_climate_tables()
download_future_climate_tables(subset="ALL")
download_future_climate_tables(base_vars = "ALL", time_periods = "ALL", models="ALL", scenarios="ALL", time_periods="ALL")
# Specify a subset by specifying entire variable names
download_future_climate_tables(subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"))
# Specify a subset by specifying components such as model, scenario etc. separately
download_future_climate_tables(base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"))

# (2) Specify a subset and tile_ids, to compute download size
download_future_climate_tables(base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"), tile_ids=c("h02v02", "h04v02"))
download_future_climate_tables(subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"), tile_ids=c("h02v02", "h04v02"))

# (3) Specify a subset and tile_ids and say download=TRUE
download_future_climate_tables(base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_future_climate_tables(base_vars = c("bio1"), time_periods = c("2041-2070", "2071-2100"), models=c("ipsl-cm6a-lr"), scenarios=c("ssp126"), tile_ids=c("h02v02", "h04v02"), download=TRUE, file_format="txt", delete_zips="TRUE")
download_future_climate_tables(subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_future_climate_tables(subset=c("bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1", "bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")


##################
### land cover ###
##################

# (1) Ask for all variables
download_landcover_tables()
download_landcover_tables(subset="ALL")
download_landcover_tables(base_vars="ALL", years="ALL")
# Specify a subset by specifying entire variable names
download_landcover_tables(subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"))
# Specify a subset by specifying components such as base_vars and years separately
download_landcover_tables(base_vars=c("c20", "c30"), years=c(1992, 1994))

# (2) Specify a subset and tile_ids, to compute download size
download_landcover_tables(subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"), tile_ids=c("h02v02", "h04v02"))
download_landcover_tables(subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"), tile_ids="ALL")
download_landcover_tables(subset="ALL", tile_ids=c("h02v02", "h04v02"))
download_landcover_tables(base_vars=c("c20", "c30"), years=c(1992, 1994), tile_ids=c("h02v02", "h04v02"))
download_landcover_tables(base_vars=c("c20", "c30"), years=c(1992, 1994), tile_ids="ALL")
download_landcover_tables(base_vars="ALL", years=c(1992, 1994), tile_ids="ALL")

# (3) Specify a subset and tile_ids and say download=TRUE
download_landcover_tables(base_vars=c("c20", "c30"), years=c(1992, 1994), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_landcover_tables(base_vars=c("c20", "c30"), years=c(1992, 1994), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")
download_landcover_tables(subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_landcover_tables(subset=c("c20_1992", "c20_1994", "c30_1992", "c30_1994"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")


############
### soil ###
############

# (1) Ask for all variable names, or specify a subset
download_soil_tables()
download_soil_tables(subset="ALL")
download_soil_tables(subset=c("awcts", "wwp"))

# (2) Specify a subset and tile_ids, to compute download size
download_soil_tables(subset=c("awcts", "wwp"), tile_ids=c("h02v02", "h04v02"))
download_soil_tables(subset=c("awcts", "wwp"), tile_ids="ALL")
download_soil_tables(subset="ALL", tile_ids=c("h02v02", "h04v02"))

# (3) Specify a subset and tile_ids and say download=TRUE
download_soil_tables(subset=c("awcts", "wwp"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_soil_tables(subset=c("awcts", "wwp"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")


#######################
### present climate ###
#######################

# (1) Ask for all variable names, or specify a subset
download_present_climate_tables()
download_present_climate_tables(subset="ALL")
download_present_climate_tables(subset=c("bio1", "bio2"))

# (2) Specify a subset and tile_ids, to compute download size
download_present_climate_tables(subset=c("bio1", "bio2"), tile_ids=c("h02v02", "h04v02"))
download_present_climate_tables(subset=c("bio1", "bio2"), tile_ids="ALL")
download_present_climate_tables(subset="ALL", tile_ids=c("h02v02", "h04v02"))

# (3) Specify a subset and tile_ids and say download=TRUE
download_present_climate_tables(subset=c("bio1", "bio2"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_present_climate_tables(subset=c("bio1", "bio2"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")


######################
### hydrography90m ###
######################

# (1) Ask for all variable names, or specify a subset
download_hydrography90m_tables()
download_hydrography90m_tables(subset="ALL")
download_hydrography90m_tables(subset=c("flow_accum", "spi"))

# (2) Specify a subset and tile_ids, to compute download size
download_hydrography90m_tables(, subset=c("flow_accum", "spi"), tile_ids=c("h02v02", "h04v02"))
download_hydrography90m_tables(, subset=c("flow_accum", "spi"), tile_ids="ALL")
download_hydrography90m_tables(, subset="ALL", tile_ids=c("h02v02", "h04v02"))

# (3) Specify a subset and tile_ids and say download=TRUE
download_hydrography90m_tables(, subset=c("flow_accum", "spi"), tile_ids=c("h02v02", "h04v02"), download=TRUE)
download_hydrography90m_tables(, subset=c("flow_accum", "spi"), tile_ids=c("h02v02", "h04v02"), download=TRUE, download_dir=".", file_format="txt", delete_zips="TRUE")

