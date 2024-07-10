#' @title Download files of the Environment90m dataset
#'
#' @description The function downloads data of the Environment90m
#' dataset, which is split into 20°x20° tiles. If a tile ID is specified, then
#' the selected layers (variable) will be downloaded. 
#' Multiple regular tiles, e.g. belonging to regional units, can be
#' downloaded in a single request. The tile or regional unit IDs can be
#' obtained using the functions "get_tile_id" and "get_regional_unit_id",
#' respectively.
#' The files will be stored locally in a folder architecture, similar as in the
#' data repository, available at
#' \url{https://public.igb-berlin.de/index.php/s/agciopgzXjWswF4?path=%2F}.
#'
#' @param variable character vector of variable names. See Details for all the
#' variable names.
#' @param years character vector of years. Only valid for ESA Land Cover variables,
#' ignored otherwise.
#' @param file_format character. Format of the requested file. "zip" and "txt"
#' are supported. Default is "txt", which means that the zip files are unzipped
#' after downloading. Note that this will take more space on disk than zips.
#' @param tile_id character vector. The IDs of the requested tiles.
#' @param download_dir character. The directory where the files will be
#' downloaded. Default is the working directory.
#' @param delete_zips boolean If FALSE, the downloaded zip files are not deleted
#' after unzipping. Defaults to TRUE. This is ignored if you request file format zip.
#' @importFrom tidyr separate
#' @importFrom stringr str_split_fixed str_extract
#' @importFrom data.table fread
#' @export
#'
#' @author Afroditi Grigoropoulou, Merret Buurman
#'
#' @references Amatulli G., Garcia Marquez J., Sethi T., Kiesel J.,
#' Grigoropoulou A., Üblacker M., Shen L. & Domisch S. (2022-08-09 )
#' Hydrography90m: A new high-resolution global hydrographic dataset.
#' IGB Leibniz-Institute of Freshwater Ecology and Inland Fisheries.
#' dataset. \url{https://doi.org/10.18728/igb-fred-762.1}
#'
#' @details
#' In the following table you can find all the variables included in the
#' Environment90m dataset. The column "Variable" includes the variable names
#' that should be used as an input in the parameter "variable" of the function.
#' Likewise, the column "File format" contains the input that should be given to
#' the "file_format" parameter.
#'
#' For more details and visualisations of the spatial layers (e.g. the available
#' tiles), please refer to
#' \url{https://hydrography.org/hydrography90m/hydrography90m_layers/}.
#' For details on the bioclimatic variables, especially for details of the scale
#' and unit of the values, please refer to
#' \url{http://chelsa-climate.org/}.
#' For details on the ESA Land Cover variables, please refer to
#' \url{https://www.climatologylab.org/terraclimate.html}. Please note that some
#' values in this dataset are aggregated from similar classes (see table below,
#' see Environment90m publication).
#' For details on the Soil data, please refer to
#' \url{https://soilgrids.org}.
#'
#' TODO: The CHELSA units also have scale and offset! Explain them here?
#' TODO: The ESA LandCover data are aggregated. Document!
#'
#'  | **Variable type**                 | **Variable name**            | **Variable** | **Unit**  | **File format** |
#'  |-----------------------------------|-------------------------------------|-------|-----------------|-----------|
#'  | Bioclimatic variables (present)   | Annual Mean Temperature             | bio1  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Mean Diurnal Range                  | bio2  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Isothermality                       | bio3  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Temperature Seasonality             | bio4  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Max Temperature of Warmest Month    | bio5  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Min Temperature of Coldest Month    | bio6  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Temperature Annual Range            | bio7  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Mean Temperature of Wettest Quarter | bio8  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Mean Temperature of Driest Quarter  | bio9  | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Mean Temperature of Warmest Quarter | bio10 | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Mean Temperature of Coldest Quarter | bio11 | degrees Celsius | zip / txt |
#'  | Bioclimatic variables (present)   | Annual Precipitation                | bio12 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (present)   | Precipitation of Wettest Month      | bio13 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (present)   | Precipitation of Driest Month       | bio14 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (present)   | Precipitation Seasonality           | bio15 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (present)   | Precipitation of Wettest Quarter    | bio16 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (present)   | Precipitation of Driest Quarter     | bio17 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (present)   | Precipitation of Warmest Quarter    | bio18 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (present)   | Precipitation of Coldest Quarter    | bio19 | kg/m^2          | zip / txt |
#'  | Bioclimatic variables (2071-2100) | Same as above                       |       |                 | zip / txt |
#'  | ESA Land Cover (1992-2018) | Cropland, rainfed (aggregated from original classes 10,11,12)         | c10_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Cropland, irrigated or post-flooding                       | c20_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%) | c30_<year> | % | zip / txt |
#'  | ESA Land Cover (1992-2018) | Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%) | c40_<year> | % | zip / txt |
#'  | ESA Land Cover (1992-2018) | Tree cover, broadleaved, evergreen, closed to open (>15%)  | c50_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Tree cover, broadleaved, deciduous, closed to open (>15%) (aggregated from original classes 60, 61, 62)  | c60_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Tree cover, needleleaved, evergreen, closed to open (>15%) (aggregated from original classes 70, 71, 72) | c70_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Tree cover, needleleaved, deciduous, closed to open (>15%) (aggregated from original classes 80, 81, 82) | c80_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Tree cover, mixed leaf type (broadleaved and needleleaved) | c90_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Mosaic tree and shrub (>50%) / herbaceous cover (<50%)    | c100_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Mosaic herbaceous cover (>50%) / tree and shrub (<50%)    | c110_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Shrubland (aggregated from original classes 120, 121, 122) | c120_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Grassland                                                 | c130_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Lichens and mosses                                        | c140_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Sparse vegetation (tree, shrub, herbaceous cover) (<15%) (aggregated from original classes 150, 151, 152, 153) | c150_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Tree cover, flooded, fresh or brackish water              | c160_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Tree cover, flooded, saline water                         | c170_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Shrub or herbaceous cover, flooded, fresh/saline/brackish water | c180_<year> | % | zip / txt |
#'  | ESA Land Cover (1992-2018) | Urban areas (aggregated from original classes 200, 201, 202) | c190_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Bare areas                                                | c200_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Water bodies                                              | c210_<year> | % | zip / txt  |
#'  | ESA Land Cover (1992-2018) | Permanent snow and ice                                    | c220_<year> | % | zip / txt  |
#'  | Soil (Global gridded soil information) | Depth to bedrock (R horizon) up to 200cm      | soil_BDRICM | cm | zip / txt |
#'  | Soil (Global gridded soil information) | Grade of a sub-soil being acid                | soil_ACDWRB | pH | zip / txt |
#'  | Soil (Global gridded soil information) | Derived saturated water content               | soil_AWCTS  |    | zip / txt |
#'  | Soil (Global gridded soil information) | Clay content                                  | soil_CLYPPT | %  | zip / txt |
#'  | Soil (Global gridded soil information) | Sand content                                  | soil_SNDPPT | %  | zip / txt |
#'  | Soil (Global gridded soil information) | Silt content                                  | soil_SLTPPT | %  | zip / txt |
#'  | Soil (Global gridded soil information) | Derived available soil water capacity         | soil_WWP    |    | zip / txt |
#'  | Soil (Global gridded soil information) | Texture class (USDA system)                   | soil_TEXMHT |    | zip / txt |
#'  | Soil (Global gridded soil information) | Soil organic carbon content (fine earth fraction) | soil_ORCDRC | g/kg | zip / txt |
#'  | Soil (Global gridded soil information) | Soil pH x 10 in H2O                          | soil_PHIHOX  | pH  | zip / txt |
#'  | Soil (Global gridded soil information) | Bulk density (fine earth)                    | soil_BLDFIE  | kg / cubic-meter | zip / txt |
#'  | Soil (Global gridded soil information) | Cation exchange capacity                     | soil_CECSOL  | cmolc/kg | zip / txt |
#'  | Soil (Global gridded soil information) | Coarse fragments volumetric                  | soil_CRFVOL  | %    | zip / txt |
#'  | Soil (Global gridded soil information) | Grade of a sub-soil being acid e.g. having a pH < 5 and low BS | soil_ACDWRB  | ph | zip / txt |
#'  | Soil (Global gridded soil information) | Probability of occurence of R horizon (0-100%) | soil_BDRLOG  | %  | zip / txt |
#'  | Soil (Global gridded soil information) | Cummulative probability of organic soil based on the TAXOUSDA and TAXNWRB | soil_HISTPR | | zip / txt |
#'  | Soil (Global gridded soil information) | Sodic soil grade based on WRB soil types and soil pH | soil_SLGWRB  | pH  | zip / txt |
#'
#' @md
#' @note
#' If there is an error during the download of a file
#' (more likely in case of files bigger than 3-4GB), you can try to manually
#' download this file by pasting the link that is returned by the error
#' message in your browser.
#'
#' @examples
#' # Download data for two variables in three regular tiles
#' # to the current working directory
#' download_env(variable = c("bio1", "bio2"),
#'                tile_id = c("h00v02", "h16v02", "h16v04"))
#'
#' # Download them as zip, to save disk space:
#' download_env(variable = c("bio1", "bio2"),
#'                file_format = "zip",
#'                tile_id = c("h00v02", "h16v02", "h16v04"))
#'
#' # Download land cover data for two years
#' download_env(variable = c("c20", "c30"), years = c(1992, 1996),
#'                file_format = "zip",
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#
#' # Download land cover data for specific variables and  years
#' download_env(variable = c("c20_1996", "c30_1992"),
#'                file_format = "zip",
#'                tile_id = c("h00v02", "h16v02", "h16v04"))

### TODO:
# Make the file with file sizes
# Examples above!
# Regional Unit Ids?


download_env <- function(variable, file_format = "txt", years = NULL,
                         tile_id = NULL,
                         download_dir = ".", delete_zips = TRUE) {

  # Introductory steps

  # Set timeout option for download to 4 hours (14400 seconds)
  options(timeout=14400)

  # Download lookup table with the size of each file
  # if it doesn't exist in the tempdir()
  file_size_file <- paste0(tempdir(), "/environment90m_paths_file_sizes.txt")
  if (!file.exists(file_size_file)) {
    message('Downloading environment90m_paths_file_sizes.txt...')
    download.file("https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/environment90m_paths_file_sizes.txt",
                  destfile = file_size_file, mode = "wb")
  }

  # Import lookup table with the size of each file
  file_size_table <- data.table::fread(file_size_file, sep = ";")
  file_size_table$file_name = basename(file_size_table$file_path)

  # Extract entire lists of possible variable names, 
  # file formats and tile_ids,
  # to check that the requested variable exists

  # Get the valid names of the environment90m variables
  # Expecting tile_id at the end: <variable>_<tile_id>
  all_varnames <- sort(unique(sub("_[^_]+$", "",
                                    file_size_table$file_name)))
  #message(paste('Available variables:', paste(all_varnames, collapse=', ')))


  ### If the user requests years: ###
  if (length(years) > 0) {

    # Which years are requested:
    if (!(all(grepl("[[:digit:]]{4}", years)))) { # matches 0000
      which_valid <- grepl("[[:digit:]]{4}", years)
      message(paste('Years are not valid:', paste(years[!which_valid], collapse=', ', '(ignoring)')))
      if (length(years[which_valid]) == 0){
        message('No valid years passed! All invalid:', years)
      }
      years <- years[which_valid]
    }

    # Are those years available?
    # TODO: Hardcoding available years would be more efficient, but less flexible in case we
    # add more years!
    #regex <- "^c[[:digit:]]+_[[:digit:]]{4}_h[[:digit:]]{2}v[[:digit:]]{2}" # matches  
    regex <- "^c[[:digit:]]+_[[:digit:]]{4}"   # matches c00_0000
    available_landcover_varnames <- all_varnames[grepl(regex, all_varnames)]
    available_years <- unique(gsub("^c[[:digit:]]+_", "", available_landcover_varnames))
    years_ok <- years %in% available_years
    requested_years <- years[years_ok]
    if (!(all(years_ok))){
      message(paste('Requested years not available:', paste(years[!years_ok], collapse=', ', '(ignoring)')))
      message(paste('Keeping years:', paste(requested_years, collapse=', ')))
    }

    # Which requested variables can be combined with years, i.e. match c10, c20 or similar:
    regex <- "^c[[:digit:]]+$" # matches c00
    which_are_landcover <- grepl(regex, variable)
    requested_landcover_vars = variable[which_are_landcover]
    if (any(which_are_landcover)) {
      message(paste('These variables can be combined with years:', paste(requested_landcover_vars, collapse=', ')))
    } else {
      message(paste('No variables can be combined with years:', paste(variable[!which_are_landcover], collapse=', ')))
    }

    # Now merge them all:
    landcover_with_years <- c()
    for (ivar in requested_landcover_vars) {
      tmp <- paste0(ivar, '_', requested_years)
      landcover_with_years <- c(landcover_with_years, tmp)
    }

    # And modify vector "variable":
    # Remove the land cover vars without years:
    variable_no_landcover <- variable[!variable %in% requested_landcover_vars]
    # Add the land cover vars with years:
    variable <- unique(c(variable_no_landcover, landcover_with_years))
    message(paste('These variables will be downloaded:', paste(variable, collapse=', ')))
  }

  # Piece of code to avoid trying to download files that are not (yet) there!
  # As of 9 July 2024, only c10 and c20 work, the rest still has to be renamed. TODO.
  available_c10 <- paste0('c10_',
    c('1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001',
      '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011',
      '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
  available_c20 <- paste0('c20_',
    c('1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001',
      '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011',
      '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
  currently_available <- c(available_c10, available_c20)
  which_ok <- variable %in% currently_available
  if (!(all(which_ok))) {
    message(paste('Sorry, currently not available yet (work in progress):',
      paste0(variable[!which_ok], collapse=', '), '(ignoring)'))
    variable <- variable[which_ok]
    if (length(variable) == 0){
      stop('No variables left to download...')
    }
  }


  # Get the valid file_names of the environment90m variables
  all_file_names <- sort(unique(file_size_table$file_name))

  # Get the valid tile ids of the environment90m
  all_tile_ids <- unique(stringr::str_extract(
    file_size_table$file_path, "h[0-9]+v[0-9]+"))

  # Remove NA from list of tile ids:
  all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]
  #message(paste('Available tiles:', paste(all_tile_ids, collapse=', ')))

  # Compute overall size of download, by iterating
  # over each variable and there over each tile:
  variable_size_sum <- 0

  for (ivar in variable) {

    tile_size_sum <- 0

    for (itile in tile_id) {

      tile_size <- check_tiles_filesize(variable = ivar,
                                        file_format = "zip",
                                        tile_id = itile,
                                        h90m_varnames = all_varnames,
                                        h90m_tile_id = all_tile_ids,
                                        h90m_file_names = all_file_names,
                                        file_size_table = file_size_table)

      #message(paste0("Variable '",ivar,"', tile '",itile, "': Size: ", tile_size, " bytes."))
      if (length(tile_size) == 0) {
        warning(paste0("Error: Tile '", itile, "', of variable '", ivar, "' not available!"))
        tile_size <- 0 # Will be skipped during download later!
      }

      tile_size_sum <- tile_size_sum + tile_size

    }

    variable_size_sum <- tile_size_sum + variable_size_sum
  }

  variable_size_sum

  # Print warning on file size and ask for input from the user
  # arg <- readline(prompt = paste0("Download size is ",
  #                                 round(variable_size_sum / 1000000, 2),
  #                                 " MB. Please type \"y\" if you are ready to smash it\n
  #                                 or \"n\" if you'd rather not to, and then press Enter \n"))
  print(paste0("Download size is ",
         round(variable_size_sum / 1000000, 2),
         " MB."))

  # General path to the download folder at IGB
  # (GDrive is not available for environment90m data)
  igb_path <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2F"

  # Use README file as a test to check if Nimbus is up.
  tryCatch(
    {
      igb_readme = paste0(igb_path, "README/README.txt")
      download.file(igb_readme, destfile = paste0(download_dir, "/README.txt"), mode = "wb")
    },
    warning = function(c) {
      message(paste0('Error: Could not download README.txt from ', igb_readme, ', maybe the server is down.'))
    },
    error = function(c) {
      message(paste0('Error: Could not download README.txt from ', igb_readme, ', maybe the server is down.'))
    }
  )

  all_downloaded_zips = c()
  for (ivar in variable) {
    for (itile in tile_id) {

      message("Downloading variable ", ivar, " for tile ", itile, "...")

      downloaded_path <- download_tiles_base(variable = ivar, file_format = "zip",
                          tile_id = itile, global = FALSE,
                          download_dir = download_dir,
                          file_size_table = file_size_table,
                          server_url = igb_path
      )
      all_downloaded_zips <- c(all_downloaded_zips, downloaded_path)
    }
  }

  message(paste('Downloaded zip files:', paste(all_downloaded_zips, collapse=', ')))


  # Unzip and delete zipfiles, if requested...
  # TODO: This could start to run in parallel, maybe?
  if (file_format == "txt") {
    if (delete_zips) {
      message("Unzipping and deleting zipfiles...")
    } else {
      message("Unzipping...")
    }
    for (zipfile in all_downloaded_zips) {
      destdir <- dirname(zipfile)
      message("Unzipping file ", zipfile, "...")
      utils::unzip(zipfile, exdir = destdir)
      if (delete_zips) {
        file.remove(zipfile)
      }
    }
  }

  cat("Please cite the Environment90m publication:\n
      Garcia Marquez, J., Amatulli, G., Grigoropoulou, A.,
      Schürz, M., Tomiczek, T., Buurman, M., and Domisch, S.:
      Global datasets of aggregated environmental variables at the
      sub-cachment scale for freshwater biodiversity modeling, in prep.
      Please contact the authors for more up-to-date citation info.")
  # TODO: Adapt citation!
}
