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
#' @param file_format character. Format of the requested file. "zip" and "csv"
#' are supported. Default is "csv", which means that the zip files are unzipped
#' after downloading. Note that this will take more space on disk than zips.
#' @param tile_id character vector. The IDs of the requested tiles.
#' @param download_dir character. The directory where the files will be
#' downloaded. Default is the working directory.
#' @param delete_zips boolean If FALSE, the downloaded zip files are not deleted
#' after unzipping. Defaults to TRUE. This is ignored if you request file format zip.
#' @importFrom tidyr separate
#' @importFrom stringr str_split_fixed str_extract
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
#' For more details and visualisations of the spatial layers, please refer to TODO
#' \url{https://hydrography.org/hydrography90m/hydrography90m_layers/}.
#' For details on the bioclimatic variables, please refer to 
#' \url{https://www.worldclim.org/data/bioclim.html} (TODO correct?).
#' For details on the ESA Land Cover variables, please refer to 
#' \url{https://dunno.com} (TODO add!!).
#'
#' TODO: The units also have scale and offset! What about them?
#' TODO: The c10 etc. have numbers behind their descriptions, what do they say?
#'
#'  | **Variable type**                 | **Variable name**            | **Variable** | **Unit**  | **File format** |
#'  |-----------------------------------|-------------------------------------|-------|-----------------|-----------|
#'  | Bioclimatic variables (present)   | Annual Mean Temperature             | bio1  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Mean Diurnal Range                  | bio2  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Isothermality                       | bio3  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Temperature Seasonality             | bio4  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Max Temperature of Warmest Month    | bio5  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Min Temperature of Coldest Month    | bio6  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Temperature Annual Range            | bio7  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Mean Temperature of Wettest Quarter | bio8  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Mean Temperature of Driest Quarter  | bio9  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Mean Temperature of Warmest Quarter | bio10 | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Mean Temperature of Coldest Quarter | bio11 | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (present)   | Annual Precipitation                | bio12 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (present)   | Precipitation of Wettest Month      | bio13 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (present)   | Precipitation of Driest Month       | bio14 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (present)   | Precipitation Seasonality           | bio15 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (present)   | Precipitation of Wettest Quarter    | bio16 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (present)   | Precipitation of Driest Quarter     | bio17 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (present)   | Precipitation of Warmest Quarter    | bio18 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (present)   | Precipitation of Coldest Quarter    | bio19 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100) | Same as above                       |       |                 | zip / csv |
#'  | ESA Land Cover (1992-2018) | Cropland, rainfed                                          | c10_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Cropland, irrigated or post-flooding                       | c20_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%) | c30_<year> | % | zip / csv |
#'  | ESA Land Cover (1992-2018) | Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%) | c40_<year> | % | zip / csv |
#'  | ESA Land Cover (1992-2018) | Tree cover, broadleaved, evergreen, closed to open (>15%)  | c50_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Tree cover, broadleaved, deciduous, closed to open (>15%)  | c60_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Tree cover, needleleaved, evergreen, closed to open (>15%) | c70_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Tree cover, needleleaved, deciduous, closed to open (>15%) | c80_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Tree cover, mixed leaf type (broadleaved and needleleaved) | c90_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Mosaic tree and shrub (>50%) / herbaceous cover (<50%)    | c100_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Mosaic herbaceous cover (>50%) / tree and shrub (<50%)    | c110_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Shrubland                                                 | c120_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Grassland                                                 | c130_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Lichens and mosses                                        | c140_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Sparse vegetation (tree, shrub, herbaceous cover) (<15%)  | c150_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Tree cover, flooded, fresh or brackish water              | c160_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Tree cover, flooded, saline water                         | c170_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Shrub or herbaceous cover, flooded, fresh/saline/brackish water | c180_<year> | % | zip / csv |
#'  | ESA Land Cover (1992-2018) | Urban areas                                               | c190_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Bare areas                                                | c200_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Water bodies                                              | c210_<year> | % | zip / csv  |
#'  | ESA Land Cover (1992-2018) | Permanent snow and ice                                    | c220_<year> | % | zip / csv  |
#'  | Soil                       | Depth to bedrock (R horizon) up to 200cm                  | soil_BDRICM | cm | zip / csv |
#'  | Soil                       | Grade of a sub-soil being acid                            | soil_ACDWRB | pH | zip / csv |
#'
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
#' download_env(variable = c("bio1", "bio1"),
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#'
#' # Download them as zip, to save disk space:
#' download_env(variable = c("bio1", "bio1"),
#'                file_format = "zip",
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#'
#' # Download land cover data for two years
#' download_env(variable = c("c20", "c30"), years = c(1992, 1996)
#'                file_format = "zip",
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#
#' # Download land cover data for specific variables and  years
#variable = c("bio1")
#scenario = c("ssp370")
#model = c("iiipsl-cm6a-lr")
#time_period = c("2071-2100")
#download_future_climate(variable = variable, file_format = "zip",
#                time_period = time_period, scenario = scenario,
#                model = model,
#                tile_id = c("h00v02","h16v02", "h16v04"))

### TODO:
# Make the file with file sizes
# Examples above!
# Regional Unit Ids?
# bio1_2071-2100_ipsl_cm6a-lr_ssp370_v2.1_h00v00.zip




download_future_climate <- function(variable, file_format = "csv",
                         time_period = c("2071-2100"), scenario = NULL,
                         model = NULL, tile_id = NULL,
                         download_dir = ".", delete_zips = TRUE) {

  # Introductory steps

  # Set timeout option for download to 4 hours (14400 seconds)
  options(timeout=14400)

  # Download lookup table with the size of each file
  # if it doesn't exist in the tempdir()
  file_size_file <- paste0(tempdir(), "/futureclimate90m_paths_file_sizes.txt")
  if (!file.exists(file_size_file)) {
    message('Downloading futureclimate90m_paths_file_sizes.txt...')
    download.file("https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/futureclimate90m_paths_file_sizes.txt",
                  destfile = file_size_file, mode = "wb")
  }

  # Import lookup table with the size of each file
  file_size_table <- fread(file_size_file, sep = ";")
  file_size_table$file_name = basename(file_size_table$file_path)

  # Extract entire lists of possible variable names, 
  # file formats and tile_ids,
  # to check that the requested variable exists

  # Get the valid names of the environment90m variables
  # Expecting tile_id at the end: <variable>_<tile_id>
  all_varnames <- sort(unique(sub("_[^_]+$", "",
                                    file_size_table$file_name)))
  #message(paste('Available variables:', paste(all_varnames, collapse=', ')))

  ### Check if base variable is available
  all_base_vars = c("bio1", "bio2", "bio8", "bio9") # TODO not hardcode!
  which_ok <- variable %in% all_base_vars
  if (!(all(which_ok))) {
    message(paste('Variable not available:', paste(variable[!which_ok], ' (ignoring)', collapse=', ')))
    message(paste('All available variables:', paste(all_base_vars, collapse=', ')))
    variable <- variable[which_ok]
    if (length(variable) == 0) {
      stop('No valid variable requested!')
    }
    message(paste('Requesting these variables:', paste(variable, collapse=', ')))
  }

  ### Check if time_period is available
  all_periods = c("2071-2100") # TODO not hardcode!
  which_ok <- time_period %in% all_periods
  if (!(all(which_ok))) {
    message(paste('Time period not available:', paste(time_period[!which_ok], ' (ignoring)', collapse=', ')))
    message(paste('All available time periods:', paste(all_periods, collapse=', ')))
    time_period <- time_period[which_ok]
    if (length(time_period) == 0) {
      stop('No valid time period requested!')
    }
    message(paste('Requesting these time periods:', paste(time_period, collapse=', ')))
  }

  ### Check if model is available
  all_models = c("ipsl-cm6a-lr", "ukesm1-0-ll", "mpi-esm1-2-hr") # TODO not hardcode!
  which_ok <- model %in% all_models
  if (!(all(which_ok))) {
    message(paste('Model not available:', paste(model[!which_ok], ' (ignoring)', collapse=', ')))
    message(paste('All available models:', paste(all_models, collapse=', ')))
    model <- model[which_ok]
    if (length(model) == 0) {
      stop('No valid model requested!')
    }
    message(paste('Requesting these models:', paste(model, collapse=', ')))
  }

  ### Check if scenario is available
  all_scenarios = c("ssp370", "ssp585") # TODO not hardcode!
  which_ok <- scenario %in% all_scenarios
  if (!(all(which_ok))) {
    message(paste('Scenario not available:', paste(scenario[!which_ok], ' (ignoring)', collapse=', ')))
    message(paste('All available scenarios:', paste(all_scenarios, collapse=', ')))
    scenario <- scenario[which_ok]
    if (length(scenario) == 0) {
      stop('No valid scenario requested!')
    }
    message(paste('Requesting these scenarios:', paste(scenario, collapse=', ')))
  }

  # Combine them into variable names, the way they are stored:
  # bio18_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1_h34v06.zip
  # Nested loops, probably not very efficient. Also: Predefine array length!
  entire_name = c()
  for (ivar in variable) {
    for (iperiod in time_period) {
      for (imodel in model){
        for (iscenario in scenario){
          tmp <- paste0(ivar, '_', iperiod, '_', imodel, '_', iscenario, '_V.2.1')
            entire_name = c(entire_name, tmp)
        }
      }
    }
  }
  message(paste('Requested: ', paste(entire_name, collapse=', ')))

  # Get the valid file_names of the environment90m variables
  all_file_names <- sort(unique(file_size_table$file_name))
  #message(paste('Requested all_file_names: ', paste(all_file_names, collapse=', ')))

  # Get the valid tile ids of the environment90m
  all_tile_ids <- unique(str_extract(
    file_size_table$file_path, "h[0-9]+v[0-9]+"))

  # Remove NA from list of tile ids:
  all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]
  #message(paste('Available tiles:', paste(all_tile_ids, collapse=', ')))

  # Compute overall size of download, by iterating
  # over each variable and there over each tile:
  variable_size_sum <- 0

  for (ivar in entire_name) {

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
  for (ivar in entire_name) {
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
  if (file_format == "csv") {
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

  cat("Please cite the Hydrography90m publication:\n
      Amatulli, G., Garcia Marquez, J., Sethi, T., Kiesel, J., Grigoropoulou, A.,
      Üblacker, M. M., Shen, L. Q., and Domisch, S.: Hydrography90m: a new
      high-resolution global hydrographic dataset, Earth Syst. Sci. Data, 14,
      4525–4550, https://doi.org/10.5194/essd-14-4525-2022, 2022.")
  # TODO: Adapt citation!
}
