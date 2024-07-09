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
#' variable names. No default, has to be provided.
#' @param scenario character vector of scenarios, e.g. "ssp370", "ssp585". See
#' Details for all the scenario names. No default, has to be provided.
#' @param model character vector of models, e.g. "mpi-esm1-2-hr", "ukesm1-0-ll",
#' "ipsl-cm6a-lr". See Details for all the scenario names. No default, has to be
#' provided.
#' @param file_format character. Format of the requested file. "zip" and "csv"
#' are supported. Default is "csv", which means that the zip files are unzipped
#' after downloading. Note that this will take more space on disk than zips.
#' @param tile_id character vector. The IDs of the requested tiles.
#' @param time_period character vector of time periods. Default: "2071-2100".
#' Currently only "2071-2100" is available, so this can be omitted. 
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
#'
#' TODO: The units also have scale and offset! What about them?
#'
#'  | **Variable type**                 | **Variable name**            | **Variable**   | **Unit**  | **File format** |
#'  |-----------------------------------|------------------------------|----------------|-----------------|-----------|
#'  | Bioclimatic variables (2071-2100)   | Annual Mean Temperature             | bio1  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Mean Diurnal Range                  | bio2  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Isothermality                       | bio3  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Temperature Seasonality             | bio4  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Max Temperature of Warmest Month    | bio5  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Min Temperature of Coldest Month    | bio6  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Temperature Annual Range            | bio7  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Mean Temperature of Wettest Quarter | bio8  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Mean Temperature of Driest Quarter  | bio9  | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Mean Temperature of Warmest Quarter | bio10 | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Mean Temperature of Coldest Quarter | bio11 | degrees Celsius | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Annual Precipitation                | bio12 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Precipitation of Wettest Month      | bio13 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Precipitation of Driest Month       | bio14 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Precipitation Seasonality           | bio15 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Precipitation of Wettest Quarter    | bio16 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Precipitation of Driest Quarter     | bio17 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Precipitation of Warmest Quarter    | bio18 | kg/m^2          | zip / csv |
#'  | Bioclimatic variables (2071-2100)   | Precipitation of Coldest Quarter    | bio19 | kg/m^2          | zip / csv |
#'
#'  | **Scenario name** | **Scenario description**   |
#'  | ssp370            | TODO: Scenario description |
#'  | ssp585            | TODO: Scenario description |
#'
#'  | **Model name** | **Scenario description** |
#'  | mpi-esm1-2-hr  | TODO: Model description  |
#'  | psl-cm6a-lr    | TODO: Model description  |
#'  | ukesm1-0-ll    | TODO: MOdel description  |
#'
#' @md
#' @note
#' If there is an error during the download of a file
#' (more likely in case of files bigger than 3-4GB), you can try to manually
#' download this file by pasting the link that is returned by the error
#' message in your browser.
#'
#' @examples
#' # Download bioclimatic variables for specific variables, scenarios, models:
#' variable = c("bio1")
#' scenario = c("ssp370")
#' model = c("ipsl-cm6a-lr")
#' download_future_climate(variable = variable, file_format = "zip",
#'                scenario = scenario, model = model,
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#'
#' # Eventually, other time periods may be added:
#' time_period = c("2071-2100", ...)
#' download_future_climate(variable = variable, file_format = "zip",
#'                time_period = time_period,
#'                scenario = scenario, model = model,
#'                tile_id = c("h00v02","h16v02", "h16v04"))
#'
#' # Download many bioclimatic variables:
#' variable = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8",
#'              "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15",
#'              "bio16", "bio17", "bio18", "bio19")
#' scenario = c("ssp370", "ssp585")
#' model = c("ipsl-cm6a-lr", "mpi-esm1-2-hr", "ukesm1-0-ll")
#' download_future_climate(variable = variable, file_format = "zip",
#'                scenario = scenario, model = model,
#'                tile_id = c("h00v02", "h16v02", "h16v04"))
#'


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
