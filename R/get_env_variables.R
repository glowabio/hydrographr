#' @title Get available variables
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
#' @param ignore_missing boolean What to do if some of the variables are not available,
#' which is most frequently caused by a typo the variable name. If TRUE, the missing or
#' misspelled ones are ignored while the others are downloaded. If FALSE, the function
#' will fail to allow the user to check the variable names and their spelling. Defaults
#' to FALSE.
#' @param tempdir character Optional (rarely needed): Pass a directory to be used as
#' temporary directory. If not provided, the result of a call to tempdir() is used.
#' @importFrom tidyr separate
#' @importFrom stringr str_split_fixed str_extract
#' @importFrom data.table fread
#' @export
#'
#' @author Merret Buurman
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




#####################
### Table getters ###
#####################

get_file_size_table <- function(file_name, tempdir = NULL) {

  # Define tempdir:
  if (is.null(tempdir)) {
    tempdir <- tempdir()
  }

  # Download lookup table with the size of each file
  # if it doesn't exist in the tempdir()
  file_local <- paste0(tempdir, "/", file_name)
  if (!file.exists(file_local)) {
    message('Downloading futureclimate90m_paths_file_sizes.txt...')
    url <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/"
    download.file(paste0(url, file_name), destfile = file_local, mode = "wb")
  }

  # Import lookup table with the size of each file
  file_size_table <- data.table::fread(file_local, sep = ";")
  file_size_table$file_name = basename(file_size_table$file_path)

  return (file_size_table)
}

get_present_climate_variable_table <- function(tempdir = NULL) {
  file_name <- "env90m_presentclimate_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(file_name, tempdir)
  return (file_size_table)
}

get_future_climate_variable_table <- function(tempdir = NULL) {
  file_name <- "env90m_futureclimate_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(file_name, tempdir)
  return (file_size_table)
}

get_hydrography90m_variable_table <- function(tempdir = NULL) {
  file_name <- "env90m_hydro_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(file_name, tempdir)
  return (file_size_table)
}

get_landcover_variable_table <- function(tempdir = NULL) {
  file_name <- "env90m_landcover_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(file_name, tempdir)
  return (file_size_table)
}

get_soil_variable_table <- function(tempdir = NULL) {
  file_name <- "env90m_soil_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(file_name, tempdir)
  return (file_size_table)
}

########################
### Variable getters ###
########################

get_future_climate_variables <- function(separated = TRUE, tempdir = NULL, 
                                         quiet = FALSE, file_size_table = NULL,
                                         base_vars = NULL, time_periods = NULL, 
                                         scenarios = NULL, models = NULL,
                                         versions = NULL) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_future_climate_variable_table(tempdir = tempdir)
  }

  # Info:
  # Future climate variables look like this:
  # <base_var>_<time_period>_<model>_<scenario>_<version>_<tile_id>.zip
  # "bio9_2071-2100_ukesm1-0-ll_ssp585_V.2.1_h32v00.zip"

  # Should we return everything, or restrict by scenario/model/time period?
  if (is.null(time_periods) && is.null(scenarios) && is.null(models) && is.null(base_vars) && is.null(versions)){
    return_all <- TRUE
    if (!(quiet)) message('Will return all variable names...')
  } else {
    return_all <- FALSE
    if (!(quiet)) message('Will return requested subset of variable names...')
  }

  ###########################################
  ## Extract all available variable names ###
  ###########################################

  regex_format <- ".[a-z]+$"
  #all_formats <- unique(stringr::str_extract(file_size_table$file_name, regex_format))
  #all_formats <- unique(sub("^.", "", all_formats))
  filenames_without_format <- unique(sub(regex_format, "", file_size_table$file_name))
  
  regex_tile <- "_h[0-9]+v[0-9]+$"
  #all_tiles <- unique(stringr::str_extract(filenames_without_format, regex_tile))
  #all_tiles <- unique(sub("^_", "", all_tiles))
  filenames_without_tile <- unique(sub(regex_tile, "", filenames_without_format))
  all_varnames <- filenames_without_tile

  # If we don't want them separately, return them now:
  if (return_all) {
    if (!(separated)) {
      return (all_varnames)
    }
  }

  ###########################################
  ## Split variable names into components ###
  ###########################################

  # Extract version from end of string 
  regex_version <- "_[^_]+$"  
  all_versions <- unique(stringr::str_extract(filenames_without_tile, regex_version))
  all_versions <- sub("^_", "", all_versions)
  filenames_without_version <- unique(sub(regex_version, "", filenames_without_tile))

  # Extract scenario from end of string 
  regex_scenario <- "_[^_]+$"  
  all_scenarios <- unique(stringr::str_extract(filenames_without_version, regex_scenario))
  all_scenarios <- sub("^_", "", all_scenarios)
  filenames_without_scenario <- unique(sub(regex_scenario, "", filenames_without_version))

  # Extract model from end of string 
  regex_model <- "_[^_]+$"  
  all_models <- unique(stringr::str_extract(filenames_without_scenario, regex_model))
  all_models <- sub("^_", "", all_models)
  filenames_without_model <- unique(sub(regex_model, "", filenames_without_scenario))

  # Extract period from end of string 
  #regex_period <- "[0-9]{4}-[0-9]{4}$"
  regex_period <- "_[^_]+$"  
  all_periods <- unique(stringr::str_extract(filenames_without_model, regex_period))
  all_periods <- sub("^_", "", all_periods)

  # Extract base vars from beginning of string and period from end
  regex_base <- "^[^_]+_"
  all_base_vars <- unique(stringr::str_extract(file_size_table$file_name, regex_base))
  all_base_vars <- sub("_$", "", all_base_vars)

  # Return list of splitted variables:
  if (return_all) {
    if (separated) {
      variables <- list(
        time_periods = sort(all_periods),
        scenarios = sort(all_scenarios),
        models = sort(all_models),
        base_vars = sort(all_base_vars),
        versions = sort(all_versions)
      )
      return(variables)
    }
  }

  ############################################
  ## Restrict to user-requested components ###
  ############################################

  # Restrict scenarios to what the user requested:
  if (is.null(models)) {
    # If user did not request scenarios years, return all available scenarios:
    returned_models <- all_models
  } else {
    # If user requested specific scenarios, return requested available scenarios:
    returned_models <- models[models %in% all_models]
    if ((!(quiet)) && (!(all(models %in% all_models))))
      message('Not available: ', paste(models[!models %in% all_models], collapse=', '))
  }

  # Restrict scenarios to what the user requested:
  if (is.null(scenarios)) {
    # If user did not request specific years, return all available years:
    returned_scenarios <- all_scenarios
  } else {
    # If user requested specific scenarios, return requested available scenarios:
    returned_scenarios <- scenarios[scenarios %in% all_scenarios]
    if ((!(quiet)) && (!(all(scenarios %in% all_scenarios))))
      message('Not available: ', paste(scenarios[!scenarios %in% all_scenarios], collapse=', '))
  }

  # Restrict time periods to what the user requested:
  if (is.null(time_periods)) {
    # If user did not request specific time periods, return all available:
    returned_time_periods <- all_periods
  } else {
    # If user requested specific years, return requested available years:
    returned_time_periods <- time_periods[time_periods %in% all_periods]
    if ((!(quiet)) && (!(all(time_periods %in% all_periods))))
      message('Not available: ', paste(time_periods[!time_periods %in% all_periods], collapse=', '))
  }

  # Restrict versions to what the user requested:
  if (is.null(versions)) {
    # If user did not request specific time periods, return all available:
    returned_versions <- all_versions
  } else {
    # If user requested specific years, return requested available years:
    returned_versions <- versions[versions %in% all_versions]
    if ((!(quiet)) && (!(all(versions %in% all_versions))))
      message('Not available: ', paste(versions[!versions %in% all_versions], collapse=', '))
  }

  # Restrict base vars to what the user requested:
  if (is.null(base_vars)) {
    # If user did not request specific base vars, return all available:
    returned_base_vars <- all_base_vars
  } else {
    # If user requested specific base vars, return requested available base vars:
    returned_base_vars <- base_vars[base_vars %in% all_base_vars]
    if ((!(quiet)) && (!(all(base_vars %in% all_base_vars))))
      message('Not available: ', paste(base_vars[!base_vars %in% all_base_vars], collapse=', '))
  }

  # Return list of user-requested variables:
  if (!(return_all)){
    if (separated) {
      variables <- list(
        time_periods = sort(returned_time_periods),
        scenarios = sort(returned_scenarios),
        models = sort(returned_models),
        base_vars = sort(returned_base_vars),
        versions = sort(returned_versions)
      )
      return(variables)
    } else {
      candidates <- levels(interaction(
        returned_base_vars,
        returned_time_periods,
        returned_models,
        returned_scenarios,
        returned_versions,
        sep="_"
      ))
      
      if (!(quiet)) {
        not_available <- candidates[! candidates %in% all_varnames]
        if (length(not_available) > 0) message(paste0('Not available: ', paste(not_available, collapse=", ")))
      }

      returned_varnames <- candidates[candidates %in% all_varnames]
      return(returned_varnames)
    }
  }
}

get_landcover_variables <- function(separated = TRUE, years = NULL,
                                    base_vars = NULL, quiet = FALSE,
                                    file_size_table = NULL, tempdir = NULL) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_landcover_variable_table(tempdir = tempdir)
  }

  # Info:
  # Landcover variables look like this:
  # <c00>_<year>_<tile_id>.zip
  # "c20_1992_h32v00.zip"

  # Should we return everything, or restrict by scenario/model/time period?
  if (is.null(years) && is.null(base_vars)){
    return_all <- TRUE
    if (!(quiet)) message('Will return all variable names...')
  } else {
    return_all <- FALSE
    if (!(quiet)) message('Will return requested subset of variable names...')
  }

  ###########################################
  ## Extract all available variable names ###
  ###########################################

  # Extract all landcover (starting with cxx_xxxx_) from filenames:
  regex <- "^c[0-9]+_[0-9]+"
  all_varnames <- unique(stringr::str_extract(file_size_table$file_name, regex))
  all_varnames <- all_varnames[!is.na(all_varnames)]

  # If we don't want them separately, return them now:
  if (!(separated)) {
    if (return_all){
      return(all_varnames)
    }
  }

  ###########################################
  ## Split variable names into components ###
  ###########################################

  # Extract years from landcover:
  all_years <- unique(sub("^c[0-9]+_", "", all_varnames))
  all_years <- all_years[!is.na(all_years)]

  # Extract c00 from landcover:
  regex <- "^c[0-9]+_"
  all_base_vars <- unique(stringr::str_extract(all_varnames, regex))
  all_base_vars <- sub("_$", "", all_base_vars)
  all_base_vars <- all_base_vars[!is.na(all_base_vars)]
  #print(paste(all_base_vars, collapse='+'))

  # Return list of all splitted variables:
  if (separated) {
    if (return_all) {
      variables <- list(
        base_vars = sort(all_base_vars),
        years = sort(all_years)
      )
      return(variables)
    }
  }

  ############################################
  ## Restrict to user-requested components ###
  ############################################

  # Restrict years to what the user requested:
  if (is.null(years)) {
    # If user did not request specific years, return all available years:
    returned_years <- all_years
  } else {
    # If user requested specific years, return requested available years:
    returned_years <- years[years %in% all_years]
    if ((!(quiet)) && (!(all(years %in% all_years))))
      message('Not available: ', paste(years[!years %in% all_years], collapse=', '))
  }

  # Restrict base_vars to what the user requested:
  if (is.null(base_vars)) {
    # If user did not request specific base_vars, return all available base_vars:
    returned_base_vars <- all_base_vars
  } else {
    # If user requested specific base_vars, return requested available base_vars:
    returned_base_vars <- base_vars[base_vars %in% all_base_vars]
    if ((!(quiet)) && (!(all(base_vars %in% all_base_vars))))
      message('Not available: ', paste(base_vars[!base_vars %in% all_base_vars], collapse=', '))
  }

  # Return list of user-requested variables:
  if (!(return_all)) {
    if (separated) {
      variables <- list(
        base_vars = sort(returned_base_vars),
        years = sort(returned_years)
      )
      return(variables)
    } else {
      candidates <- levels(interaction(
        returned_base_vars,
        returned_years,
        sep="_"
      ))

      if (!(quiet)) {
        not_available <- candidates[! candidates %in% all_varnames]
        if (length(not_available) > 0) message(paste0('Not available: ', paste(not_available, collapse=", ")))
      }

      returned_varnames <- candidates[candidates %in% all_varnames]
      return(returned_varnames)
    }
  }
}

get_soil_variables <- function(file_size_table = NULL, tempdir = NULL, quiet = FALSE) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_soil_variable_table(tempdir = tempdir)
  }

  all_varnames <- unique(sub("_[^_]+$", "", file_size_table$file_name))
  return (sort(all_varnames))
}

get_present_climate_variables <- function(file_size_table = NULL, tempdir = NULL, quiet = FALSE) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_present_climate_variable_table(tempdir = tempdir)
  }

  all_varnames <- unique(sub("_[^_]+$", "", file_size_table$file_name))
  return (sort(all_varnames))
}

get_hydrography90m_variables <- function(file_size_table = NULL, tempdir = NULL, quiet = FALSE) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_hydro90m_variable_table(tempdir = tempdir)
  }

  all_varnames <- unique(sub("_[^_]+$", "", file_size_table$file_name))
  return (sort(all_varnames))
}

