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
                                         versions = NULL, ignore_missing = FALSE,
                                         tile_ids = NULL) {

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
  } else {
    return_all <- FALSE
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

      # Compute download size and print to screen, if tile_ids specified:
      if (!(is.null(tile_ids))) {
        download_bytes <- download_size(tile_ids, all_varnames, file_size_table, quiet)
        message(paste0("Download size: ",
          length(all_varnames), " variables, ",
          length(tile_ids), " tiles: ",
          download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
      }

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

      # Compute download size and print to screen, if tile_ids specified:
      if (is.null(tile_ids)) {
        download_bytes <- NA
      } else {
        download_bytes <- download_size(tile_ids, all_varnames, file_size_table, quiet)
        message(paste0("Download size: ",
          length(all_varnames), " variables, ",
          length(tile_ids), " tiles: ",
          download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
      }

      variables <- list(
        comment = "Returned all available variables for this dataset.",
        time_periods = sort(all_periods),
        scenarios = sort(all_scenarios),
        models = sort(all_models),
        base_vars = sort(all_base_vars),
        versions = sort(all_versions),
        download_bytes = download_bytes,
        complete_variable_names = all_varnames
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

    # If some models are not available, warn or stop:
    if (!(all(models %in% all_models))) {
      err_msg <- paste0('Not available: Model(s) ', paste(models[!models %in% all_models], collapse=', '))
      if (ignore_missing) {
        message(paste0(err_msg, " Will be ignored...")) # shown right away
        warning(paste0(err_msg, " Was ignored...")) # shown at the end
      } else {
        stop(paste0(err_msg, '. Please check your spelling and try again!'))
      }
    }
  }

  # Restrict scenarios to what the user requested:
  if (is.null(scenarios)) {
    # If user did not request specific years, return all available years:
    returned_scenarios <- all_scenarios
  } else {
    # If user requested specific scenarios, return requested available scenarios:
    returned_scenarios <- scenarios[scenarios %in% all_scenarios]

    # If some scenarios are not available, warn or stop:
    if (!(all(scenarios %in% all_scenarios))) {
      err_msg <- paste0('Not available: Scenario(s) ', paste(scenarios[!scenarios %in% all_scenarios], collapse=', '))
      if (ignore_missing) {
        message(paste0(err_msg, " Will be ignored...")) # shown right away
        warning(paste0(err_msg, " Was ignored...")) # shown at the end
      } else {
        stop(paste0(err_msg, '. Please check your spelling and try again!'))
      }
    }
  }

  # Restrict time periods to what the user requested:
  if (is.null(time_periods)) {
    # If user did not request specific time periods, return all available:
    returned_time_periods <- all_periods
  } else {
    # If user requested specific time periods, return requested available years:
    returned_time_periods <- time_periods[time_periods %in% all_periods]

    # If some timeperiods are not available, warn or stop:
    if (!(all(time_periods %in% all_periods))) {
      err_msg <- paste0('Not available: Time period(s) ', paste(time_periods[!time_periods %in% all_periods], collapse=', '))
      if (ignore_missing) {
        message(paste0(err_msg, " Will be ignored...")) # shown right away
        warning(paste0(err_msg, " Was ignored...")) # shown at the end
      } else {
        stop(paste0(err_msg, '. Please check your spelling and try again!'))
      }
    }
  }

  # Restrict versions to what the user requested:
  if (is.null(versions)) {
    # If user did not request specific versions, return all available:
    returned_versions <- all_versions
  } else {
    # If user requested specific years, return requested available years:
    returned_versions <- versions[versions %in% all_versions]
    # If some versions are not available, warn or stop:
    if (!(all(versions %in% all_versions))) {
      err_msg <- paste0('Not available: Version(s) ', paste(versions[!versions %in% all_versions], collapse=', '))
      if (ignore_missing) {
        message(paste0(err_msg, " Will be ignored...")) # shown right away
        warning(paste0(err_msg, " Was ignored...")) # shown at the end
      } else {
        stop(paste0(err_msg, '. Please check your spelling and try again!'))
      }
    }
  }

  # Restrict base vars to what the user requested:
  if (is.null(base_vars)) {
    # If user did not request specific base vars, return all available:
    returned_base_vars <- all_base_vars
  } else {
    # If user requested specific base vars, return requested available base vars:
    returned_base_vars <- base_vars[base_vars %in% all_base_vars]
    # If some basevars are not available, warn or stop:
    if (!(all(base_vars %in% all_base_vars))) {
      err_msg <- paste0('Not available: Base var(s) ', paste(base_vars[!base_vars %in% all_base_vars], collapse=', '))
      if (ignore_missing) {
        message(paste0(err_msg, " Will be ignored...")) # shown right away
        warning(paste0(err_msg, " Was ignored...")) # shown at the end
      } else {
        stop(paste0(err_msg, '. Please check your spelling and try again!'))
      }
    }
  }

  # Restrict all vars to what the user requested:
  # TODO: This causes warning "longer object length is not a multiple of shorter object length"
  candidates <- levels(interaction(
    returned_base_vars,
    returned_time_periods,
    returned_models,
    returned_scenarios,
    returned_versions,
    sep="_"
  ))
  returned_varnames <- candidates[candidates %in% all_varnames]

  # If some vars are not available, warn or stop:
  not_available <- candidates[! candidates %in% all_varnames]
  if (length(not_available) > 0) {
    err_msg <- paste0('Not available: Variable(s) ', paste(not_available, collapse=", "))
    if (ignore_missing) {
      message(paste0(err_msg, " Will be ignored...")) # shown right away
      warning(paste0(err_msg, " Was ignored...")) # shown at the end
    } else {
      stop(paste0(err_msg, '. Please check your spelling and try again!'))
    }
  }

  # Compute download size and print to screen, if tile_ids specified:
  if (is.null(tile_ids)) {
    download_bytes <- NA
  } else {
    download_bytes <- download_size(tile_ids, returned_varnames, file_size_table, quiet)
    message(paste0("Download size: ",
      length(returned_varnames), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
  }

  # Return list of user-requested variables:
  if (!(return_all)){
    if (separated) {
      variables <- list(
        comment = "Returned all requested variables for this dataset that are available.",
        time_periods = sort(returned_time_periods),
        scenarios = sort(returned_scenarios),
        models = sort(returned_models),
        base_vars = sort(returned_base_vars),
        versions = sort(returned_versions),
        download_bytes = download_bytes,
        complete_variable_names = returned_varnames
      )
      return(variables)
    } else {
      return(returned_varnames)
    }
  }
}

get_landcover_variables <- function(separated = TRUE, years = NULL,
                                    base_vars = NULL, quiet = FALSE,
                                    file_size_table = NULL, tempdir = NULL,
                                    ignore_missing = FALSE, tile_ids = NULL) {

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
  } else {
    return_all <- FALSE
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

      # Compute download size and print to screen, if tile_ids specified:
      if (!(is.null(tile_ids))) {
        download_bytes <- download_size(tile_ids, all_varnames, file_size_table, quiet)
        message(paste0("Download size: ",
          length(all_varnames), " variables, ",
          length(tile_ids), " tiles: ",
          download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
      }
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

      # Compute download size and print to screen, if tile_ids specified:
      if (is.null(tile_ids)){
        download_bytes <- NA
      } else {
        download_bytes <- download_size(tile_ids, all_varnames, file_size_table, quiet)
        message(paste0("Download size: ",
          length(all_varnames), " variables, ",
          length(tile_ids), " tiles: ",
          download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
      }

      variables <- list(
        comment = "Returned all available variables for this dataset.",
        base_vars = sort(all_base_vars),
        years = sort(all_years),
        download_bytes = download_bytes,
        complete_variable_names = all_varnames
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
    # If some years are not available, warn or stop:
    if (!(all(years %in% all_years))) {
      err_msg <- paste0('Not available: Year(s) ', paste(years[!years %in% all_years], collapse=', '))
      if (ignore_missing) {
        message(paste0(err_msg, " Will be ignored...")) # shown right away
        warning(paste0(err_msg, " Was ignored...")) # shown at the end
      } else {
        stop(paste0(err_msg, '. Please check your spelling and try again!'))
      }
    }
  }

  # Restrict base_vars to what the user requested:
  if (is.null(base_vars)) {
    # If user did not request specific base_vars, return all available base_vars:
    returned_base_vars <- all_base_vars
  } else {
    # If user requested specific base_vars, return requested available base_vars:
    returned_base_vars <- base_vars[base_vars %in% all_base_vars]

    # If some base vars are not available, warn or stop:
    if (!(all(base_vars %in% all_base_vars))) {
      err_msg <- paste0('Not available: Base var(s) ', paste(base_vars[!base_vars %in% all_base_vars], collapse=', '))
      if (ignore_missing) {
        message(paste0(err_msg, " Will be ignored...")) # shown right away
        warning(paste0(err_msg, " Was ignored...")) # shown at the end
      } else {
        stop(paste0(err_msg, '. Please check your spelling and try again!'))
      }
    }
  }

  # Restrict all vars to what the user requested:
  # TODO: This causes warning "longer object length is not a multiple of shorter object length"
  candidates <- levels(interaction(
    returned_base_vars,
    returned_years,
    sep="_"
  ))
  returned_varnames <- candidates[candidates %in% all_varnames]

  # If some vars are not available, warn or stop:
  not_available <- candidates[! candidates %in% all_varnames]
  if (length(not_available) > 0) {
    err_msg <- paste0('Not available: Variable(s) ', paste(not_available, collapse=", "))
    if (ignore_missing) {
      message(paste0(err_msg, " Will be ignored...")) # shown right away
      warning(paste0(err_msg, " Was ignored...")) # shown at the end
    } else {
      stop(paste0(err_msg, '. Please check your spelling and try again!'))
    }
  }

  # Compute download size and print to screen, if tile_ids specified:
  if (is.null(tile_ids)) {
    download_bytes <- NA
  } else {
    download_bytes <- download_size(tile_ids, returned_varnames, file_size_table, quiet)
    message(paste0("Download size: ",
      length(returned_varnames), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
  }

  # Return list of user-requested variables:
  if (!(return_all)) {
    if (separated) {
      variables <- list(
        comment = "Returned all requested variables for this dataset that are available.",
        base_vars = sort(returned_base_vars),
        years = sort(returned_years),
        download_bytes = download_bytes,
        complete_variable_names = returned_varnames
      )
      return(variables)
    } else {
      return(returned_varnames)
    }
  }
}

get_soil_variables <- function(tile_ids = NULL, tempdir = NULL, quiet = FALSE,
                               file_size_table = NULL, ignore_missing = FALSE,
                               separated = FALSE) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_soil_variable_table(tempdir = tempdir)
  }

  # Get all variable name from the table:
  all_varnames <- unique(sub("_[^_]+$", "", file_size_table$file_name))

  # Compute download size, if tile_ids is specified:
  if (is.null(tile_ids)) {
    download_bytes <- NA
  } else {
    download_bytes <- download_size(tile_ids, all_varnames, file_size_table, quiet)
    message(paste0("Download size: ",
      length(all_varnames), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
  }

  # Return all variable names
  if (!(separated)) {
    return (sort(all_varnames))
  } else {
    # Return as a list including download_size if separated=TRUE
    variables <- list(
      comment = "Returned all available variables for this dataset.",
      complete_variable_names = sort(all_varnames),
      download_bytes = download_bytes
    )
    return(variables)
  }
}

get_present_climate_variables <- function(tile_ids = NULL, tempdir = NULL,
                                          quiet = FALSE, ignore_missing = FALSE,
                                          file_size_table = NULL, separated = FALSE) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_present_climate_variable_table(tempdir = tempdir)
  }

  # Get all variable name from the table:
  all_varnames <- unique(sub("_[^_]+$", "", file_size_table$file_name))

  # Compute download size, if tile_ids is specified:
  if (is.null(tile_ids)) {
    download_bytes <- NA
  } else {
    download_bytes <- download_size(tile_ids, all_varnames, file_size_table, quiet)
    message(paste0("Download size: ",
      length(all_varnames), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
  }

  # Return all variable names
  if (!(separated)) {
    return (sort(all_varnames))
  } else {
    # Return as a list including download_size if separated=TRUE
    variables <- list(
      comment = "Returned all available variables for this dataset.",
      complete_variable_names = sort(all_varnames),
      download_bytes = download_bytes
    )
    return(variables)
  }
}

get_hydrography90m_variables <- function(tile_ids = NULL, ignore_missing = FALSE,
                                         quiet = NULL, file_size_table = NULL,
                                         tempdir = NULL, separated = FALSE) {

  # Use existing file_size_table, so we don't have to redownload every time:
  if (is.null(file_size_table)) {
    file_size_table <- get_hydro90m_variable_table(tempdir = tempdir)
  }

  # Get all variable name from the table:
  all_varnames <- unique(sub("_[^_]+$", "", file_size_table$file_name))

  # Compute download size, if tile_ids is specified:
  if (is.null(tile_ids)) {
    download_bytes <- NA
  } else {
    download_bytes <- download_size(tile_ids, all_varnames, file_size_table, quiet)
    message(paste0("Download size: ",
      length(all_varnames), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
  }

  # Return all variable names
  if (!(separated)) {
    return (sort(all_varnames))
  } else {
    # Return as a list including download_size if separated=TRUE
    variables <- list(
      comment = "Returned all available variables for this dataset.",
      complete_variable_names = sort(all_varnames),
      download_bytes = download_bytes
    )
    return(variables)
  }
}

download_size <- function(tile_ids, all_varnames, file_size_table, quiet = FALSE, ignore_missing = FALSE) {

  # Get the valid tile ids and files names
  all_tile_ids <- unique(stringr::str_extract(file_size_table$file_path, "h[0-9]+v[0-9]+"))
  all_tile_ids <- all_tile_ids[!is.na(all_tile_ids)]
  all_file_names <- sort(unique(file_size_table$file_name))

  # Which of the requested tile ids are available?
  not_available <- tile_ids[! tile_ids %in% all_tile_ids]
  if (length(not_available) > 0) {
    err_msg <- paste0('Not available: Tile id(s) ', paste(not_available, collapse=", "))
    if (ignore_missing) {
      warning(err_msg)
    } else {
      stop(paste0(err_msg, '. Please check your spelling and try again!'))
    }
  }

  # Compute overall size of download, by iterating
  # over each variable and there over each tile:
  download_bytes <- 0
  i <- 0

  for (ivar in all_varnames) {
    i <- i+1
    j <- 0
    byte_sum_one_var <- 0

    # All tiles of this variable:
    for (itile in tile_ids) {
      j <- j+1

      tile_byte <- check_tiles_filesize(
        variable = ivar,
        file_format = "zip",
        tile_id = itile,
        h90m_varnames = all_varnames,
        h90m_tile_id = all_tile_ids,
        h90m_file_names = all_file_names,
        file_size_table = file_size_table
      )
    
      if (length(tile_byte) == 0) {
        tile_byte <- 0
        warning(paste0("Not available: Tile '", itile, "', of variable '", ivar, "'!"))
      } else {
        #if (!quiet) message(paste0("  Tile ", j, " ('", itile, "') of variable '", ivar, "': Size: ", tile_byte, " bytes."))
      }

      # Sum up for all tiles:
      byte_sum_one_var <- byte_sum_one_var + tile_byte
    }

    # Download size for one variable:
    if (!quiet) message(paste0("Variable '", ivar, "' (", j," tiles): ", byte_sum_one_var/1000000, " MB (", byte_sum_one_var, " bytes)."))

    # Sum up for all variables:
    download_bytes <- download_bytes + byte_sum_one_var
  }

  # Download size for all variables:
  #message(paste0("Overall: ", length(tile_ids), " tiles: ", download_bytes/1000000000, " GB ( ", download_bytes," bytes)."))
  return(download_bytes)
}


# Questions: Return download size or not?

