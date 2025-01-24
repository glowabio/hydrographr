#' @title Download Environment90m tables
#'
#' @description
#' The various `download_something_tables()` functions allow to retrieve
#' the Environment90m variable names and download data of the
#' Environment90m datasets, which are split into 20°x20° tiles.
#'
#' There are basically 3 usages:
#'
#' (1) If the functions are called without arguments (i.e. without specifying
#' variable names and tiles), the available variable names are returned.
#' 
#' (2) If a subset of variables and tile IDs are specified, the download size
#' of the resulting download will be computed.
#' 
#' (3) If a subset of variables and tile IDs are specified, and `download` is
#' set to `TRUE`, the requested tables will be downloaded and either left as
#' zipped files or unzipped to text files.
#' 
#' Multiple regular tiles, e.g. belonging to regional units, can be
#' downloaded in a single request. The tile IDs can be obtained using the
#' function [get_tile_id()].
#'
#'
#' @param subset Vector of the variable names that should be downloaded (or
#'  string "ALL" for all available variables).
#' @param tile_ids Vector containing all tile ids of the tiles (e.g. "h10v04")
#'  that should be downloaded, or whose download availability and size should
#'  be checked(or string "ALL" for all available tiles). 
#' @param download logical. If TRUE, and if `tile_ids` is specified, the files
#'  will be downloaded from the IGB server. If FALSE, and if `tile_ids` is
#'  specified, the download size will be computed. If FALSE, and if `tile_ids`
#'  is NULL, the variable names will be returned to the user. 
#' @param download_dir Directory where the downloads should be stored.
#'  Defaults to the current working directory ".". Ignored if `download=FALSE`.
#' @param file_format File format of the tables, either "txt" or "zip". If
#'  "txt", then the zipped tables are unzipped. If "zip", the downloaded
#'  zipped files are left as they are. Default is "txt", which means that the
#'  zip files are unzipped after downloading. Note that this will take more
#'  space on disk than zips.
#' @param delete_zips logical. boolean If `FALSE`, the downloaded zip files are
#'  not deleted after unzipping. Defaults to TRUE. This is ignored if you
#'  request file format zip.
#' @param ignore_missing logical. What to do if some of the requested variables
#'  and/or tile_ids are not available, which is most frequently caused by a
#'  typo the variable name. If TRUE, the missing or misspelled ones are
#'  ignored while the others are downloaded (and a warning is given out).
#'  If FALSE, the function will fail to allow the user to check the variable
#'  names and their spelling. Defaults to FALSE.
#' @param tempdir Optional (rarely needed). Path to the directory where to
#'  store/look for the temporary various file size tables for the various
#'  Environment90m datasets, which are required and downloaded by the
#'  functions. If not passed, defaults to the output of [base::tempdir()].
#' @param quiet logical. If FALSE, informative messages will be printed.
#'   Default is FALSE.
#' @returns A named list of:
#' * variable names,
#' * components of variable names (if applies),
#' * the dataset name,
#' * the requested tile_ids (if applies),
#' * the download size (if `tile_id` is specified),
#' * the path where downloaded files are stored to (if `download=TRUE`),
#' * etc.
#' @seealso [download_tiles()] for downloading spatial layers (raster, vector)
#'   of the original Hydrography90m dataset, split to the same tiles.
#'
#'
#' @importFrom stringr str_extract
#' @importFrom data.table fread
#'
#' @details
#' In the following table you can find all the variables included in the
#' Environment90m dataset. The column "Variable" includes the variable names
#' that should be used as an input in the parameter "variable" of the function.
#' Likewise, the column "File format" contains the input that should be given to
#' the "file_format" parameter.
#'
#' The Environment90m dataset comprises data from the landcover dataset
#' ESA Land Cover (esa_cci_landcover_v2_1_1), CHELSA v2.1 (chelsa_bioclim_v2_1),
#' SOILGRIDS (soilgrids250m_v2_0) and Hydrography90m (hydrography90m_v1_0).
#'
#' For visualisations the available tiles, and for details on the variables of
#' the Hydrography90m dataset, please refer to
#' \url{https://hydrography.org/hydrography90m/hydrography90m_layers/}.
#'
#' For details on the bioclimatic variables, especially for details of the scale
#' and unit of the values, please refer to
#' \url{http://chelsa-climate.org/}.
#'
#' For details on the ESA Land Cover variables, please refer to
#' \url{https://www.climatologylab.org/terraclimate.html}. Please note that some
#' values in this dataset are aggregated from similar classes (see 
#' Environment90m publication).
#'
#' For details on the Soil data, please refer to
#' \url{https://soilgrids.org}.
#'
#' @author Merret Buurman
#'
#' @references Garcia Marquez J., Amatulli, G., Grigoropoulou, A.,
#' Schürz, M., Tomiczek, T., Buurman, M., Bremerich, V.,
#' Bego, K. and Domisch, S.:
#' Global datasets of aggregated environmental variables at the
#' sub-catchment scale for freshwater biodiversity modeling, in prep.
#' Please contact the authors for more up-to-date citation info.
#'
#' @name download-env90m-tables
NULL



#' @describeIn download-env90m-tables Download SOILGRIDS tables
#'  (soilgrids250m_v2_0)
#' @examples
#' 
#' ### Soil: soilgrids250m_v2_0 ###
#' # Show all available soil variable names:
#' download_soil_tables()
#' 
#' # Compute download size of all soil variables, for one tile:
#' download_soil_tables(
#'   subset = "ALL",
#'   tile_ids = c("h00v04"),
#'   download = FALSE)
#'
#' # Download one soil variable (Clay content), for two tiles:
#' download_soil_tables(
#'   subset = c("clyppt"),
#'   tile_ids = c("h00v04", "h10v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "zip")
#'
#' # Download one soil variable (Clay content), for one tile,
#' # unzip, and delete the zips:
#' download_soil_tables(
#'   subset = c("clyppt"),
#'   tile_ids = c("h00v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "txt",
#'   delete_zips = TRUE) 
#' 
#' @export
download_soil_tables <- function(subset = NULL,
                                 tile_ids = NULL,
                                 download = FALSE,
                                 download_dir = ".",
                                 file_format = "txt",
                                 delete_zips = TRUE,
                                 ignore_missing = FALSE,
                                 tempdir = NULL,
                                 quiet = FALSE) {

  return(download_simple_tables(
    "soilgrids250m_v2_0",
    "env90m_soil_paths_file_sizes.txt",
    subset, tile_ids, ignore_missing, download, download_dir,
    file_format, delete_zips, tempdir, quiet
  ))
}



#' @describeIn download-env90m-tables Download flow tables (flo1k_v1_0)
#' @examples
#'
#' ### Flow (mean): flo1k_v1_0 ###
#' # Show all available flo1k variable names:
#' download_flo1k_tables()
#'
#' # Compute download size of the only flo1k_v1_0 variable (mean flow),
#' # for one tile:
#' download_flo1k_tables(
#'   subset = "ALL",
#'   tile_ids = c("h00v04"),
#'   download = FALSE)
#'
#' # Download the only flo1k_v1_0 variable (flo1k), for two tiles:
#' download_soil_tables(
#'   subset = c("flo1k"),
#'   tile_ids = c("h00v04", "h10v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "zip")
#'
#' # Download the only flo1k_v1_0 variable (flo1k), for one tile,
#' # unzip, and delete the zips:
#' download_soil_tables(
#'   subset = c("flo1k"),
#'   tile_ids = c("h00v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "txt",
#'   delete_zips = TRUE)
#'
#' @export
download_flo1k_tables <- function(subset = NULL,
                                  tile_ids = NULL,
                                  download = FALSE,
                                  download_dir = ".",
                                  file_format = "txt",
                                  delete_zips = TRUE,
                                  ignore_missing = FALSE,
                                  tempdir = NULL,
                                  quiet = FALSE) {

  return(download_simple_tables(
    "flo1k_v1_0",
    "env90m_flo1k_paths_file_sizes.txt",
    subset, tile_ids, ignore_missing, download, download_dir,
    file_format, delete_zips, tempdir, quiet
  ))
}



#' @describeIn download-env90m-tables Download CGIAR-CSI tables (cgiar_csi_v3)
#' @examples
#'
#' ### CGIAR CSI dataset: cgiar_csi_v3 ###
#' # Show all available cgiar variable names
#' download_cgiar_tables()
#'
#' # Compute download size of all cgiar variables, for one tile:
#' download_cgiar_tables(
#'   subset = "ALL",
#'   tile_ids = c("h00v04"),
#'   download = FALSE)
#'
#' # Download one cgiar variable (Global Aridity Index), for two tiles:
#' download_cgiar_tables(
#'   subset = c("garid"),
#'   tile_ids = c("h00v04", "h10v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "zip")
#'
#' # Download two cgiar variables (Global Aridity Index, Potential
#' # Evapotranspiration), for one tile, unzip, and delete the zips:
#' download_cgiar_tables(
#'   subset = c("garid", "gevapt"),
#'   tile_ids = c("h00v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "txt",
#'   delete_zips = TRUE)
#'
#' @export
download_cgiar_tables <- function(subset = NULL,
                                  tile_ids = NULL,
                                  download = FALSE,
                                  download_dir = ".",
                                  file_format = "txt",
                                  delete_zips = TRUE,
                                  ignore_missing = FALSE,
                                  tempdir = NULL,
                                  quiet = FALSE) {

  return(download_simple_tables(
    "cgiar_csi_v3",
    "env90m_cgiar_paths_file_sizes.txt",
    subset, tile_ids, ignore_missing, download, download_dir,
    file_format, delete_zips, tempdir, quiet
  ))
}


#' @describeIn download-env90m-tables Download MERIT-DEM tables
#'  (Multi-Error-Removed Improved-Terrain Digital Elevation Model,
#'  merit_dem_v1_0_3)
#' @examples
#'
#' ### Digital Elevation Model: merit_dem_v1_0_3 ###
#' # (Multi-Error-Removed Improved-Terrain Digital Elevation Model)
#' # Show all available merit_dem_v1_0_3 variable names
#' download_merit_dem_tables()
#'
#' # Compute download size of the only merit_dem_v1_0_3 variable
#' # (Mean elevation), for one tile:
#' download_merit_dem_tables(
#'   subset = "ALL",
#'   tile_ids = c("h00v04"),
#'   download = FALSE)
#'
#' # Download the only merit_dem_v1_0_3 variable (mean elevation),
#' # for two tiles:
#' download_merit_dem_tables(
#'   subset = c("elev"), # or "ALL"
#'   tile_ids = c("h00v04", "h10v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "zip")
#'
#' # Download the only merit_dem_v1_0_3 variable (mean elevation),
#' # for one tile, unzip, and delete the zips:
#' download_merit_dem_tables(
#'   subset = c("elev"),
#'   tile_ids = c("h00v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "txt",
#'   delete_zips = TRUE)
#'
#' @export
download_merit_dem_tables <- function(subset = NULL,
                                      tile_ids = NULL,
                                      download = FALSE,
                                      download_dir = ".",
                                      file_format = "txt",
                                      delete_zips = TRUE,
                                      ignore_missing = FALSE,
                                      tempdir = NULL,
                                      quiet = FALSE) {

  return(download_simple_tables(
    "merit_dem_v1_0_3",
    "env90m_merit_dem_paths_file_sizes.txt",
    subset, tile_ids, ignore_missing, download, download_dir,
    file_format, delete_zips, tempdir, quiet
  ))
}



#' @describeIn download-env90m-tables Download Hydrography90m tables
#'  (hydrography90m_v1_0)
#' @examples
#' 
#' ### Hydrography90m: hydrography90m_v1_0 ###
#' # Show all available hy90m variable names
#' download_hydrography90m_tables()
#' 
#' # Compute download size of all hy90m variables, for one tile:
#' download_hydrography90m_tables(
#'   subset = "ALL",
#'   tile_ids = c("h00v04"),
#'   download = FALSE)
#'
#' # Download one hy90m variable (Strahler’s stream order), for two tiles:
#' download_hydrography90m_tables(
#'   subset = c("stream_strahler"),
#'   tile_ids = c("h00v04", "h10v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "zip")
#'
#' # Download one hy90m variable (Strahler’s stream order), for one tile,
#' # unzip, and delete the zips:
#' download_hydrography90m_tables(
#'   subset = c("stream_strahler"),
#'   tile_ids = c("h00v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "txt",
#'   delete_zips = TRUE) 
#' 
#' @export
download_hydrography90m_tables <- function(subset = NULL,
                                           tile_ids = NULL,
                                           download = FALSE,
                                           download_dir = ".",
                                           file_format = "txt",
                                           delete_zips = TRUE,
                                           ignore_missing = FALSE,
                                           tempdir = NULL,
                                           quiet = FALSE) {

  return(download_simple_tables(
    "hydrography90m_v1_0", 
    "env90m_hydro_paths_file_sizes.txt",
    subset, tile_ids, ignore_missing, download, download_dir,
    file_format, delete_zips, tempdir, quiet
  ))
}


#' @describeIn download-env90m-tables Download CHELSA bioclimatic variables tables,
#'  except for future simulations (Climatologies at high resolution for the
#'  earth’s land surface areas, chelsa_bioclim_v2_1)
#' @examples
#' 
#' ### Bioclimatic Variables: chelsa_bioclim_v2_1 ###
#' ### (excluding future simulations)             ###
#' # Show all available bioclim variable names
#' # (excluding future simulations):
#' download_present_climate_tables()
#' 
#' # Compute download size of all bioclim variables, for one tile:
#' download_present_climate_tables(
#'   subset = "ALL",
#'   tile_ids = c("h00v04"),
#'   download = FALSE)
#'
#' # Download one bioclim variable (Annual mean temperature), for two tiles:
#' download_present_climate_tables(
#'   subset = c("bio1"),
#'   tile_ids = c("h00v04", "h10v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "zip")
#'
#' # Download one bioclim variable (Annual mean temperature), for one tile,
#' # unzip, and delete the zips:
#' download_present_climate_tables(
#'   subset = c("bio1"),
#'   tile_ids = c("h00v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "txt",
#'   delete_zips = TRUE) 
#' 
#' @export
download_present_climate_tables <- function(subset = NULL,
                                            tile_ids = NULL,
                                            download = FALSE,
                                            download_dir = ".",
                                            file_format = "txt",
                                            delete_zips = TRUE,
                                            ignore_missing = FALSE,
                                            tempdir = NULL,
                                            quiet = FALSE) {

  return(download_simple_tables(
    "chelsa_bioclim_v2_1",
    "env90m_presentclimate_paths_file_sizes.txt",
    subset, tile_ids, ignore_missing, download, download_dir,
    file_format, delete_zips, tempdir, quiet
  ))
}


#' @describeIn download-env90m-tables Download CHELSA bioclimatic variables tables,
#'  future simulations only (Climatologies at high resolution for the
#'  earth’s land surface areas, chelsa_bioclim_v2_1)
#' @param base_vars (Only in `download_future_climate_tables()` and
#'  `download_landcover_tables()`) Vector of the desired base variables, e.g.
#'  the landcover variable "c20_1992" can be expressed as base variable "c20"
#'  and year "1992".
#' @param time_periods (Only in `download_future_climate_...`) Vector of the
#'  desired time periods (leave `NULL` or specify `"ALL"` for all available
#'  time periods).
#' @param models (Only in `download_future_climate_...`) Vector of the desired
#'  models (leave `NULL` or specify `"ALL"` for all available models).
#' @param scenarios (Only in `download_future_climate_...`) Vector of the
#'  desired scenarios (leave `NULL` or specify `"ALL"` for all available
#'  scenarios).
#' @param versions (Only in `download_future_climate_...`) Vector of the
#'  desired versions (leave `NULL` or specify `"ALL"` for all available
#'  versions). As of January 2025, the only available version is "V.2.1".
#' 
#' @examples
#'
#' ### Bioclimatic Variables: chelsa_bioclim_v2_1 ###
#' ### (future simulations)                       ###    
#' # Show all available future bioclim variable names
#' download_future_climate_tables()
#' 
#' # Compute download size of all variables, for one tile:
#' download_future_climate_tables(
#'   subset = "ALL",
#'   tile_ids = c("h00v04"),
#'   download = FALSE)
#'
#' # Download one hy90m variable (Annual mean temperature), for two tiles:
#' download_future_climate_tables(
#'   subset = c("bio1"),
#'   tile_ids = c("h00v04", "h10v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "zip")
#'
#' # Download one hy90m variable (Annual mean temperature), for one tile,
#' # unzip, and delete the zips:
#' download_future_climate_tables(
#'   subset = c("bio1"),
#'   tile_ids = c("h00v04"),
#'   download = TRUE,
#'   download_dir = ".",
#'   file_format = "txt",
#'   delete_zips = TRUE) 
#' 
#' @export
download_future_climate_tables <- function(base_vars = NULL,
                                           time_periods = NULL,
                                           models = NULL,
                                           scenarios = NULL,
                                           versions = NULL,
                                           subset = NULL,
                                           tile_ids = NULL,
                                           download = FALSE,
                                           download_dir = ".",
                                           file_format = "txt",
                                           delete_zips = TRUE,
                                           ignore_missing = FALSE,
                                           tempdir = NULL,
                                           quiet = FALSE) {

  #########################################
  ### Extract info from file size table ###
  #########################################

  # Use file_size_table from tempdir or download it:
  file_name <- "env90m_futureclimate_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(file_name, tempdir, quiet)

  # Info:
  # Future climate variables look like this:
  # <base_var>_<time_period>_<model>_<scenario>_<version>_<tile_id>.zip
  # "bio9_2071-2100_ukesm1-0-ll_ssp585_V.2.1_h32v00.zip"

  # Extract all formats from file size table:
  regex_format <- ".[a-z]+$"
  #all_formats <- unique(stringr::str_extract(file_size_table$file_name, regex_format))
  #all_formats <- unique(sub("^.", "", all_formats))
  filenames_without_format <- unique(sub(regex_format, "", file_size_table$file_name))

  # Extract all tiles from file size table:
  regex_tile <- "_h[0-9]+v[0-9]+$"
  all_tiles <- unique(stringr::str_extract(filenames_without_format, regex_tile))
  all_tiles <- unique(sub("^_", "", all_tiles))
  filenames_without_tile <- unique(sub(regex_tile, "", filenames_without_format))
  all_varnames <- filenames_without_tile

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

  ############################################
  ### Does user want all variables / tiles ###
  ### or did they specify a subset?        ###
  ############################################

  # Should we return everything, or restrict by scenario/model/time period?
  if (is.null(subset) && is.null(time_periods) && is.null(scenarios) && is.null(models) && is.null(base_vars) && is.null(versions)) {
    return_all_vars <- TRUE
  } else if (length(subset) == 1 && subset == "ALL") {
    return_all_vars <- TRUE
  } else if (length(time_periods) == 1 && time_periods == "ALL" &&
             length(scenarios) == 1    && scenarios  == "ALL" &&
             length(models) == 1       && models  == "ALL" &&
             length(base_vars) == 1    && base_vars == "ALL" &&
             length(versions) == 1     && versions == "ALL") {
    return_all_vars <- TRUE
  } else {
    return_all_vars <- FALSE
  }

  # Same for tiles
  if (length(tile_ids) == 1 && tile_ids == "ALL") {
    tile_ids <- all_tiles
    return_all_tiles <- TRUE
  } else {
    return_all_tiles <- FALSE
  }

  #####################################
  ### Return/download all variables ###
  ### (no subset specified)         ###
  #####################################

  # If user did not specify a subset, return all variable names
  # as a list of splitted components:
  if (return_all_vars) {

    # Assemble the object to be returned:
    variables <- list(
      variable_names = sort(all_varnames),
      base_vars = sort(all_base_vars),
      models = sort(all_models),
      scenarios = sort(all_scenarios),
      time_periods = sort(all_periods),
      versions = sort(all_versions),
      comment = "All available variables for this dataset.",
      dataset_name = "chelsa_bioclim_v2_1"
    )

    # Compute download size, if desired tiles are specified:
    if (! (is.null(tile_ids))) {
      download_bytes <- compute_download_size(
        all_varnames, tile_ids, file_size_table, quiet)
      message(paste0("Info: Download size: ",
        length(all_varnames), " variables, ",
        length(tile_ids), " tiles: ",
        download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
      variables$download_bytes=download_bytes
      variables$tile_ids=tile_ids
    }

    # Does the user want to download all fut.clim. variables, or just see them?
    if (download) {

      # If the user did not specify a subset, we only allow downloading
      # they explicitly say subset="ALL"
      if (is.null(subset) && is.null(time_periods) && is.null(scenarios) && is.null(models) && is.null(base_vars) && is.null(versions)) {
        msg <- "You did not provide any subset. If you are sure that you want ALL variables, please specify subset='ALL'!"
        variables$note <- msg
        warning(msg)
        return(variables)
      }

      # Without knowing which tiles, user cannot download:
      if (is.null(tile_ids)) {
        msg <- "To download, please specify parameter 'tile_ids'."
        variables$note = msg
        warning(msg) # warning, because user obviously wanted to download...

      # If tiles were specified, we can download:
      } else {
        # Note: The following message is never quiet!
        message("\nStarting download of ", download_bytes/1000000000, " GB...")
        outcome <- do_env90m_download(all_varnames, tile_ids, file_size_table,
            download_dir = download_dir, file_format = file_format,
            quiet = quiet, delete_zips = delete_zips)
        variables <- c(variables, outcome)
      }

    # User does not want to download all fut.clim. variables, just see them:
    } else {
      if (is.null(tile_ids)) {
        msg <- "To download, please specify parameter 'tile_ids', and add 'download=TRUE'."
        if (!(quiet)) message(msg)
        variables$note = msg
      } else {
        msg <- "To download, please specify 'download=TRUE'."
        if (!(quiet)) message(msg)
        variables$note = msg
      }
    }

    return(variables)
  }

  # If user specified a subset, continue with those specified variables that
  # are available:

  ########################################
  ## Restrict to user-requested subset ###
  ########################################

  # If the user specified a subset of complete variable names, use it:
  if (!(is.null(subset))){
    # Only use those variables that are available:
    varnames_to_be_returned <- subset[subset %in% all_varnames]

    # Construct the list to be returned to the user
    variables <- list(
      comment = "Subset of variables for this dataset.",
      dataset_name = "chelsa_bioclim_v2_1",
      variable_names = sort(varnames_to_be_returned)
    )

  # If the user specified subsets of components (e.g. models, scenarios, ...)
  } else {

    # Restrict scenarios to what the user requested:
    if (is.null(models) || (length(models) == 1 && models == "ALL")) {
      # If user did not request scenarios models, return all available models:
      models_to_be_returned <- all_models
    } else {
      # If user requested specific models, return requested available models:
      models_to_be_returned <- models[models %in% all_models]
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
    if (is.null(scenarios) || (length(scenarios) == 1 && scenarios == "ALL")) {
      # If user did not request specific scenarios, return all available scenarios:
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
    if (is.null(time_periods) || (length(time_periods) == 1 && time_periods == "ALL")) {
      # If user did not request specific time periods, return all available:
      returned_time_periods <- all_periods
    } else {
      # If user requested specific time periods, return requested available time_periods:
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
    if (is.null(versions) || (length(versions) == 1 && versions == "ALL")) {
      # If user did not request specific versions, return all available:
      returned_versions <- all_versions
    } else {
      # If user requested specific versions, return requested available version:
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
    if (is.null(base_vars) || (length(base_vars) == 1 && base_vars == "ALL")) {
      # If user did not request specific base vars, return all available:
      base_vars_to_be_returned <- all_base_vars
    } else {
      # If user requested specific base vars, return requested available base vars:
      base_vars_to_be_returned <- base_vars[base_vars %in% all_base_vars]
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

    # Combine those subsets of years or base vars to a subset of
    # complete variable names
    # TODO: This may cause warning "longer object length is not a multiple of shorter object length"
    subset <- levels(interaction(
      base_vars_to_be_returned,
      returned_time_periods,
      models_to_be_returned,
      returned_scenarios,
      returned_versions,
      sep="_"
    ))

    # Only continue with those that are available:
    varnames_to_be_returned <- subset[subset %in% all_varnames]

    # Construct the list to be returned to the user
    variables <- list(
      variable_names = sort(varnames_to_be_returned),
      time_periods = sort(returned_time_periods),
      scenarios = sort(returned_scenarios),
      models = sort(models_to_be_returned),
      base_vars = sort(base_vars_to_be_returned),
      versions = sort(returned_versions),
      comment = "Subset of variables for this dataset.",
      dataset_name = "chelsa_bioclim_v2_1"
    )
  }

  # If some vars are not available, warn or stop:
  not_available <- subset[! subset %in% all_varnames]
  if (length(not_available) > 0) {
    err_msg <- paste0('Not available: Variable(s) ', paste(not_available, collapse=", "))
    if (ignore_missing) {
      message(paste0(err_msg, " Will be ignored...")) # shown right away
      warning(paste0(err_msg, " Was ignored...")) # shown at the end
      variables$not_available = not_available
    } else {
      stop(paste0(err_msg, '. Please check your spelling and try again!'))
    }
  }  

  # Compute download size, if tile_ids is specified:
  if (!(is.null(tile_ids))) {
    download_bytes <- compute_download_size(varnames_to_be_returned, tile_ids, file_size_table, quiet)
    message(paste0("Info: Download size: ",
      length(varnames_to_be_returned), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
    variables$download_bytes=download_bytes
    variables$tile_ids=tile_ids
  }

  # Download the future climate data if requested:
  if (download) {
    if (is.null(tile_ids)) {
      msg <- "To download, please specify parameter 'tile_ids'."
      variables$note = msg
      warning(msg) # warning, because user obviously wanted to download...
    } else {
      # Note: The following message is never quiet!
      message("\nStarting download of ", download_bytes/1000000000, " GB...")
      outcome <- do_env90m_download(varnames_to_be_returned, tile_ids, file_size_table,
          download_dir = download_dir, file_format = file_format,
          quiet = quiet, delete_zips = delete_zips)
      variables <- c(variables, outcome)
    }
  } else {
    if (is.null(tile_ids)){
      msg <- "To download, please specify parameter 'tile_ids', and add 'download=TRUE'."
      variables$note = msg
      if (!(quiet)) message(msg)
    } else {
      msg <- "To download, please specify 'download=TRUE'."
      variables$note = msg
      if (!(quiet)) message(msg)
    }
  }

  # Return the list
  return(variables)
}


#' @describeIn download-env90m-tables Download ESA Land Cover tables
#'  (esa_cci_landcover_v2_1_1)
#' 
#' @param base_vars (Only in `download_future_climate_tables()` and
#'  `download_landcover_tables()`) Vector of the desired base variables, e.g.
#'  the landcover variable "c20_1992" can be expressed as base variable "c20"
#'  and year "1992".
#' @param years (Only in `download_landcover_...`) Vector of the desired years
#'  (leave `NULL` or specify `"ALL"` for all available years).
#' @examples
#'
#' ### Landcover: esa_cci_landcover_v2_1_1 ###
#' # Show all available landcover variable names:
#' download_landcover_tables()
#'
#' # Compute download size of two landcover base variables (Cropland, rainfed,
#' # and Grassland) and two years, for all tiles:
#'   vars <- download_landcover_tables(
#'     base_vars=c("c10", "c130"),
#'     years=c(1992, 1993),
#'     tile_ids="ALL")
#' 
#' # Download two base variables and one year, for two tiles:
#'   vars <- download_landcover_tables(
#'     base_vars=c("c10", "c130"),
#'     years=c(1992),
#'     tile_ids=c("h00v04", "h10v04"),
#'     download=TRUE,
#'     download_dir="/tmp",
#'     file_format="zip",
#'     delete_zips=FALSE)
#' 
#' @export
download_landcover_tables <- function(base_vars = NULL,
                                      years = NULL,
                                      subset = NULL,
                                      tile_ids = NULL,
                                      download = FALSE,
                                      download_dir = ".",
                                      file_format = "txt",
                                      delete_zips = TRUE,
                                      ignore_missing = FALSE,
                                      tempdir = NULL,
                                      quiet = FALSE) {

  #########################################
  ### Extract info from file size table ###
  #########################################

  file_name <- "env90m_landcover_paths_file_sizes.txt"
  file_size_table <- get_file_size_table(file_name, tempdir, quiet)

  # Info:
  # Landcover variables look like this:
  # <c00>_<year>_<tile_id>.zip
  # "c20_1992_h32v00.zip"

  # Extract all landcover (starting with cxx_xxxx) from filenames:
  regex <- "^c[0-9]+_[0-9]+"
  all_varnames <- unique(stringr::str_extract(file_size_table$file_name, regex))
  all_varnames <- all_varnames[!is.na(all_varnames)]

  # Extract years from landcover:
  all_years <- unique(sub("^c[0-9]+_", "", all_varnames))
  all_years <- all_years[!is.na(all_years)]

  # Extract c00 from landcover:
  regex <- "^c[0-9]+_"
  all_base_vars <- unique(stringr::str_extract(all_varnames, regex))
  all_base_vars <- sub("_$", "", all_base_vars)
  all_base_vars <- all_base_vars[!is.na(all_base_vars)]
  #print(paste(all_base_vars, collapse='+'))

  # Extract all formats from file size table:
  regex_format <- ".[a-z]+$"
  #all_formats <- unique(stringr::str_extract(file_size_table$file_name, regex_format))
  #all_formats <- unique(sub("^.", "", all_formats))
  filenames_without_format <- unique(sub(regex_format, "", file_size_table$file_name))

  # Extract all tiles from file size table:
  regex_tile <- "_h[0-9]+v[0-9]+$"
  all_tiles <- unique(stringr::str_extract(filenames_without_format, regex_tile))
  all_tiles <- unique(sub("^_", "", all_tiles))


  ############################################
  ### Does user want all variables / tiles ###
  ### or did they specify a subset?        ###
  ############################################

  # Should we return everything, or restrict by scenario/model/time period?
  if (is.null(years) && is.null(base_vars) && is.null(subset)) {
    return_all_vars <- TRUE
  } else if (length(subset) == 1 && subset == "ALL") {
    return_all_vars <- TRUE
  } else if (length(years) == 1     && years == "ALL" &&
             length(base_vars) == 1 && base_vars == "ALL") {
    return_all_vars <- TRUE
  } else {
    return_all_vars <- FALSE
  }

  # Same for tiles
  if (length(tile_ids) == 1 && tile_ids == "ALL") {
    tile_ids <- all_tiles
    return_all_tiles <- TRUE
  } else {
    return_all_tiles <- FALSE
  }

  #####################################
  ### Return/download all variables ###
  ### (no subset specified)         ###
  #####################################

  # If user did not specify a subset, return all variable names
  # as a list of splitted components:
  if (return_all_vars) {

    # Assemble the object to be returned:
    variables <- list(
      base_vars = sort(all_base_vars),
      years = sort(all_years),
      variable_names = sort(all_varnames),
      comment = "All available variables for this dataset.",
      dataset_name = "esa_cci_landcover_v2_1_1"
    )

    # Compute download size, if desired tiles are specified:
    if (! (is.null(tile_ids))) {
      download_bytes <- compute_download_size(
        all_varnames, tile_ids, file_size_table, quiet)
      message(paste0("Info: Download size: ",
        length(all_varnames), " variables, ",
        length(tile_ids), " tiles: ",
        download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
      variables$download_bytes=download_bytes
      variables$tile_ids=tile_ids
    }

    # Does the user want to download all landcover variables, or just see them?
    if (download) {

      # If the user did not specify a subset, we only allow downloading
      # they explicitly say subset="ALL"
      if (is.null(years) && is.null(base_vars) && is.null(subset)) {
        msg <- "You did not provide any subset. If you are sure that you want ALL variables, please specify subset='ALL'!"
        variables$note <- msg
        warning(msg)
        return(variables)
      }

      # Without knowing which tiles, user cannot download:
      if (is.null(tile_ids)) {
        msg <- "To download, please specify parameter 'tile_ids'."
        variables$note = msg
        warning(msg) # warning, because user obviously wanted to download...

      # If tiles were specified, we can download:
      } else {
        # Note: The following message is never quiet!
        message("\nStarting download of ", download_bytes/1000000000, " GB...")
        outcome <- do_env90m_download(all_varnames, tile_ids, file_size_table,
            download_dir = download_dir, file_format = file_format,
            quiet = quiet, delete_zips = delete_zips)
        variables <- c(variables, outcome)
      }

    # User does not want to download all variables, just see them:
    } else {
      if (is.null(tile_ids)){
        msg <- "To download, please specify parameter 'tile_ids', and add 'download=TRUE'."
        if (!(quiet)) message(msg)
        variables$note = msg
      } else {
        msg <- "To download, please specify 'download=TRUE'."
        if (!(quiet)) message(msg)
        variables$note = msg
      }
    }

    return(variables)
  }

  # If user specified a subset, continue with those specified variables that
  # are available:

  ########################################
  ## Restrict to user-requested subset ###
  ########################################

  # If the user specified a subset of complete variable names, use it:
  if (!(is.null(subset))){
    # Only use those variables that are available:
    varnames_to_be_returned <- subset[subset %in% all_varnames]

    # Construct the list to be returned to the user
    variables <- list(
      comment = "Subset of variables for this dataset.",
      dataset_name = "esa_cci_landcover_v2_1_1",
      variable_names = sort(varnames_to_be_returned)
    )

  } else {

    # Restrict years to what the user requested:
    if (is.null(years) || (length(years) == 1 && years == "ALL")) {
      # If user did not request specific years, return all available years:
      # TODO: Something around here (?) causes Warning message: In ans * length(l) + if1 :
      # longer object length is not a multiple of shorter object length
      years_to_be_returned <- all_years
    } else {
      # If user requested specific years, return requested available years:
      years_to_be_returned <- years[years %in% all_years]
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
    if (is.null(base_vars) || (length(base_vars) == 1 && base_vars == "ALL")) {
      # If user did not request specific base_vars, return all available base_vars:
      base_vars_to_be_returned <- all_base_vars
    } else {
      # If user requested specific base_vars, return requested available base_vars:
      base_vars_to_be_returned <- base_vars[base_vars %in% all_base_vars]

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

    # Combine those subsets of years or base vars to a subset of
    # complete variable names
    subset <- c()
    for (base_var in base_vars_to_be_returned) {
      for (year in years_to_be_returned) {
        subset <- c(subset, paste0(base_var, "_", year))
      }
    }
    # Note: Those nested for loops may not be the best way to do it.
    # I tried levels(interaction(basevars, years, sep="_")), but it caused a warning:
    # "longer object length is not a multiple of shorter object length"

    # Only continue with those that are available:
    varnames_to_be_returned <- subset[subset %in% all_varnames]

    # Construct the list to be returned to the user
    variables <- list(
      variable_names = sort(varnames_to_be_returned),
      base_vars = sort(base_vars_to_be_returned),
      years = sort(years_to_be_returned),
      comment = "Subset of variables for this dataset.",
      dataset_name = "esa_cci_landcover_v2_1_1"
    )
  }

  # If some vars are not available, warn or stop:
  not_available <- subset[! subset %in% all_varnames]
  if (length(not_available) > 0) {
    err_msg <- paste0('Not available: Variable(s) ', paste(not_available, collapse=", "))
    if (ignore_missing) {
      message(paste0(err_msg, " Will be ignored...")) # shown right away
      warning(paste0(err_msg, " Was ignored...")) # shown at the end
      variables$not_available = not_available
    } else {
      stop(paste0(err_msg, '. Please check your spelling and try again!'))
    }
  }

  # Compute download size, if tile_ids is specified:
  if (!(is.null(tile_ids))) {
    download_bytes <- compute_download_size(varnames_to_be_returned, tile_ids, file_size_table, quiet)
    message(paste0("Info: Download size: ",
      length(varnames_to_be_returned), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
    variables$download_bytes=download_bytes
    variables$tile_ids=tile_ids
  }

  # Download the landcover data if requested:
  if (download) {
    if (is.null(tile_ids)) {
      msg <- "To download, please specify parameter 'tile_ids'."
      variables$note = msg
      warning(msg) # warning, because user obviously wanted to download...
    }
    # Note: The following message is never quiet!
    message("\nStarting download of ", download_bytes/1000000000, " GB...")
    outcome <- do_env90m_download(varnames_to_be_returned, tile_ids, file_size_table,
        download_dir = download_dir, file_format = file_format,
        quiet = quiet, delete_zips = delete_zips)
    variables <- c(variables, outcome)
  } else {
    if (is.null(tile_ids)){
      msg <- "To download, please specify parameter 'tile_ids', and add 'download=TRUE'."
      variables$note = msg
      if (!(quiet)) message(msg)
    } else {
      msg <- "To download, please specify 'download=TRUE'."
      variables$note = msg
      if (!(quiet)) message(msg)
    }
  }

  # Return the list
  return(variables)
}


#' @title Download generic Env90m tables
#' 
#' @description
#' Download ... for all Environment90m datasets which have simple variable
#' names, not combined variable names that can be split into various components
#' (e.g. model, scenario, ... like in the Future Climate case).
#' 
#' @param dataset_name String. Name of the dataset. Will be returned to the
#'  user in the result.
#' @param table_file_name String. Name of the table that should be downloaded.
#'  Each Environment90m dataset has one file size table. 
#' @param subset Vector of the variable names that should be downloaded (or
#' string "ALL" for all available variables).  
#' @param tile_ids Vector containing all tile ids of the tiles (e.g. "h10v04")
#'  whose size should be computed and whose availability should be checked.
#' @param ignore_missing logical. If TRUE, a warning is given out for missing
#'  tiles. If FALSE, the function stops with an error. Defaults to FALSE.
#' @param quiet logical. If FALSE, informative messages will be printed.
#'   Default is FALSE.
#' @param download logical. If TRUE, and if `tile_ids` is specified, the files
#'  will be downloaded from the server. If FALSE, and if `tile_ids` is
#'  specified, the download size will be computed. If FALSE, and if `tile_ids`
#'  is NULL, the variable names will be returned to the user. 
#' @param download_dir Directory where the downloads should be stored. Defaults
#'  to the current working directory ".".
#' @param file_format File format of the tables, either "txt" or "zip". If
#'  "txt", then the zipped tables are unzipped. If "zip", the downloaded zipped
#'  files are left as they are. Defaults to "txt".
#' @param delete_zips logical. If TRUE, zip files will be deleted after
#'  unzipping. Only works when file_format is "txt".
#' @param tempdir String. Path to the directory where to store/look for the
#'  file size table.
#' @returns Data frame that contains file name, size in bytes and file path
#'  (on the server) for all files (data tables) of one specific
#'  Environment90m dataset (e.g. landcover, soil).
#' @examples
#'  download_simple_tables(
#'    "soilgrids250m_v2_0",
#'    "env90m_soil_paths_file_sizes.txt",
#'    c("clyppt"), # this is one soil variable name
#'    "ALL", # all tiles
#'    FALSE, # ignore_missing?
#'    TRUE, # download?
#'    "./downloads",
#'    "txt",
#'    TRUE, # delete_zips?
#'    "/tmp", # temp dir
#'    FALSE) # quiet?
#' 
#' @noRd
download_simple_tables <- function(
  dataset_name,
  table_file_name,
  subset,
  tile_ids,
  ignore_missing,
  download,
  download_dir,
  file_format,
  delete_zips,
  tempdir,
  quiet) {


  #########################################
  ### Extract info from file size table ###
  #########################################

  # Use file_size_table from tempdir or download it:
  file_size_table <- get_file_size_table(table_file_name, tempdir, quiet)

  # Get all variable names from the table:
  all_varnames <- unique(sub("_[^_]+$", "", file_size_table$file_name))

  # Get all tiles from the table:
  regex_format <- ".[a-z]+$"
  filenames_without_format <- unique(sub(regex_format, "", file_size_table$file_name))
  regex_tile <- "_h[0-9]+v[0-9]+$"
  all_tiles <- unique(stringr::str_extract(filenames_without_format, regex_tile))
  all_tiles <- unique(sub("^_", "", all_tiles))

  ############################################
  ### Does user want all variables / tiles ###
  ### or did they specify a subset?        ###
  ############################################
  
  # If user did not specify a subset, return all variable names:
  if (is.null(subset) || (length(subset) == 1 && subset=="ALL")) {
    return_all_vars <- TRUE
  } else {
    return_all_vars <- FALSE
  }

  # Same for tiles
  if (length(tile_ids) == 1 && tile_ids == "ALL") {
    tile_ids <- all_tiles
    return_all_tiles <- TRUE
  } else {
    return_all_tiles <- FALSE
  }

  #####################################
  ### Return/download all variables ###
  ### (no subset specified)         ###
  #####################################

  if (return_all_vars) {
    variables <- list(
      comment = "All available variables for this dataset.",
      dataset_name = dataset_name,
      variable_names = sort(all_varnames)
    )

    # Compute download size, if desired tiles are specified:
    if (! (is.null(tile_ids))) {
      download_bytes <- compute_download_size(
        all_varnames, tile_ids, file_size_table, quiet)
      if (!(quiet)) message(paste0("Info: Download size: ",
        length(all_varnames), " variables, ",
        length(tile_ids), " tiles: ",
        download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
      variables$download_bytes=download_bytes
      variables$tile_ids=tile_ids
    }

    # Does the user want to download all variables, or just see them?
    if (download) {

      # If the user did not specify a subset, we only allow downloading
      # they explicitly say subset="ALL"
      if (is.null(subset)) {
        msg <- "You did not provide any subset. If you are sure that you want ALL variables, please specify subset='ALL'!"
        variables$note <- msg
        warning(msg)
        return(variables)
      }

      # Without knowing which tiles, user cannot download:
      if (is.null(tile_ids)) {
        msg <- "To download, please specify parameter 'tile_ids'."
        variables$note = msg
        warning(msg) # warning, because user obviously wanted to download...

      # If tiles were specified, we can download:
      } else {
        # Note: The following message is never quiet!
        message("\nStarting download of ", download_bytes/1000000000, " GB...")
        outcome <- do_env90m_download(all_varnames, tile_ids, file_size_table,
            download_dir = download_dir, file_format = file_format,
            quiet = quiet, delete_zips = delete_zips)
        variables <- c(variables, outcome)
      }

    # User does not want to download all variables, just see them:
    } else {
      if (is.null(tile_ids)) {
        msg <- "To download, please specify parameter 'tile_ids', and add 'download=TRUE'."
        variables$note = msg
        if (!(quiet)) message(msg)
      } else {
        msg <- "To download, please specify 'download=TRUE'."
        variables$note = msg
        if (!(quiet)) message(msg)
      }
    }

    return(variables)
  }

  # If user specified a subset, continue with those specified variables that
  # are available:

  ########################################
  ## Restrict to user-requested subset ###
  ########################################

  varnames_to_be_returned <- subset[subset %in% all_varnames]

  # If any are not available, warn or stop:
  if (!(all(subset %in% all_varnames))) {
    not_available <- subset[! subset %in% all_varnames]
    err_msg <- paste0('Not available: Variable(s) ', paste(not_available, collapse=', '))
    if (ignore_missing) {
      message(paste0(err_msg, " Will be ignored...")) # shown right away
      warning(paste0(err_msg, " Was ignored...")) # shown at the end
    } else {
      stop(paste0(err_msg, '. Please check your spelling and try again!'))
    }
  }

  # Construct the list to be returned to the user
  variables <- list(
    comment = "Subset of variables for this dataset.",
    dataset_name = dataset_name,
    variable_names = sort(varnames_to_be_returned)
  )

  # Compute download size, if tile_ids is specified:
  if (!(is.null(tile_ids))) {
    download_bytes <- compute_download_size(varnames_to_be_returned, tile_ids, file_size_table, quiet)
    if (!(quiet)) message(paste0("Info: Download size: ",
      length(varnames_to_be_returned), " variables, ",
      length(tile_ids), " tiles: ",
      download_bytes/1000000000, " GB (", download_bytes, " bytes)."))
    variables$download_bytes=download_bytes
    variables$tile_ids=tile_ids
  }

  # Download the data if requested:
  if (download) {
    if (is.null(tile_ids)) {
      msg <- "To download, please specify parameter 'tile_ids'."
      warning(msg) # warning, because user obviously wanted to download...
      variables$note = note
    }
    # Note: The following message is never quiet!
    message("\nStarting download of ", download_bytes/1000000000, " GB...")
    outcome <- do_env90m_download(varnames_to_be_returned, tile_ids, file_size_table,
        download_dir = download_dir, file_format = file_format,
        quiet = quiet, delete_zips = delete_zips)
    variables <- c(variables, outcome)
  } else {
    if (is.null(tile_ids)){
      msg <- "To download, please specify parameter 'tile_ids', and add 'download=TRUE'."
      variables$note = msg
      if (!(quiet)) message(msg)
    } else {
      msg <- "To download, please specify 'download=TRUE'."
      variables$note = msg
      if (!(quiet)) message(msg)
    }

  }

  # Return the list
  return(variables)
}



###############
### Helpers ###
###############


#' @title Get table of file names, sizes and paths
#' 
#' @description Download and loads the specified file size table from the
#'  server, or reuse existing table if found in the temp directory.
#' 
#' @param file_name String. Name of the table that should be downloaded.
#'  Each Environment90m dataset has one file size table. 
#' @param tempdir String. Path to the directory where to store/look for the
#'  file size table. If not passed, defaults to the output of [base::tempdir()].
#' @returns Data frame that contains file name, size in bytes and file path
#'  (on the server) for all files (data tables) of one specific
#'  Environment90m dataset (e.g. landcover, soil).
#' @param quiet logical. If FALSE, informative messages will be printed.
#'   Default is FALSE.
#' @examples
#' # Download and load the file size table for the landcover dataset:
#' file_size_table <- get_file_size_table(
#'   file_name = "env90m_landcover_paths_file_sizes.txt")
#' 
#' @noRd
get_file_size_table <- function(file_name, tempdir = NULL, quiet = FALSE) {

  # Define tempdir:
  if (is.null(tempdir)) {
    tempdir <- tempdir()
  }

  # Download lookup table with the size of each file
  # if it doesn't exist in the tempdir()
  file_local <- paste0(tempdir, "/", file_name)
  if (!file.exists(file_local)) {
    if (!(quiet)) message(paste('Downloading ', file_name,'...'))
    url <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2FREADME/"
    download.file(paste0(url, file_name), destfile = file_local, mode = "wb",
      quiet = quiet)
  }

  # Import lookup table with the size of each file
  file_size_table <- data.table::fread(file_local, sep = ";")
  file_size_table$file_name = basename(file_size_table$file_path)

  return (file_size_table)
}


#' @title Compute download size and check specified tile ids.
#' 
#' @description
#' Compute the download size by adding up the file sizes in bytes listed in
#' the file size table for each file that corresponds to the specified set of
#' tile ids and variable names. While going through the files, the
#' availability is also checked.
#' 
#' @param variable_names Vector containing all variable names whose size
#'  should be computed, e.g. c("c20_1992", "c20_1993")
#' @param tile_ids Vector containing all tile ids of the tiles (e.g. "h10v04")
#'   whose size should be computed and whose availability should be checked.
#' @param file_size_table Data frame that contains file name, size and file
#'  path (on the server) for all files (data tables) for a specific 
#'  Environment90m dataset. Used for extracting variable names, tile ids,
#'  computing download size and finding the file on the server. No default.
#' @param ignore_missing logical. If TRUE, a warning is given out for missing
#'  tiles. If FALSE, the function stops with an error. Defaults to FALSE.
#' @param quiet logical. If FALSE, informative messages will be printed.
#'   Default is FALSE.
#' @returns Size in bytes of the specified variables and tiles on disk (zipped).
#' @seealso [check_tiles_filesize()] which is used by this function.
#' @examples
#' 
#' # First, download and load the required file size table:
#' file_size_table <- get_file_size_table(
#'   file_name = "env90m_landcover_paths_file_sizes.txt")
#'
#' # Then, compute the download size for all variables and for two tiles:
#' bytes <- compute_download_size(
#'   "ALL",
#'   c("h10v04", "h00v04"),
#'   file_size_table,
#'   quiet = FALSE,
#'   ignore_missing = FALSE
#' )
#' 
#' @noRd
compute_download_size <- function(varnames,
                                  tile_ids,
                                  file_size_table,
                                  ignore_missing = FALSE,
                                  quiet = FALSE) {

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

  for (ivar in varnames) {
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
        h90m_varnames = varnames, # TODO we pass the required ones, not ALL
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
    if (!quiet) message(paste0("Info: Variable '", ivar, "' (", j," tiles): ", byte_sum_one_var/1000000, " MB (", byte_sum_one_var, " bytes)."))

    # Sum up for all variables:
    download_bytes <- download_bytes + byte_sum_one_var
  }

  # Download size for all variables:
  #message(paste0("Overall: ", length(tile_ids), " tiles: ", download_bytes/1000000000, " GB ( ", download_bytes," bytes)."))
  return(download_bytes)
}



#' @title Download Environment90m tables
#' 
#' @description
#' Download zipped tables of the Environment90m dataset from the IGB server,
#' unzip them (if requested) and delete the zipfiles (if requested).
#' 
#' For info about the dataset, the variables and the tile names, please refer
#' the Environment90m publication.
#' 
#' @param variable_names Vector containing all variable names that should be
#'  downloaded, e.g. c("c20_1992", "c20_1993").
#' @param tile_ids Vector containing all tile ids of the tiles (e.g. "h10v04")
#'  whose data should be downloaded.
#' @param file_size_table Data frame that contains file name, size and file
#'  path (on the server) for all files (data tables) for a specific
#'  Environment90m dataset. Used for extracting variable names, tile ids,
#'  computing download size and finding the file on the server. No default.
#' @param download_dir Directory where the downloads should be stored.
#'  Defaults to the current working directory ".".
#' @param file_format File format of the tables, either "txt" or "zip". If
#'  "txt", then the zipped tables are unzipped. If "zip", the downloaded
#'  zipped files are left as they are. Defaults to "txt".
#' @param delete_zips logical. If TRUE, zip files will be deleted after
#'  unzipping. Only works when file_format is "txt".
#' @param quiet logical. If FALSE, informative messages will be printed.
#'   Default is FALSE.
#' @returns A named list with an item named "downlaoded", containing a vector
#'  of the paths of the downloaded files.
#' @seealso [download_tiles_base()] which is used by this function.
#' @examples
#' # First, download and load the required file size table:
#' file_size_table <- get_file_size_table(
#'   file_name = "env90m_landcover_paths_file_sizes.txt")
#' 
#' # Then, download tables for one tile (h10v04),
#' # for two variables (c100_1992, c100_1993).
#' result <- do_download(
#'     c("c100_1992", "c100_1993"),
#'     c("h10v04"),
#'     file_size_table,
#'     file_format = "zip",
#'     delete_zips = FALSE,
#'     quiet = FALSE)
#' 
#' @noRd
do_env90m_download <- function(variable_names, tile_ids, file_size_table, download_dir = ".", file_format = "txt", quiet = FALSE, delete_zips = TRUE) {

  # Set timeout option for download to 4 hours (14400 seconds)
  options(timeout=14400)

  # General path to the download folder at IGB
  igb_path <- "https://public.igb-berlin.de/index.php/s/zw56kEd25NsQqcQ/download?path=%2F"

  # Check availability of IGB server by downloading README
  tryCatch(
    {
      igb_readme = paste0(igb_path, "README/accessibility_check.txt")
      download.file(igb_readme, destfile = paste0(download_dir, "/accessibility_check.txt"),
        mode = "wb", quiet = quiet)
    },
    warning = function(c) {
      message(paste0('Error: Could not download from ', igb_readme, ', maybe the server is down.'))
      # TODO: Do we have to use warning() here?
    },
    error = function(c) {
      message(paste0('Error: Could not download from ', igb_readme, ', maybe the server is down.'))
      # TODO: Do we have to use error() here?
    }
  )

  # Download zip files for all variables, all tiles:
  all_downloaded_zips = c()
  all_result_files = c()
  i <- 0
  n <- length(variable_names)
  for (ivar in variable_names) {
    i <- i+1
    if (!(quiet)) message(paste0('Downloading variable ', i, "/", n, ": ",  ivar))
    
    j <- 0
    m <- length(tile_ids)
    for (itile in tile_ids) {
      j <- j+1
      if (!(quiet)) message(paste0('Downloading variable ', i, "/", n, ", tile: ", j, "/", m, ": ",  itile))

      downloaded_path <- download_tiles_base(
        variable = ivar,
        tile_id = itile,
        file_format = "zip",
        global = FALSE,
        download_dir = download_dir,
        file_size_table = file_size_table,
        server_url = igb_path
      )
      all_downloaded_zips <- c(all_downloaded_zips, downloaded_path)
    }
  }
  if (!(quiet)) message(paste('Downloaded zip files:', paste(all_downloaded_zips, collapse=', ')))
  result <- list(downloaded=all_downloaded_zips)

  # Unzip and delete zipfiles, if requested...
  # TODO: This could start to run in parallel, maybe?
  if (file_format == "txt") {
    if (delete_zips) {
      if (!(quiet)) message("Unzipping and deleting zipfiles...")
    } else {
      if (!(quiet)) message("Unzipping...")
    }
    all_destdirs <- c()
    all_deleted <- c()
    for (zipfile in all_downloaded_zips) {
      destdir <- dirname(zipfile)
      if (!(quiet)) message("Unzipping file ", zipfile, "...")
      utils::unzip(zipfile, exdir = destdir)
      all_destdirs <- c(all_destdirs, destdir)
      if (delete_zips) {
        file.remove(zipfile)
        all_deleted <- c(all_deleted, zipfile)
      }
    }
    result$unzipped <- unique(all_destdirs)
    result$deleted <- all_deleted
  }

  cat("Please cite the Environment90m publication:\n
      Garcia Marquez, J., Amatulli, G., Grigoropoulou, A.,
      Schürz, M., Tomiczek, T., Buurman, M., Bremerich, V.,
      Bego, K. and Domisch, S.:
      Global datasets of aggregated environmental variables at the
      sub-catchment scale for freshwater biodiversity modeling, in prep.
      Please contact the authors for more up-to-date citation info.\n")

  return(result)
}