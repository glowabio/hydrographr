
# 2. Join sub-catchment IDs with occurrence points ------------------------

# Add subcID column to the given dataset, and exports the overall dataframe in a csv
# The longitude-latitude values should be in the 3rd-4th column respectively!!
get_subcID <- function(taxon, subc_path, dataset_path) {

  dataIDs <- NULL
  IDs <- NULL

  # Call external gdal command of gdallocationinfo()
  IDs <- processx::run(paste0(home_dir, "/scripts/functions/gdallocinfo.sh"),
                       args = c(dataset_path, subc_path),
                       echo = F)$stdout
  # Format the IDs string
  IDs <- data.frame(subcID = strsplit(IDs, "\n", "", fixed = TRUE)[[1]])

  # Import taxon occurrence points
  dataset <- fread(dataset_path, header = T, sep=" ")

  # Join the IDs with the observations
  dataIDs <- cbind.data.frame(dataset, IDs)

  return(dataIDs)

}

# Call the function
dataIDs <- get_subcID(taxon = taxon,
                      subc_path = subc_path,
                      dataset_path = paste0(data_out, taxon, "_points.txt")
)


