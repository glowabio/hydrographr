# Get predict table

This function creates a table by joining the information of one or more
variables available in the Environment90m dataset. These variables need
to be downloaded beforehand for the tiles overlapping with the area of
interest. The subsetting of the original tables occurs by providing a
list of subcatchment ids of interest.

## Usage

``` r
get_predict_table(
  variable,
  statistics = "ALL",
  tile_id,
  input_var_path,
  subcatch_id,
  out_file_path,
  n_cores = NULL,
  read = TRUE,
  quiet = TRUE,
  tempdir = NULL,
  overwrite = FALSE
)
```

## Arguments

- statistics:

  character vector of statistics names. Possible values are "sd",
  "mean", "min", "max", "range" or "ALL". Default "ALL".

- tile_id:

  character. The IDs of the tiles of interest.

- input_var_path:

  path to directory that contains the tables of the Environment90m
  dataset. Tables may be in subdirectories of the provided directory.

- subcatch_id:

  path to a text file with subcatchment ids, or numeric vector
  containing subcatchment ids.

- out_file_path:

  character. The path to the output file to be created.

- n_cores:

  numeric. Number of cores used for parallelization.

- read:

  logical. If TRUE, the table with environmental variables gets read
  into R. If FALSE, the table is only stored on disk. Default is TRUE.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

- tempdir:

  String. Path to the directory where to store/look for the file size
  table. If not passed, defaults to the output of
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html).

- overwrite:

  logical. If TRUE, the output file will be overwritten if it. already
  exists. Useful for repeated testing. Default is FALSE.

- variable(s):

  character vector of variable(s) names. Possible values are: all
  variables in the Environment90m dataset, which can bew viewed by
  calling 'download\_\_tables()'. For more details, see
  '?download_env90m_tables'.

## Value

The function returns a table with

- subcatchment ids (subc_id)

- a column for each descriptive statistic of each variable (eg.
  bio1_mean: mean of the variable bio1)

## Author

Jaime García Márquez, Yusdiel Torres-Cambas

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory) 

# Download one variable, in this case elevation, from the Environment90m dataset
# repository
download_merit_dem_tables(subset="ALL", tile_ids = "h18v04", download = TRUE,
download_dir = my_directory)

# prepare file with list subcatchments of interest
subc_raster <- paste0(my_directory, "subcatchment_1264942.tif")
target_ids <- extract_ids(subc_layer = subc_raster)
fwrite(target_ids, paste0(my_directory, 'subc_IDs.txt'))

# Define variable and tile:
var <- c("elev")
tile_id <- c("h18v04")

# Point to Env90m input data
in_path <- my_directory
# point to sub-catchment id table
subc_ids <- paste0(my_directory, '/hydrography90m_test_data/subc_IDs.txt')
# define output path
output <- paste0(my_directory, '/hydrography90m_test_data/predictTB.csv')

# Run the function with 2 cores
tb <- get_predict_table(variable = var,
                  statistics = c("ALL"),
                  tile_id = tile_id,
                  input_var_path = in_path,
                  subcatch_id = subc_ids,
                  out_file_path = output,
                  read = TRUE, quiet = FALSE,
                  n_cores = 2)

head(tb)
```
