# Split table

Split the table along a set number of rows into multiple parts of equal
length.

Note that duplicated rows will be removed.

## Usage

``` r
split_table(data, split = NULL, split_tbl_path, read = FALSE, quiet = TRUE)
```

## Arguments

- data:

  a data.frame or data.table

- split:

  numeric. number of rows selected to split the table

- split_tbl_path:

  character. Full path to store the split tables

- read:

  logical. If TRUE, then the split data tables get read into R as a data
  tables. If FALSE, the tables are only stored on disk. Default is
  FALSE.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## Author

Jaime Garcia Marquez, Thomas Tomiczek

## Examples

``` r
# Create data table

df <- data.frame(matrix(ncol = 2, nrow = 10000))
colnames(df) <- c('var1', 'var2')
# or with real data
my_directory <- tempdir()
download_test_data(my_directory)
df <- fread(paste0(my_directory, '/projectionTB.csv'), fill=TRUE)

# Define full path to store split data tables
split_tbl_path <- paste0(my_directory,
                     "/hydrography90m_test_data/")
# Split data table
hydrography90m_ids <- split_table(df, split = 20000, split_tbl_path)

# Show the output table
hydrography90m_ids
```
