# Set no data value

Change or set the NoData value for a raster layer. The change happens
in-place, meaning that the original file is overwritten on disk.

## Usage

``` r
set_no_data(data_dir, var_layer, no_data, quiet = TRUE)
```

## Arguments

- data_dir:

  character. Path to the directory containing all input data.

- var_layer:

  character vector of variable layers on disk, e.g. c("sti_h16v02.tif",
  "slope_grad_dw_cel_h00v00.tif"). The original files will be
  overwritten.

- no_data:

  numeric. The desired NoData value.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## References

<https://gdal.org/programs/gdal_edit.html>

## Author

Afroditi Grigoropoulou, Marlene Schürz

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Define no data value
set_no_data(data_dir = paste0(my_directory, "/hydrography90m_test_data"),
            var_layer = "cti_1264942.tif",
            no_data = -9999)
```
