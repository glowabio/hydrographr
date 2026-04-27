# Report NoData value

This function reports the defined NoData value of a raster layer. The
NoData value of a raster layer represents the absence of data. In
computations the NoData value can be treated in different ways. Either
the NoData value will be reported or the Nodata value will be ignored
and a value is computed from the available values of a specified
location.

## Usage

``` r
report_no_data(data_dir, var_layer, n_cores = NULL)
```

## Arguments

- data_dir:

  character. Path to the directory containing all input data.

- var_layer:

  character vector of variable raster layers on disk, e.g.
  "slope_grad_dw_cel_h00v00.tif".

- n_cores:

  numeric. Number of cores used for parallelization, in case multiple
  .tif files are provided to var_layer.

## References

<https://gdal.org/programs/gdalinfo.html>

## Author

Afroditi Grigoropoulou, Marlene Schürz

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Report the NoData value
report_no_data(data_dir = paste0(my_directory, "/hydrography90m_test_data"),
               var_layer = c("subcatchment_1264942.tif", "flow_1264942.tif",
                             "spi_1264942.tif"),
               n_core = 2)
```
