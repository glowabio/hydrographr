# Crop raster to extent

This function crops an input raster layer (.tif) given a bounding box
(xmin, ymin, xmax, ymax coordinates, or a spatial object from which to
extract a bounding box) or to the boundary of a polygon vector layer
(cutline source). The cropping is performed directly on disk, i.e. the
input layer does not need to be loaded into R. The output is always
written to disk, and can be optionally loaded into R as a SpatRaster
(terra package) object (using read = TRUE).

## Usage

``` r
crop_to_extent(
  raster_layer,
  vector_layer = NULL,
  bounding_box = NULL,
  out_dir,
  file_name,
  compression = "low",
  bigtiff = TRUE,
  read = TRUE,
  quiet = TRUE
)
```

## Arguments

- raster_layer:

  character. Full path to the input raster .tif layer.

- vector_layer:

  character. Full path to a vector layer that is used as a cutline data
  source (similar to a mask operation).

- bounding_box:

  numeric vector of the coordinates of the corners of a bounding box
  (xmin, ymin, xmax, ymax), SpatRaster, SpatVector, or other spatial
  object.

- out_dir:

  character. The directory where the output will be stored.

- file_name:

  character. Name of the cropped output raster .tif file.

- compression:

  character. Compression of the written output file. Compression levels
  can be defined as "none", "low", or "high". Default is "low".

- bigtiff:

  logical. Define whether the output file is expected to be a BIGTIFF
  (file size larger than 4 GB). If FALSE and size \> 4GB no file will be
  written. Default is TRUE.

- read:

  logical. If TRUE, the cropped raster .tif layer gets read into R. If
  FALSE, the layer is only stored on disk. Default is TRUE.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## Value

The function returns always a .tif raster file written to disk.
Optionally, a SpatRaster (terra object) can be loaded into R with read =
TRUE.

## Author

Yusdiel Torres-Cambas

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Define full path to the input raster .tif layer ans vector layer
spi_raster <- paste0(my_directory, "/hydrography90m_test_data",
                     "/spi_1264942.tif")
basin_vector <- paste0(my_directory, "/hydrography90m_test_data",
                      "/basin_59.gpkg")

# Crop the Stream Power Index to the basin
spi_basin <- crop_to_extent(raster_layer = spi_raster,
                            vector_layer = basin_vector,
                            out_dir = my_directory,
                            file_name = "spi_basin_cropped.tif",
                            read = TRUE)
```
