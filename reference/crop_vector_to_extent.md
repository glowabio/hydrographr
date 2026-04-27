# Crop vector to extent

This function crops an input vector layer (.shp, .gpkg, .geojson) given
a bounding box (xmin, ymin, xmax, ymax coordinates, or a spatial object
from which to extract a bounding box) or to the boundary of a polygon
vector layer (cutline source). The cropping is performed directly on
disk using ogr2ogr, i.e. the input layer does not need to be loaded into
R. The output is always written to disk, and can be optionally loaded
into R as an sf object (using read = TRUE).

## Usage

``` r
crop_vector_to_extent(
  vector_layer,
  clip_layer = NULL,
  bounding_box = NULL,
  out_dir,
  file_name,
  read = TRUE,
  quiet = TRUE
)
```

## Arguments

- vector_layer:

  character. Full path to the input vector layer (.shp, .gpkg,
  .geojson).

- clip_layer:

  character. Full path to a polygon vector layer used as the clipping
  boundary (similar to a mask operation). Optional.

- bounding_box:

  numeric vector of the coordinates of the corners of a bounding box
  (xmin, ymin, xmax, ymax), SpatRaster, SpatVector, or other spatial
  object.

- out_dir:

  character. The directory where the output will be stored.

- file_name:

  character. Name of the cropped output vector file. Supported formats:
  .gpkg, .shp, .geojson.

- read:

  logical. If TRUE, the cropped vector layer gets read into R as an sf
  object. If FALSE, the layer is only stored on disk. Default is TRUE.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## Value

The function always writes a vector file to disk. Optionally, an sf
object can be loaded into R with read = TRUE.

## Note

Shapefile output (.shp) truncates column names to 10 characters due to
format limitations. Use .gpkg or .geojson to preserve full column names.

## Author

Afroditi Grigoropoulou

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Define full path to the input vector layer
basin_vector <- paste0(my_directory, "/hydrography90m_test_data",
                       "/basin_59.gpkg")

# Crop a vector layer to a bounding box
cropped_sf <- crop_vector_to_extent(
  vector_layer = basin_vector,
  bounding_box = c(8.5, 42.24, 8.8, 42.3),
  out_dir = my_directory,
  file_name = "basin_cropped.gpkg",
  read = TRUE
)
```
