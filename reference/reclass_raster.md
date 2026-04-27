# Reclassify a raster layer

Reclassifies a raster .tif layer based on a look-up table, such that the
output raster contains the new values. The function uses the r.reclass
function of GRASS GIS.

Note that the input raster needs to be of type integer. If the input
raster layer has floating point values, you can multiply it by some
factor (e.g. 1000) to achieve integer values, otherwise the GRASS GIS
r.reclass will round the raster values down to the next integer which is
not always desired.

## Usage

``` r
reclass_raster(
  data = NULL,
  rast_val = NULL,
  new_val = NULL,
  remaining = NULL,
  remaining_value = -9999,
  raster_layer,
  recl_layer,
  no_data = -9999,
  type = "Int32",
  compression = "low",
  bigtiff = TRUE,
  read = FALSE,
  quiet = TRUE
)
```

## Arguments

- data:

  a data.frame or data.table with the original and new values to be
  written to the raster.

- rast_val:

  character. The name of the column with the original raster values.

- new_val:

  character. The name of the column with the new raster values, which
  need to be integer values. In case of floating point values, consider
  multiplying the values e.g. by 1000 to keep three decimals.

- remaining:

  character. How to treat raster values not listed in the
  reclassification table: \`"same"\` retains their original values
  (equivalent to \`\* = \*\` in GRASS), \`"value"\` assigns a fixed
  value (\`remaining_value\`), and \`NULL\` (default) does nothing. When
  \`remaining = "same"\`, \`remaining_value\` is overlooked.

- remaining_value:

  numeric. Value to assign if \`remaining = "value"\`. Default is -9999.

- raster_layer:

  Full path to the input raster .tif layer.

- recl_layer:

  character. Full path of the output .tif layer, i.e., the reclassified
  raster file.

- no_data:

  numeric. The no_data value of the new .tif layer. Default is -9999.

- type:

  character. Data type; Options are Byte, Int16, UInt16, Int32,
  UInt32,CInt16, CInt32. Default is Int32. Note that only integer values
  are allowed.

- compression:

  character. Compression of the written output file. Compression levels
  can be defined as "none", "low", or "high". Default is "low",
  referring to compression type "DEFLATE" and compression level 2.
  "high" refers to compression level 9.

- bigtiff:

  logical. Define whether the output file is expected to be a BIGTIFF
  (file size larger than 4 GB). If FALSE and size \> 4GB no file will be
  written. Default is TRUE.

- read:

  logical. If TRUE, then the reclassified raster .tif layer gets read
  into R as a SpatRaster (terra object). If FALSE, the layer is only
  stored on disk. Default is FALSE.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## References

https://grass.osgeo.org/grass82/manuals/r.reclass.html

## Author

Marlene Schürz, Afroditi Grigoropoulou

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Read the stream order for each sub-catchment as a data.table
my_dt <- read_geopackage(gpkg= paste0(my_directory,
                                         "/hydrography90m_test_data",
                                         "/order_vect_59.gpkg"),
                         import_as = "data.table")


# Select the stream segment ID and and the Strahler stream order
str_ord <- my_dt[,c("stream", "strahler")]

# Define input and output raster layer
stream_raster <- paste0(my_directory,
                        "/hydrography90m_test_data/stream_1264942.tif")

recl_raster <- paste0(my_directory,
                      "/hydrography90m_test_data/reclassified_raster.tif")

# Reclassify the stream network to obtain the Strahler stream order raster
str_ord_rast <- reclass_raster(data = str_ord,
                               rast_val = "stream",
                               new_val = "strahler",
                               raster_layer = stream_raster,
                               recl_layer = recl_raster)

# Reclassify the raster to obtain a mask, where every value is converted to '1'
mask_rast <- reclass_raster(data = NULL,
                           rast_val = NULL,
                           new_val = NULL,
                           remaining = "value",
                           remaining_value = 1,
                           raster_layer = stream_raster,
                           recl_layer = recl_raster)


# Reclassify the raster to only a subset of the Strahler stream order values,
# while maintaining the rest of the values unchanged
mask_rast <- reclass_raster(data = str_ord[1:1000,],
                           rast_val = stream,
                           new_val = strahler,
                           remaining = "same",
                           remaining_value = NULL,
                           raster_layer = stream_raster,
                           recl_layer = recl_raster)

```
