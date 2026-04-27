# Merge raster or vector objects

Merge multiple raster or spatial vector objects from disk to form a new
raster or spatial vector object with a larger spatial extent. A
directory with at least two raster .tif or spatial vector geopackage
files should be provided. Depending on the input, the output is a .tif
or a .gpkg file (saved under out_dir). If read = TRUE, the output is
read into R as a SpatRaster (terra package) object in case of .tif
files, or as a SpatVector (terra package) object in case of .gpkg files.

## Usage

``` r
merge_tiles(
  tile_dir,
  tile_names,
  out_dir,
  file_name,
  name = "stream",
  compression = "low",
  bigtiff = TRUE,
  read = FALSE,
  quiet = TRUE
)
```

## Arguments

- tile_dir:

  character. The directory containing the raster or spatial vectors
  tiles, which should be merged.

- tile_names:

  character. The names of the files to be merged, including the file
  extension (.tif or .gpkg).

- out_dir:

  character. The directory where the output will be stored.

- file_name:

  character. Name of the merged output file, including the file
  extension (.tif or .gpkg).

- name:

  character. The attribute table column name of the stream segment
  ("stream"), sub-catchment ("ID"), basin ("ID") or outlet ("ID") column
  which is used for merging GeoPackages. Default is "stream".

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

  logical. If TRUE, the merged layer gets read into R. If FALSE, the
  layer is only stored on disk. Default is FALSE.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## Value

A .tif raster file or .gpkg spatial vector object that is always written
to disk, and optionally loaded into R.

## References

<https://gdal.org/programs/gdalbuildvrt.html>

<https://gdal.org/programs/gdal_translate.html>

<https://gdal.org/programs/ogrmerge.html#ogrmerge>

<https://gdal.org/programs/ogr2ogr.html>

## Author

Thomas Tomiczek, Jaime Garcia Marquez, Afroditi Grigoropoulou

## Examples

``` r
# Download tiles into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_tiles(variable = "basin",
               file_format = "tif",
               tile_id = c("h22v08","h22v10"),
               download_dir = my_directory)

# Define folder containing only the tiles, which should me merged
tiles_folder <- paste0(my_directory, "/r.watershed/basin_tiles20d")
# Define output folder
output_folder <- paste0(my_directory, "/merged_tiles")
# Create output folder if it doesn't exist
if(!dir.exists(output_folder)) dir.create(output_folder)


# Merge tiles
merged_tiles <- merge_tiles(tile_dir = tiles_folder,
                            tile_names = c("basin_h22v08.tif", "basin_h22v10.tif"),
                            out_dir = output_folder,
                            file_name = "basin_merged.tif",
                            read = TRUE)
```
