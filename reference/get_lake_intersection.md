# Get intersection points between Hydrography90m stream network and lake shapefiles

The function identifies, for each lake ID, the intersection points
between the stream network (i.e., Hydrography90m) and the corresponding
lake's geospatial file (e.g., HydroLAKES shapefile).

## Usage

``` r
get_lake_intersection(
  data,
  lake_id = "HydroLAKES_polys_v10",
  lakes,
  lake_name,
  buffer = TRUE,
  edge,
  stream,
  flow,
  basins,
  lake_dat,
  n_cores = 1,
  quiet = TRUE
)
```

## Arguments

- data:

  a data.frame or data.table that contains the columns regarding the
  lake ids, i.e. HydroLAKES ids. (i.e., output of extract_lake_ids); see
  also help(extract_lake_ids)

- lake_id:

  character. The name of the column containing lake ids.

- lakes:

  character. Full path to lake geo-spatial files; i.e. HydroLAKES
  shapefiles.

- lake_name:

  character. The name of the attribute table column in the geo-spatial
  files containing the lake ids; i.e. "HydroLAKES_polys_v10" for
  HydroLAKES shapefiles.

- buffer:

  character. users can either set buffer to TRUE, FALSE or provide
  buffer size in meters; if TRUE a predefined buffer is used where the
  size of the buffer depends on the lake size; if FALSE no buffer is
  applied; Default is TRUE

- edge:

  character. Full path to GuidosToolbox Workbench MSPA tool see also
  (https://forest.jrc.ec.europa.eu/en/activities/lpa/gtb/)

- stream:

  character. Full path to Hydrography90m stream network tif file add a
  reference were to check the necessary tif files for lake analysis

- flow:

  character. Full path to Hydrography90m flow accumulation tif file

- basins:

  character. Full path to Hydrography90m basin tif file

- lake_dat:

  character. Full path of the output.csv table, i.e., the lake
  intersection table.

- n_cores:

  interger. Number of cores used in parallelization, Default is one.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## References

https://grass.osgeo.org/grass82/manuals/ Soille P. and Vogt P. (2009).
Morphological segmentation of binary patterns. Pattern Recognition
Letters 30, 4:456-459, doi: 1.1016/j.patrec.2008.10.015
<https://ies-ows.jrc.ec.europa.eu/gtb/GTB/MSPA_Guide.pdf>

## Author

Jaime Garcia Marquez, Thomas Tomiczek

## Examples
