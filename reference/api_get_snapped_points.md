# Snap Points to the River Network

Snaps input point locations to the nearest stream segment at or above a
given Strahler order, using the GeoFRESH API.

## Usage

``` r
api_get_snapped_points(
  data,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "site_id",
  min_strahler = NULL,
  add_distance = TRUE,
  force_async = NULL,
  async_threshold = 500,
  poll_interval = 10,
  max_wait = 3600
)
```

## Arguments

- data:

  Data.frame. Must contain columns for longitude, latitude, and a site
  identifier.

- colname_lon:

  Character. Name of the longitude column. Default: "longitude".

- colname_lat:

  Character. Name of the latitude column. Default: "latitude".

- colname_site_id:

  Character. Name of the site ID column. Default: "site_id".

- min_strahler:

  Integer. Minimum Strahler order to snap to. Required.

- add_distance:

  Logical. If TRUE, includes a \`distance_metres\` column with the
  snapping distance. Default: TRUE.

- force_async:

  Logical. If NULL (default), async mode is used automatically for
  datasets \>= \`async_threshold\`. Set TRUE or FALSE to force a mode
  regardless of dataset size.

- async_threshold:

  Integer. Number of points above which async mode is used
  automatically. Default: 500.

- poll_interval:

  Numeric. Seconds between status checks when polling async jobs.
  Default: 10.

- max_wait:

  Numeric. Maximum seconds to wait for async job completion. Default:
  3600.

## Value

Data.frame with one row per snapped point, containing: - All original
columns from \`data\` - \`longitude_snapped\`, \`latitude_snapped\`:
snapped coordinates - \`subc_id\`: subcatchment ID of the snapped
segment - \`strahler\`: Strahler order of the snapped segment -
\`distance_metres\`: distance from original to snapped point (if
\`add_distance = TRUE\`)

## See also

Other ocgapi: [`api_get_basin_polygon()`](api_get_basin_polygon.md),
[`api_get_ids()`](api_get_ids.md),
[`api_get_snapped_points_cascade()`](api_get_snapped_points_cascade.md),
[`api_get_stream_segments()`](api_get_stream_segments.md),
[`api_get_upstream_catchment()`](api_get_upstream_catchment.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sites <- data.frame(
  site_id   = c("S1", "S2", "S3"),
  longitude = c(9.931555, 9.921555, 9.941555),
  latitude  = c(54.695070, 54.295070, 54.495070)
)

# Snap to streams with Strahler >= 4
result <- api_get_snapped_points(data = sites, min_strahler = 4)

# Non-default column names
result <- api_get_snapped_points(
  data           = sites,
  colname_lon    = "decimalLongitude",
  colname_lat    = "decimalLatitude",
  colname_site_id = "gbifID",
  min_strahler   = 4
)
} # }
```
