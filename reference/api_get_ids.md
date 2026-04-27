# Get Subcatchment/Geographic IDs

Retrieves subcatchment IDs and related geographic identifiers from the
GeoFRESH API. Three modes cover distinct spatial queries:

## Usage

``` r
api_get_ids(
  points = NULL,
  basin_ids = NULL,
  subc_ids = NULL,
  mode = "basin",
  min_strahler = NULL,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "site_id",
  colname_subc_id = NULL,
  comment = NULL,
  force_async = TRUE,
  poll_interval = 10,
  max_wait = 3600
)
```

## Arguments

- points:

  Data.frame. Input sites. Used in all three modes: - mode="basin": must
  contain longitude and latitude columns (and optionally site_id). Can
  alternatively supply \`basin_ids\` or \`subc_ids\` directly. -
  mode="upstream": must contain longitude, latitude, and site_id
  columns. - mode="local": must contain either longitude + latitude
  columns, or a subc_id column. Returns one row of IDs per input row.

- basin_ids:

  Integer or vector. Basin IDs (mode="basin" only). Alternative to
  \`points\` or \`subc_ids\`.

- subc_ids:

  Integer or vector. Subcatchment IDs (mode="basin" only). Alternative
  to \`points\` or \`basin_ids\`.

- mode:

  Character. One of "basin", "upstream", or "local". Default: "basin".

- min_strahler:

  Integer. Minimum Strahler order filter (mode="basin" and
  mode="upstream" only). Default: NULL.

- colname_lon:

  Character. Name of the longitude column in \`points\`. Default:
  "longitude".

- colname_lat:

  Character. Name of the latitude column in \`points\`. Default:
  "latitude".

- colname_site_id:

  Character. Name of the site ID column in \`points\`. Default:
  "site_id".

- colname_subc_id:

  Character. Name of an existing subcatchment ID column in \`points\`
  (mode="local" only). If provided, the API uses these directly instead
  of looking up from coordinates. Default: NULL.

- comment:

  Character. Optional comment for API logging. Default: NULL.

- force_async:

  Logical. Force async (TRUE) or sync (FALSE) mode. Default: TRUE.

- poll_interval:

  Numeric. Seconds between status checks. Default: 10.

- max_wait:

  Numeric. Max seconds to wait for async job. Default: 3600.

## Value

\- mode="basin": Integer vector of all subcatchment IDs in the
basin(s). - mode="upstream": Data.frame with columns \`site_id\`,
\`subc_id\`, \`upstream_id\` (one row per upstream subcatchment per
site). - mode="local": Data.frame with one row per input point,
containing \`subc_id\`, \`basin_id\`, and \`reg_id\`.

## Details

\- \*\*"basin"\*\*: returns all subcatchment IDs contained within the
basin(s) of the given locations. - \*\*"upstream"\*\*: returns all
subcatchment IDs upstream of each input point. - \*\*"local"\*\*:
returns the local identifiers (subc_id, basin_id, reg_id) for each input
point or subcatchment — i.e. the reverse lookup.

## See also

Other ocgapi: [`api_get_basin_polygon()`](api_get_basin_polygon.md),
[`api_get_snapped_points()`](api_get_snapped_points.md),
[`api_get_snapped_points_cascade()`](api_get_snapped_points_cascade.md),
[`api_get_stream_segments()`](api_get_stream_segments.md),
[`api_get_upstream_catchment()`](api_get_upstream_catchment.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basin mode: get all subcatchments in a basin
result <- api_get_ids(basin_ids = 1288419, mode = "basin")

# Basin mode: from snapped points dataframe
result <- api_get_ids(
  points          = all_snapped,
  colname_lon     = "longitude_snapped",
  colname_lat     = "latitude_snapped",
  colname_site_id = "site_id",
  mode            = "basin"
)

# Upstream mode: all upstream subcatchments per site
sites <- data.frame(site_id = "S1", longitude = 9.93, latitude = 54.70)
result <- api_get_ids(points = sites, mode = "upstream")

# Local mode: get subc_id, basin_id, reg_id for each point
sites <- data.frame(longitude = c(10.7, 9.9), latitude = c(53.5, 54.7))
result <- api_get_ids(points = sites, mode = "local")

# Local mode: from existing subc_ids column
result <- api_get_ids(
  points         = my_df,
  colname_subc_id = "subc_id",
  mode           = "local"
)
} # }
```
