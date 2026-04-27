# Get Upstream Catchment Polygon (Dissolved)

This function returns a single dissolved polygon representing the merged
area of all sub-catchments upstream of a specified location. The
location can be identified by one of: \`subc_id\`, a pair of coordinates
(\`lon\` + \`lat\`), or a GeoJSON \`point\`. All inputs are sent as
JSON.

## Usage

``` r
api_get_upstream_catchment(
  subc_id = NULL,
  lon = NULL,
  lat = NULL,
  point = NULL,
  add_upstream_ids = FALSE,
  comment = NULL
)
```

## Arguments

- subc_id:

  Integer. A sub-catchment ID from which to trace upstream. One of
  \`subc_id\`, (\`lon\` + \`lat\`), or \`point\` must be provided.

- lon:

  Numeric. Longitude of a point (used together with \`lat\`).

- lat:

  Numeric. Latitude of a point (used together with \`lon\`).

- point:

  List. A GeoJSON Point geometry or Feature. Alternative to providing
  \`lon\` + \`lat\`. Optional.

- add_upstream_ids:

  Logical. If \`TRUE\`, upstream sub-catchment IDs are included in the
  output. Defaults to \`FALSE\`.

- comment:

  Character. Optional comment for API logging.

## Value

An \`sf\` object representing the dissolved upstream catchment polygon.

## Details

Sends a request to the GeoFRESH API to retrieve the dissolved upstream
catchment polygon for a given point or sub-catchment and returns it as
an \`sf\` object.

## See also

Other ocgapi: [`api_get_basin_polygon()`](api_get_basin_polygon.md),
[`api_get_ids()`](api_get_ids.md),
[`api_get_snapped_points()`](api_get_snapped_points.md),
[`api_get_snapped_points_cascade()`](api_get_snapped_points_cascade.md),
[`api_get_stream_segments()`](api_get_stream_segments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Using lon/lat (Sarantaporos outlet)
catchment_sf <- api_get_upstream_catchment(
  lon = 20.538704,
  lat = 40.113735,
  add_upstream_ids = TRUE
)

# Using subc_id
catchment_sf <- api_get_upstream_catchment(
  subc_id = 506586041
)

# Using a GeoJSON point
catchment_sf <- api_get_upstream_catchment(
  point = list(
    type = "Feature",
    geometry = list(
      type = "Point",
      coordinates = c(20.538704, 40.113735)
    )
  )
)

library(leaflet)
leaflet(catchment_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "blue")
} # }
```
