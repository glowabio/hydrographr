# Extract partial stream network needed for snapped points

Given a stream network and snapped point subcatchments, this returns the
reduced stream network consisting of: - All segments with Strahler order
\>= \`strahler_retain_threshold\` - All downstream paths from
finer-order snapped segments until reaching a segment with Strahler \>=
\`strahler_retain_threshold\` - PLUS a configurable number of
\*upstream\* same-Strahler segments for context

## Usage

``` r
extract_partial_stream_network(
  stream,
  snapped_subcs,
  strahler_retain_threshold = 4,
  upstream_buffer = 0
)
```

## Arguments

- stream:

  sf object with columns: \`subc_id\`, \`target\`, \`strahler_order\`,
  and LINESTRING geometry.

- snapped_subcs:

  integer vector: subc_ids after snapping.

- strahler_retain_threshold:

  integer: minimum Strahler to fully retain (default 4).

- upstream_buffer:

  integer: how many same-Strahler upstream segments to retain.

## Value

sf object containing the partial stream network.
