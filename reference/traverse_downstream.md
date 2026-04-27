# Traverse downstream along a stream network

Starting from a given segment and position along that segment, this
function traverses downstream along the network for a specified
distance, collecting all segments (or partial segments) encountered.

## Usage

``` r
traverse_downstream(lines_sf, segment_id, start_fraction, max_distance)
```

## Arguments

- lines_sf:

  An \`sf\` object representing the stream network with columns
  \`subc_id\`, \`target\`, and \`length\`.

- segment_id:

  Character or numeric. The starting segment ID.

- start_fraction:

  Numeric between 0 and 1. The fractional position along the starting
  segment (0 = start of segment, 1 = end of segment).

- max_distance:

  Numeric. Maximum distance to traverse downstream (in the same units as
  \`length\` column in \`lines_sf\`).

## Value

A list of segment information, where each element contains:

- subc_id:

  Segment ID

- fraction_start:

  Starting fraction along the segment

- fraction_end:

  Ending fraction along the segment

- length_used:

  Length of segment used in the traversal

## Details

The function follows the network downstream (using the \`target\`
column) until either the maximum distance is reached or there are no
more downstream segments. Partial segments are included if the distance
budget is exhausted partway through a segment.
