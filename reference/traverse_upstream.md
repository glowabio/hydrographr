# Traverse upstream along a stream network

Starting from a given segment and position along that segment, this
function traverses upstream along the network for a specified distance,
collecting all segments (or partial segments) encountered. Handles
branching networks by exploring all upstream tributaries.

## Usage

``` r
traverse_upstream(lines_sf, segment_id, start_fraction, max_distance)
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

  Numeric. Maximum distance to traverse upstream (in the same units as
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

The function explores upstream by finding all segments whose \`target\`
points to the current segment. It uses a queue-based approach to handle
multiple upstream branches, avoiding revisiting segments. Partial
segments are included if the distance budget is exhausted partway
through a segment.
