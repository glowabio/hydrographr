# Trace downstream until reaching a Strahler threshold

Trace downstream until reaching a Strahler threshold

## Usage

``` r
trace_downstream(start_id, df, strahler_retain_threshold)
```

## Arguments

- start_id:

  Starting segment ID

- df:

  Stream network data.frame with columns subc_id, target, strahler_order

- strahler_retain_threshold:

  Minimum Strahler order to stop at

## Value

Vector of segment IDs from start_id downstream to threshold
