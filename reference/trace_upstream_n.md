# Trace upstream N steps, handling multiple tributaries

Trace upstream N steps, handling multiple tributaries

## Usage

``` r
trace_upstream_n(start_id, df, steps)
```

## Arguments

- start_id:

  Starting segment ID

- df:

  Stream network data.frame with columns subc_id, target

- steps:

  Number of steps to trace upstream

## Value

Vector of segment IDs encountered within N steps upstream
