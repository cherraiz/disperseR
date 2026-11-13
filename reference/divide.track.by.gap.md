# Split trajectory by time gap

Splits individual tracking trajectories into multiple segments if the
time difference between consecutive locations exceeds a specified
threshold.

## Usage

``` r
divide.track.by.gap(data, ids, time.col, gap.days)
```

## Arguments

- data:

  A data.frame containing the tracking data.

- ids:

  Character. The name of the column containing individual identifiers.

- time.col:

  Character. The name of the column containing timestamps (must be
  POSIXct or Date).

- gap.days:

  Numeric or named list. The maximum allowed gap in days before
  splitting the track. Can be a single numeric value for all
  individuals, or a named list specifying different thresholds per
  individual.

## Value

A data.frame identical to the input but with an additional column
`id_split` representing the new segmented identifiers.

## Examples

``` r
# Split tracks wherever two consecutive fixes are more than 3 hours apart
split_trk <- divide.track.by.gap(sim_tracks, ids = "ID",
                                 time.col = "timestamp", gap.days = 3 / 24)
table(split_trk$id_split)
#> 
#> F02_1 F02_2 F02_3 F02_4 F04_1 F04_2 F04_3 F04_4 F04_5 F04_6 F04_7 F05_1 F05_2 
#>   173    25    26   514     9   361    93    80    35   164     4    75   203 
#> F05_3 F05_4 F05_5 M01_1 M01_2 M01_3 M01_4 M01_5 M01_6 M01_7 M03_1 M03_2 M03_3 
#>     4     7   121   116   172   145   285    19   310     8    65   119    39 
#> M03_4 
#>   218 
```
