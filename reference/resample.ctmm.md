# Resample tracking data using Continuous-Time Movement Models (CTMM)

Fits continuous-time movement models to individual tracking data and
predicts locations at regular time intervals. It handles coordinate
reprojection, model selection, and retains static individual traits.

## Usage

``` r
resample.ctmm(
  dat,
  tint,
  ids = "ID",
  timestamp = "timestamp",
  x = "x",
  y = "y",
  proj.in,
  proj.out = proj.in,
  static.cols = NULL,
  verbose = TRUE
)
```

## Arguments

- dat:

  A data.frame containing the tracking data.

- tint:

  Numeric. The desired time interval for resampling, in seconds.

- ids:

  Character. The name of the column containing individual identifiers.
  Default is "ID".

- timestamp:

  Character. The name of the column containing timestamps. Default is
  "timestamp".

- x:

  Character. The name of the column containing the X coordinate or
  Longitude. Default is "x".

- y:

  Character. The name of the column containing the Y coordinate or
  Latitude. Default is "y".

- proj.in:

  An object of class `crs` (from `sf`), an EPSG code, or a PROJ4 string
  representing the input coordinate reference system.

- proj.out:

  An object of class `crs` (from `sf`), an EPSG code, or a PROJ4 string
  representing the output coordinate reference system. Must be in
  meters. Defaults to `proj.in`.

- static.cols:

  Character vector. Optional names of columns containing static
  individual information (e.g., sex, species) to append to the resampled
  dataset. Default is NULL.

- verbose:

  Logical. If TRUE, prints progress and model summaries to the console.
  Default is TRUE.

## Value

A data.frame with the resampled trajectories, including predicted X and
Y coordinates, time in POSIXct format, time in numeric seconds, and any
specified static columns.

## Examples

``` r
# \donttest{
# Regularise the first 150 fixes of one individual to hourly locations
trk <- sim_tracks[sim_tracks$ID == "F02", ][1:150, ]
reg <- resample.ctmm(trk, tint = 3600, ids = "ID", timestamp = "timestamp",
                     proj.in = 25830, static.cols = "sex", verbose = FALSE)
#> Minimum sampling interval of 51.4 minutes in unknown
head(reg)
#>         ID        x       y                time     time_s    sex
#> 1056   F02 421007.4 4332970 2024-03-01 11:56:18 1709294178 female
#> 1056.1 F02 421003.0 4332981 2024-03-01 12:56:18 1709297778 female
#> 1056.2 F02 420998.6 4332980 2024-03-01 13:56:18 1709301378 female
#> 1056.3 F02 420984.0 4332982 2024-03-01 14:56:18 1709304978 female
#> 1056.4 F02 420977.1 4332973 2024-03-01 15:56:18 1709308578 female
#> 1056.5 F02 420937.5 4332964 2024-03-01 16:56:18 1709312178 female
# }
```
