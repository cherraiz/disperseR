# Simulated dispersal tracks

Simulated GPS tracks of five dispersing individuals with hourly fixes,
timing jitter and about 10\\ resting during the day, also while
dispersing); directed movements are biased correlated random walks, and
some of them include short stopovers (0.5-1.5 days). Coordinates are in
ETRS89 / UTM zone 30N (EPSG:25830), in metres.

- `"M01"`: settles in an area made of two nearby patches, shuttling
  between them, moves to a second area with a stopover on the way, makes
  a looping excursion and returns to the first area (with another
  stopover).

- `"F02"`: settles in one area, makes a 2.5-day stay and settles in two
  more areas, with a stopover before the last one.

- `"M03"`: settles in one area and makes a long transit with a
  monitoring gap of about two days.

- `"F04"`: home range made of two nearby patches, with two looping
  excursions.

- `"F05"`: explores with stopovers, settles briefly, departs again and
  the tracking ends while it is still moving.

## Usage

``` r
sim_tracks
```

## Format

A data.frame with 6 columns:

- ID:

  Individual identifier.

- timestamp:

  Fix time (POSIXct, UTC).

- x, y:

  Easting and northing (m).

- sex:

  Static individual attribute.

- behaviour:

  Simulated behaviour: `"stay"` (area-restricted stay, including trips
  between nearby patches), `"stopover"` or `"travel"` (directed
  movement, including its daytime rests).

## Source

Simulated; see `data-raw/simulate_tracks.R` in the package sources.

## See also

[`sim_states`](https://cherraiz.github.io/disperseR/reference/sim_states.md)
