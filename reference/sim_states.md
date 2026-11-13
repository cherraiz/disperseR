# Simulated dispersal tracks with behavioural states

[`sim_tracks`](https://cherraiz.github.io/disperseR/reference/sim_tracks.md)
split at monitoring gaps longer than one day with
[`divide.track.by.gap`](https://cherraiz.github.io/disperseR/reference/divide.track.by.gap.md),
regularised to hourly locations with
[`resample.ctmm`](https://cherraiz.github.io/disperseR/reference/resample.ctmm.md),
and classified by segment with a hidden Markov model using
[`fitHMM.wrap`](https://cherraiz.github.io/disperseR/reference/fitHMM.wrap.md)
with `stationary = TRUE` (the stationary state, mostly daytime rest, is
merged into the encamped state).

## Usage

``` r
sim_states
```

## Format

A data.frame with 10 columns:

- ID:

  Individual identifier.

- id_split:

  Track segment identifier (individual and segment number).

- time:

  Location time (POSIXct, UTC).

- time_s:

  Location time in seconds since 1970-01-01.

- x, y:

  Easting and northing (m, EPSG:25830).

- step:

  Step length (m).

- angle:

  Turning angle (radians).

- state:

  Viterbi-decoded state (1 = Transit, 2 = Encamped).

- sex:

  Static individual attribute.

## Source

Simulated; see `data-raw/simulate_tracks.R` in the package sources.
