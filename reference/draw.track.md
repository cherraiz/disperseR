# Plot movement tracks

Draws the trajectory of each individual, with locations optionally
coloured by any column of the data, such as the movement `phase` from
[`class.mov`](https://cherraiz.github.io/disperseR/reference/class.mov.md),
the behavioural `state` from
[`fitHMM.wrap`](https://cherraiz.github.io/disperseR/reference/fitHMM.wrap.md)
or the `cluster_id` from
[`get.arp`](https://cherraiz.github.io/disperseR/reference/get.arp.md).

## Usage

``` r
draw.track(
  data,
  colour.col = NULL,
  ids = NULL,
  id.col = "ID",
  time.col = "time",
  coord.names = c("x", "y"),
  colour.path = FALSE,
  km = FALSE,
  palette = NULL,
  point.size = 0.6
)
```

## Arguments

- data:

  A data.frame or an object of class `disperseR`.

- colour.col:

  Character. Optional name of the column used to colour the locations.
  Default is `NULL` (no colour).

- ids:

  Character vector. Optional selection of individuals to plot. Default
  is `NULL` (all individuals).

- id.col:

  Character. Name of the column containing individual identifiers.
  Default is "ID".

- time.col:

  Character. Name of the column containing timestamps, used to order the
  locations. Default is "time".

- coord.names:

  Character vector of length 2 with the names of the X and Y coordinate
  columns. Default is `c("x", "y")`.

- colour.path:

  Logical. If `TRUE`, the path between locations is also coloured by
  `colour.col`; otherwise it is drawn in grey. Default is `FALSE`.

- km:

  Logical. If `TRUE`, projected coordinates in metres are shown in
  kilometres. Default is `FALSE`.

- palette:

  Optional named character vector of colours for the values of
  `colour.col`. If `NULL`, a default palette is used for the `phase`
  column and the default `ggplot2` palette otherwise.

- point.size:

  Numeric. Size of the location points. Default is 0.6.

## Value

A `ggplot` object.

## Details

With one individual the plot keeps the true aspect ratio
(`coord_equal`). With several individuals each one is drawn in its own
panel with free scales.

## Examples

``` r
draw.track(sim_tracks, time.col = "timestamp", km = TRUE)


eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
mov <- class.mov(link.arp(arp, thres = 24 * 3600))
draw.track(mov, colour.col = "phase", km = TRUE)

draw.track(mov, colour.col = "phase", ids = "M01", colour.path = TRUE, km = TRUE)
```
