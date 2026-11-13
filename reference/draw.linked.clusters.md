# Plot linked Area-Restricted Phase (ARP) clusters

Generates a spatial plot of Area-Restricted Phase (ARP) clusters and the
movement links between them. It visualizes ARPs as convex hulls,
highlights ARP points, and draws the network edges connecting them.

## Usage

``` r
draw.linked.clusters(data, ids = NULL, grid_spacing = NULL, id.col = "ID")
```

## Arguments

- data:

  An object of class `disperseR` containing movement data with ARP
  classifications.

- ids:

  Character vector. A selection of individual IDs to plot. If `NULL`
  (default), plots all individuals.

- grid_spacing:

  Numeric. Spacing for background reference grid lines. If `NULL`
  (default), no grid lines are drawn.

- id.col:

  Character. Name of the column containing individual identifiers.
  Default is "ID".

## Value

A `ggplot` object showing the spatial distribution and network of ARPs.

## Examples

``` r
eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
linked <- link.arp(arp, thres = 24 * 3600)
draw.linked.clusters(linked)

draw.linked.clusters(linked, ids = "M01", grid_spacing = 5000)
```
