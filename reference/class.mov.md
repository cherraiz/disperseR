# Classify movement phases

Classifies movement trajectories into distinct functional phases (i.e.,
ARP, Type 0, Type 1, Type 2, Type 3) based on Area-Restricted Phase
(ARP) patches, spatial links, and site fidelity, based on network
complexity.

## Usage

``` r
class.mov(
  data,
  id.col = "ID",
  time.col = "time",
  establish = FALSE,
  establish.days = NULL,
  arp.links = "arp"
)
```

## Arguments

- data:

  An object of class `disperseR` containing movement data and linked ARP
  clusters.

- id.col:

  Character. Name of the column containing individual identifiers.
  Default is "ID".

- time.col:

  Character. Name of the column containing timestamp or date-time
  values. Default is `"time"`.

- establish:

  Logical. If `TRUE`, applies site fidelity corrections requiring a
  minimum duration at a ARP site to consider it established. Default is
  `FALSE`.

- establish.days:

  Numeric. Minimum duration threshold in days for an ARP cluster to be
  considered an established site. Required if `establish = TRUE`. The
  duration of a stay goes from the first to the last location of
  consecutive ARP blocks joined by
  [`link.arp()`](https://cherraiz.github.io/disperseR/reference/link.arp.md),
  including the linking locations between them, so a linked ARP made of
  several short visits counts as one stay.

- arp.links:

  Character. Method for evaluating links between ARPs. Either `"arp"` or
  `"mcp"` (Minimum Convex Polygon). Default is `"arp"`.

  - `"arp"`: Classifies locations sequence of ARP cluster links as ARPs.

  - `"mcp"`: Calculates a Minimum Convex Polygon (MCP) around the points
    of the preceding ARP cluster. It evaluates if the sequence of ARP
    cluster links geometrically falls within this polygon to refine the
    classification. If so, they are considered ARPs. Otherwise they are
    considered Type 1. Requires `x` and `y` coordinate columns.

## Value

An object of class `disperseR` with an updated dataset containing the
classified `phase` and `cluster_established` columns.

## Details

The movement phases follow a 0-3 hierarchy based on spatial network
memory:

- **Type 0:** Unanchored movement. Occurs before the first ARP is
  established.

- **Type 1:** Loop / Local fidelity. Origin and destination ARPs are
  identical.

- **Type 2:** Network expansion. Movement from a known ARP to a newly
  discovered ARP.

- **Type 3:** Internal transit. Movement between two different,
  previously known ARPs.

## Examples

``` r
eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
linked <- link.arp(arp, thres = 24 * 3600)

mov <- class.mov(linked)
table(mov$data$ID, mov$data$phase)
#>      
#>       ARP Type 0 Type 1 Type 2 Type 2 or Type 1 Type 3
#>   F02 629     55      0    142                0      0
#>   F04 649     54    130      0                0      0
#>   F05 159    161      0      0              129      0
#>   M01 854     63     72    103                0     95
#>   M03 364     55      0     74                0      0

# Require at least 3 days in an ARP for it to count as established
mov_est <- class.mov(linked, establish = TRUE, establish.days = 3)
table(mov_est$data$ID, mov_est$data$phase)
#>      
#>       ARP Type 0 Type 1 Type 2 Type 2 or Type 1 Type 3
#>   F02 557     55      0    214                0      0
#>   F04 649     54    130      0                0      0
#>   F05 159    161      0      0              129      0
#>   M01 854     63     72    103                0     95
#>   M03 364     55      0     74                0      0
```
