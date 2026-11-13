# Link Area-Restricted Phase (ARP) Clusters

Merges temporally close Area-Restricted Phase (ARP) clusters and
movement gaps based on a specified temporal threshold using graph
components from `igraph`.

## Usage

``` r
link.arp(data, thres = 0, time.col = "time", id.col = "ID")
```

## Arguments

- data:

  An object of class `disperseR` containing movement data and ARP
  cluster definitions.

- thres:

  Numeric. Temporal gap threshold (in seconds) below which adjacent ARP
  clusters will be linked. Default is 0.

- time.col:

  Character. Name of the column containing timestamp or date-time
  values. Default is `"time"`.

- id.col:

  Character. Name of the column containing individual identifiers.
  Default is `"ID"`.

## Value

An updated object of class `disperseR` containing linked cluster IDs and
associated graph network structures under `cluster_graphs`.

## Examples

``` r
eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
# Link ARP clusters separated by less than 24 hours
linked <- link.arp(arp, thres = 24 * 3600)
table(linked$data$ID, linked$data$linked_clusters)
#>      
#>       arp_1 arp_2 arp_3 arp_4 mov_1 mov_3 mov_5 mov_7
#>   F02   194    72   218   145    55    46    24    72
#>   F04   649     0     0     0    54    72    58     0
#>   F05   159     0     0     0   161   129     0     0
#>   M01   551   303     0     0    63   103    72    95
#>   M03   171   193     0     0    55    74     0     0
```
