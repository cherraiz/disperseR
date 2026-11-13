# Identify Area-Restricted Phase (ARP) clusters

Identifies spatial Area-Restricted Phase (ARP) clusters using DBSCAN
based on spatial proximity, duration, and state classification
proportions.

## Usage

``` r
get.arp(
  data,
  ids = "ID",
  epsilon,
  minPts = 3,
  time.col = "time",
  state.col = "state",
  pct.state1 = NULL,
  delta.days = NULL,
  group.col = NULL
)
```

## Arguments

- data:

  A data.frame or an object of class `disperseR`.

- ids:

  Character. Name of the column containing individual identifiers.
  Default is "ID".

- epsilon:

  Numeric or named list. Spatial distance threshold for DBSCAN. Can be a
  single value, or a named list per individual or group.

- minPts:

  Integer. Minimum number of points required to form a cluster in
  DBSCAN. Default is 3.

- time.col:

  Character. Name of the column containing timestamps (must be POSIXct).
  Default is "time".

- state.col:

  Character. Name of the column containing behavioral or movement state
  classifications. Default is "state".

- pct.state1:

  Numeric. Maximum proportion threshold of state 1 points allowed in a
  cluster. If not provided (NULL), this filter is not applied and a
  warning is triggered.

- delta.days:

  Numeric. Minimum duration threshold in days for a cluster to be
  classified as a ARP area. Duration is computed from the first to the
  last location assigned to the cluster, regardless of intermediate
  locations. If not provided (NULL), this filter is not applied and a
  warning is triggered.

- group.col:

  Character. Optional column name used for grouping when `epsilon` is
  provided as a named list.

## Value

An object of class `disperseR` containing the updated dataset with
cluster assignments, a list of cluster summaries, and list of filtered
ARP clusters.

## Examples

``` r
eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
arp$arp_clusters
#> $F02
#> # A tibble: 4 × 6
#>   cluster_id start               end                 dur_days pct_state1     n
#>        <int> <dttm>              <dttm>                 <dbl>      <dbl> <int>
#> 1          4 2024-03-03 18:56:18 2024-03-11 19:56:18     8.04    0.0103    194
#> 2          7 2024-03-13 18:56:18 2024-03-16 17:56:18     2.96    0.0139     72
#> 3          9 2024-03-17 18:56:18 2024-03-26 19:56:18     9.04    0.00917   218
#> 4         13 2024-03-29 20:56:18 2024-04-04 20:56:18     6       0         142
#> 
#> $F04
#> # A tibble: 2 × 6
#>   cluster_id start               end                 dur_days pct_state1     n
#>        <int> <dttm>              <dttm>                 <dbl>      <dbl> <int>
#> 1          4 2024-03-03 19:02:54 2024-03-29 18:02:54     26.0     0.0370   297
#> 2          6 2024-03-07 01:02:54 2024-04-05 05:02:54     29.2     0.0452   310
#> 
#> $F05
#> # A tibble: 1 × 6
#>   cluster_id start               end                 dur_days pct_state1     n
#>        <int> <dttm>              <dttm>                 <dbl>      <dbl> <int>
#> 1          7 2024-03-08 04:56:34 2024-03-14 18:56:34     6.58    0.00629   159
#> 
#> $M01
#> # A tibble: 4 × 6
#>   cluster_id start               end                 dur_days pct_state1     n
#>        <int> <dttm>              <dttm>                 <dbl>      <dbl> <int>
#> 1          4 2024-03-04 03:00:52 2024-04-19 22:00:52     46.8     0.0318   346
#> 2          5 2024-03-07 00:00:52 2024-03-21 22:00:52     14.9     0.0545   165
#> 3         13 2024-03-26 06:00:52 2024-04-10 20:00:52     15.6     0.143     14
#> 4         14 2024-03-26 19:00:52 2024-04-10 17:00:52     14.9     0.0281   285
#> 
#> $M03
#> # A tibble: 2 × 6
#>   cluster_id start               end                 dur_days pct_state1     n
#>        <int> <dttm>              <dttm>                 <dbl>      <dbl> <int>
#> 1          4 2024-03-03 19:00:38 2024-03-10 21:00:38     7.08    0.0234    171
#> 2          8 2024-03-16 00:00:35 2024-03-24 00:00:35     8       0.00518   193
#> 
```
