# Summarise movement phases

Builds a table with one row per movement phase of each individual, as
classified by
[`class.mov`](https://cherraiz.github.io/disperseR/reference/class.mov.md):
its start, end, duration, number of locations, distance travelled, net
displacement and, for movements, the ARPs where they start and end.

## Usage

``` r
phase.summary(
  data,
  id.col = "ID",
  time.col = "time",
  phase.col = "phase",
  coord.names = c("x", "y")
)
```

## Arguments

- data:

  An object of class `disperseR` returned by
  [`class.mov`](https://cherraiz.github.io/disperseR/reference/class.mov.md),
  or a data.frame with the same columns.

- id.col:

  Character. Name of the column containing individual identifiers.
  Default is "ID".

- time.col:

  Character. Name of the column containing timestamps (POSIXct). Default
  is "time".

- phase.col:

  Character. Name of the column containing the movement phases. Default
  is "phase".

- coord.names:

  Character vector of length 2 with the names of the X and Y coordinate
  columns (projected, in metres). Default is `c("x", "y")`.

## Value

A data.frame with columns `ID` (named as `id.col`), `n_phase`, `phase`,
`arp`, `from`, `to`, `start`, `end`, `duration_days`, `n_locations`,
`distance_km` and `displacement_km`.

## Details

Consecutive locations with the same phase form one row. Consecutive ARP
locations belonging to different linked clusters (`linked_clusters`)
form separate rows. `distance_km` is the sum of the distances between
consecutive locations of the phase, and `displacement_km` the
straight-line distance between its first and last location. `from` and
`to` give the ARP (`linked_clusters`) before and after each phase.

## Examples

``` r
eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
mov <- class.mov(link.arp(arp, thres = 24 * 3600))
ph <- phase.summary(mov)
head(ph)
#>    ID n_phase  phase   arp  from    to               start                 end
#> 1 F02       1 Type 0  <NA>  <NA> arp_1 2024-03-01 11:56:18 2024-03-03 17:56:18
#> 2 F02       2    ARP arp_1  <NA>  <NA> 2024-03-03 18:56:18 2024-03-11 19:56:18
#> 3 F02       3 Type 2  <NA> arp_1 arp_2 2024-03-11 20:56:18 2024-03-13 17:56:18
#> 4 F02       4    ARP arp_2  <NA>  <NA> 2024-03-13 18:56:18 2024-03-16 17:56:18
#> 5 F02       5 Type 2  <NA> arp_2 arp_3 2024-03-16 18:56:18 2024-03-17 17:56:18
#> 6 F02       6    ARP arp_3  <NA>  <NA> 2024-03-17 18:56:18 2024-03-26 19:56:18
#>   duration_days n_locations distance_km displacement_km
#> 1     2.2500000          55   15.736831      14.1350608
#> 2     8.0416667         194   17.682587       0.9518428
#> 3     1.8750000          46   16.005406      12.5102060
#> 4     2.9583333          72    6.535382       0.2658206
#> 5     0.9583333          24   10.749695       9.4423885
#> 6     9.0416667         218   20.905722       0.6053980

# Total distance travelled outside ARPs per individual
tapply(ph$distance_km[ph$phase != "ARP"], ph$ID[ph$phase != "ARP"], sum)
#>      F02      F04      F05      M01      M03 
#> 63.20082 57.97788 84.48324 91.58864 61.15221 
```
