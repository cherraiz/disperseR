# Transfer classification data from disperseR object to a base dataset

Merges behavioral or movement phase classifications generated in a
disperseR object back into an external base data.frame using temporal or
spatial matching methods.

## Usage

``` r
transfer.disperseR(
  base.df,
  disperseR.obj,
  id.col = "name",
  time.col = "timestamp",
  method = "temporal",
  coord.names = c("x", "y"),
  max.diff = NULL
)
```

## Arguments

- base.df:

  A data.frame containing the target dataset where variables will be
  transferred.

- disperseR.obj:

  An object of class `disperseR` containing the analyzed data.

- id.col:

  Character. Column name in `base.df` representing individual
  identifiers.

- time.col:

  Character. Column name in `base.df` representing timestamps.

- method:

  Character. The matching method to use for transferring data. Choose
  one of:

  - `"temporal"`: Matches each row to the closest point in time.

  - `"spatial"`: Matches each row to the closest point in space using
    minimum Euclidean distance.

  - `"forward"`: Matches each row to the nearest future location in the
    disperseR object.

  - `"backward"`: Matches each row to the nearest past location in the
    disperseR object\].

  - `"movement"`: Uses temporal matching but applies a specific criteria
    for phase transitions. If the target point falls between two
    identical phases, it assigns that phase. If it falls exactly between
    a ARP (Area-Restricted Phase) and a movement phase, it prioritizes
    assigning the movement phase.

  - `"arp"`: Uses temporal matching but applies a specific criteria for
    phase transitions. If the target point falls between two identical
    phases, it assigns that phase. If it falls exactly between a ARP
    (Area-Restricted Phase) and a movement phase, it prioritizes
    assigning the ARP phase.

- coord.names:

  Character vector of length 2. Names of the X and Y coordinate columns
  in `base.df`, required if `method = "spatial"`.

- max.diff:

  Numeric. Optional. Maximum allowed time difference (in seconds) for a
  match to be considered valid.

## Value

A data.frame based on `base.df` with appended classification columns
from the `disperseR` object.

## Examples

``` r
eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
mov <- class.mov(link.arp(arp, thres = 24 * 3600))

# Transfer phases back to the original (irregular) fixes
out <- transfer.disperseR(sim_tracks, mov, id.col = "ID",
                          time.col = "timestamp", max.diff = 3600)
#> Data transferred for 5 individuals
table(out$ID, out$phase)
#>      
#>       ARP Type 0 Type 1 Type 2 Type 2 or Type 1 Type 3
#>   F02 566     48      0    124                0      0
#>   F04 580     47    119      0                0      0
#>   F05 143    146      0      0              121      0
#>   M01 766     55     61     93                0     80
#>   M03 323     51      0     67                0      0
```
